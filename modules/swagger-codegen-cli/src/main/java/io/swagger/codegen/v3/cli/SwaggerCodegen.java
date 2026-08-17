package io.swagger.codegen.v3.cli;

import io.swagger.codegen.v3.cli.cmd.Generate;
import io.swagger.codegen.v3.CodegenArgument;
import io.swagger.codegen.v3.CodegenConfig;
import io.swagger.codegen.v3.CodegenConfigLoader;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.OpenAPIV3Parser;
import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.inf.Argument;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.helper.HelpScreenException;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.Subparser;
import net.sourceforge.argparse4j.inf.Subparsers;
import net.sourceforge.argparse4j.inf.Namespace;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.Arrays;

/**
 * User: lanwen Date: 24.03.15 Time: 17:56
 * <p>
 * Command line interface for swagger codegen use `swagger-codegen-cli.jar help` for more info
 *
 * @since 2.1.3-M1
 */
public class SwaggerCodegen {


    private static Logger LOGGER = LoggerFactory.getLogger(SwaggerCodegen.class);
    private static String GENERATE_COMMAND_NAME = "Generate";

    public static void main(String[] args) {
        final String oas3 = CLIHelper.loadResourceOAS3File();
        if(StringUtils.isBlank(oas3)) {
            LOGGER.error("Could not load resource file.");
            System.exit(1);
        }
        final OpenAPI openAPI = new OpenAPIV3Parser().readContents(oas3, null, null).getOpenAPI();
        final Map<String, Schema> schemaMap = openAPI.getComponents().getSchemas();
        final Set<String> schemaNames = schemaMap.keySet();

        final ArgumentParser codegenParser = ArgumentParsers.newFor("swagger-codegen")
            .singleMetavar(true).build()
            .defaultHelp(true)
            .epilog("Command names and long options may be abbreviated as long as they remain unambiguous. Try 'swagger-codegen COMMAND --help' for options specific to COMMAND.");

        final Subparsers subparsers = codegenParser.addSubparsers()
            .title("commands")
            .help("additional help")
            .metavar("COMMAND")
            .dest("COMMAND");

        final Map<String, Schema> commandMap = new HashMap<>();
        List<CodegenArgument> codegenArguments = null;
        final Map<String, Subparser> parserMap = new HashMap<>();

        for(String schemaName : schemaNames) {
            final Schema schema = schemaMap.get(schemaName);
            final String command = CLIHelper.getCommand(schemaName, schema);
            final String description = CLIHelper.getDescription(schemaName, schema);
            final Map<String, Schema> schemaProperties = schema.getProperties();
            final Subparser parser = subparsers.addParser(command).help(description);

            commandMap.put(command, schema);
            parserMap.put(command, parser);

            if(schemaProperties == null || schemaProperties.isEmpty()) {
                LOGGER.debug(String.format("there are not options for command '%s'", command));
                continue;
            }

            final List<String> schemaRequired = schema.getRequired();

            for (String propertyName : schemaProperties.keySet()) {
                final Schema property = schemaProperties.get(propertyName);
                final Map<String, Object> extensions = property.getExtensions();
                if(!CLIHelper.containsOptionExtensions(extensions)) {
                    LOGGER.warn(String.format("there are not option extensions for property '%s?", propertyName));
                    continue;
                }
                String[] arguments = CLIHelper.getArguments(extensions);
                final Argument argument = parser.addArgument(arguments)
                        .type(CLIHelper.getClass(property))
                        .help(property.getDescription());

                if(schemaRequired != null && schemaRequired.contains(propertyName)) {
                    argument.required(true);
                    LOGGER.debug(String.format("property: %s (required)", propertyName));
                }

                if(property instanceof BooleanSchema) {
                    argument.action(Arguments.storeTrue());
                    LOGGER.debug(String.format("property: %s (boolean)", propertyName));
                } else if(property instanceof ArraySchema) {
                    argument.action(Arguments.append());
                    LOGGER.debug(String.format("property: %s (array)", propertyName));
                }
            }
        }
        for (Map.Entry<String, Subparser> entry : parserMap.entrySet()) {
            final String command = entry.getKey();
            final Subparser parser = entry.getValue();
            LOGGER.debug(String.format("command: %s", command));

            if (command.equalsIgnoreCase(GENERATE_COMMAND_NAME)) {
                Namespace res = null;
                try {
                    res = codegenParser
                        // filter out the help options as they cause
                        // the help message to be printed
                        // HelpScreenException to be thrown
                        .parseKnownArgs(Arrays
                                        .stream(args)
                                        .filter(a -> !a.startsWith("-h") &&
                                                !("--help".startsWith(a) && a.length() > 2)
                                                )
                                        .toArray(String[]::new), null);
                    LOGGER.debug(String.format("res: %s", res));
		} catch (ArgumentParserException e) {
                    LOGGER.debug(e.toString());
		}
                String language = (res != null ? res.get("lang") : null);
                if (StringUtils.isNotBlank(language)) {
                    LOGGER.debug(String.format("language: %s", language));
                    CodegenConfig config = CodegenConfigLoader.forName(language);
                    codegenArguments = config.readLanguageArguments();
                    if (codegenArguments != null && !codegenArguments.isEmpty()) {
                        for (CodegenArgument codegenArgument : codegenArguments) {
                            String[] arguments = CLIHelper.getArguments(codegenArgument);
                            Class clazz = "boolean".equalsIgnoreCase(codegenArgument.getType()) ? Boolean.class : String.class;
                            final Argument argument = parser.addArgument(arguments)
                                    .type(clazz)
                                    .help(codegenArgument.getDescription());
                            if (codegenArgument.getType().equalsIgnoreCase("boolean")) {
                                argument.action(Arguments.storeTrue());
                            } else if(codegenArgument.getArray() != null && codegenArgument.getArray()) {
                                argument.action(Arguments.append());
                            }
                        }
                    }
                }
            }

        }
        final Map<String, Object> inputArgs = new HashMap<>();
        try {
            codegenParser.parseArgs(args, inputArgs);
            LOGGER.debug(String.format("inputArgs: %s", inputArgs));
        } catch (HelpScreenException e) {
            // thrown when help option is invoked
            codegenParser.handleError(e);
            System.exit(0);
        } catch (ArgumentParserException e) {
            codegenParser.handleError(e);
            System.exit(1);
        }
        final String userInputCommand = (String)inputArgs.get("COMMAND");
        final Schema commandSchema = commandMap.get(userInputCommand);
        final Map<String, Object> extensions = commandSchema.getExtensions();
        if(extensions == null || extensions.isEmpty() || extensions.get("x-class-name") == null) {
            LOGGER.error("Extensions are required to run command. I.e.: 'x-class-name'");
            System.exit(1);
        }
        final String className = extensions.get("x-class-name").toString();
        try {
            final Class clazz = Class.forName(className);
            final Object commandObject = clazz.newInstance();
            final Map<String, Object> optionValueMap = CLIHelper.createOptionValueMap(commandSchema, inputArgs);

            BeanUtils.populate(commandObject, optionValueMap);

            if (codegenArguments != null && !codegenArguments.isEmpty() && commandObject instanceof Generate) {
                codegenArguments = codegenArguments.stream()
                        .filter(codegenArgument -> {
                            final String option = CLIHelper.fixOptionName(codegenArgument.getOption());
                            final String optionValue = String.valueOf(inputArgs.get(option));

                            if (StringUtils.isNotBlank(optionValue) && !"null".equalsIgnoreCase(optionValue)) {
                                codegenArgument.setValue(optionValue);
                                return true;
                            } else {
                                return false;
                            }
                        })
                        .collect(Collectors.toList());

                Generate generateCommand = (Generate) commandObject;
                generateCommand.setCodegenArguments(codegenArguments);
            }

            if(commandObject instanceof Runnable) {
                ((Runnable) commandObject).run();
            }

        } catch (ClassNotFoundException | IllegalAccessException | InstantiationException | InvocationTargetException ex) {
            LOGGER.error(String.format("Could not load class '%s' for command '%s'", className, userInputCommand), ex);
            System.exit(1);
        }
    }
}
