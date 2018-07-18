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
import net.sourceforge.argparse4j.inf.Subparser;
import net.sourceforge.argparse4j.inf.Subparsers;
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
            return;
        }
        final OpenAPI openAPI = new OpenAPIV3Parser().readContents(oas3, null, null).getOpenAPI();
        final Map<String, Schema> schemaMap = openAPI.getComponents().getSchemas();
        final Set<String> schemaNames = schemaMap.keySet();

        final ArgumentParser codegenParser = ArgumentParsers.newFor("swagger-codegen").build();
        final Subparsers subparsers = codegenParser.addSubparsers()
                .title("commands")
                .help("additional help")
                .metavar("Command");

        final Map<String, Schema> commandMap = new HashMap<>();
        List<CodegenArgument> codegenArguments = null;

        for(String schemaName : schemaNames) {
            final Schema schema = schemaMap.get(schemaName);
            final String command = CLIHelper.getCommand(schemaName, schema);
            final Map<String, Schema> schemaProperties = schema.getProperties();
            final Subparser parser = subparsers.addParser(command).help(command);

            commandMap.put(command, schema);

            if(schemaProperties == null || schemaProperties.isEmpty()) {
                LOGGER.debug(String.format("there are not options for command '%s'", command));
                continue;
            }
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
                        .help(property.getDescription())
                        .metavar(StringUtils.EMPTY);

                if(property instanceof BooleanSchema) {
                    argument.nargs("?").setConst(true);
                } else if(property instanceof ArraySchema) {
                    argument.nargs("*");
                }
            }
            if (command.equalsIgnoreCase(GENERATE_COMMAND_NAME)) {
                String language = CLIHelper.detectlanguage(args);
                if (StringUtils.isNotBlank(language)) {
                    CodegenConfig config = CodegenConfigLoader.forName(language);
                    codegenArguments = config.readLanguageArguments();
                    if (codegenArguments != null && !codegenArguments.isEmpty()) {
                        for (CodegenArgument codegenArgument : codegenArguments) {
                            String[] arguments = CLIHelper.getArguments(codegenArgument);
                            Class clazz = "boolean".equalsIgnoreCase(codegenArgument.getType()) ? Boolean.class : String.class;
                            final Argument argument = parser.addArgument(arguments)
                                    .type(clazz)
                                    .help(codegenArgument.getDescription())
                                    .metavar(StringUtils.EMPTY);
                            if (codegenArgument.getType().equalsIgnoreCase("boolean")) {
                                argument.nargs("?").setConst(true);
                            } else if(codegenArgument.getArray() != null && codegenArgument.getArray()) {
                                argument.nargs("*");
                            }
                        }
                    }
                }
            }
        }
        final Map<String, Object> inputArgs = new HashMap<>();
        try {
            codegenParser.parseArgs(args, inputArgs);
        } catch (ArgumentParserException e) {
            codegenParser.handleError(e);
            return;
        }
        final String userInputCommand = CLIHelper.detectCommand(args);
        if(userInputCommand == null) {
            LOGGER.error("No command found.");
            return;
        }
        final Schema commandSchema = commandMap.get(userInputCommand);
        if(commandSchema == null) {
            LOGGER.error(String.format("There are not schema related to command '%s'", userInputCommand));
            return;
        }
        final Map<String, Object> extensions = commandSchema.getExtensions();
        if(extensions == null || extensions.isEmpty() || extensions.get("x-class-name") == null) {
            LOGGER.error("Extensions are required to run command. i.e: 'x-class-name'");
            return;
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
                new Thread(((Runnable) commandObject)).start();
            }

        } catch (ClassNotFoundException | IllegalAccessException | InstantiationException | InvocationTargetException ex) {
            LOGGER.error(String.format("Could not load class '%s' for command '%s'", className, userInputCommand), ex);
        }
    }
}
