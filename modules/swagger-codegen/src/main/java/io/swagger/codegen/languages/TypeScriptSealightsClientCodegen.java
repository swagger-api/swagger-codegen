package io.swagger.codegen.languages;

import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.BooleanProperty;

public class TypeScriptSealightsClientCodegen extends AbstractTypeScriptClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(TypeScriptSealightsClientCodegen.class);
    private static final SimpleDateFormat SNAPSHOT_SUFFIX_FORMAT = new SimpleDateFormat("yyyyMMddHHmm");

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String SNAPSHOT = "snapshot";

    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String npmRepository = null;

    public TypeScriptSealightsClientCodegen() {
        super();

        typeMapping.put("file", "Buffer");

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-sealights";
        embeddedTemplateDir = templateDir = "typescript-sealights";

        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package"));
        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(SNAPSHOT, "When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm", BooleanProperty.TYPE).defaultValue(Boolean.FALSE.toString()));
    }


    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("api.mustache", null, "api.ts"));
    }

    private String getPackageRootDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    @Override
    public String getName() {
        return "typescript-sealights";
    }

    @Override
    public String getHelp() {
        return "Generates a Sealights TypeScript nodejs client library.";
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals("Buffer");
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof FileProperty) {
            return "Buffer";
        }
        return super.getTypeDeclaration(p);
    }


    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }
}
