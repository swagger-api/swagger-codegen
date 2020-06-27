package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;

import io.swagger.models.properties.BooleanProperty;

public class TypeScriptNodeClientCodegen extends AbstractTypeScriptClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(TypeScriptNodeClientCodegen.class);
    private static final SimpleDateFormat SNAPSHOT_SUFFIX_FORMAT = new SimpleDateFormat("yyyyMMddHHmm");

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String SNAPSHOT = "snapshot";

    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String npmRepository = null;
    protected String apiSuffix = "Api";

    public TypeScriptNodeClientCodegen() {
        super();

        typeMapping.put("file", "Buffer");
        languageSpecificPrimitives.add("Buffer");

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        outputFolder = "generated-code/typescript-node";
        embeddedTemplateDir = templateDir = "typescript-node";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api-single.mustache", ".ts");
        modelPackage = "model";
        apiPackage = "api";

        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package"));
        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(SNAPSHOT, "When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm", BooleanProperty.TYPE).defaultValue(Boolean.FALSE.toString()));
    }


    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "models.ts"));
        supportingFiles.add(new SupportingFile("api-all.mustache", apiPackage().replace('.', File.separatorChar), "apis.ts"));
        supportingFiles.add(new SupportingFile("api.mustache", getIndexDirectory(), "api.ts"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));

        if(additionalProperties.containsKey(NPM_NAME)) {
            addNpmPackageGeneration();
        }
    }

    private void addNpmPackageGeneration() {
        if(additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        }

        if (additionalProperties.containsKey(SNAPSHOT) && Boolean.valueOf(additionalProperties.get(SNAPSHOT).toString())) {
            this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
        }
        additionalProperties.put(NPM_VERSION, npmVersion);

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        //Files for building our lib
        supportingFiles.add(new SupportingFile("package.mustache", getPackageRootDirectory(), "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", getPackageRootDirectory(), "tsconfig.json"));
    }

    private String getPackageRootDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    private String getIndexDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    private String getApiFilenameFromClassname(String classname) {
        String name = classname.substring(0, classname.length() - apiSuffix.length());
        return toApiFilename(name);
    }

    private String getModelNameFromModelFilename(String filename) {
        String name = filename.substring((modelPackage() + File.separator).length());
        return camelize(name);
    }

    @Override
    public String getName() {
        return "typescript-node";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript nodejs client library.";
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

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default" + apiSuffix;
        }
        return initialCaps(name) + apiSuffix;
    }

    @Override
    public String toApiFilename(String name) {
        if (name.length() == 0) {
            return "default" + apiSuffix;
        }
        return camelize(name, true) + apiSuffix;
    }

    @Override
    public String toApiImport(String name) {
        return apiPackage() + "/" + toApiFilename(name);
    }

    @Override
    public String toModelFilename(String name) {
        return camelize(toModelName(name), true);
    }

    @Override
    public String toModelImport(String name) {
        return modelPackage() + "/" + toModelFilename(name);
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                mo.put("tsImports", toTsImports(cm, cm.imports));
            }
        }
        return result;
    }

    private List<Map<String, String>> toTsImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> tsImport = new HashMap<>();
                tsImport.put("classname", im);
                tsImport.put("filename", toModelFilename(im));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");

        Map<String, CodegenSecurity> authMethodsMap = new HashMap<>();
        for (CodegenOperation op : (List<CodegenOperation>) objs.get("operation")) {
            if(op.hasAuthMethods){
                for(CodegenSecurity sec : op.authMethods){
                    authMethodsMap.put(sec.name, sec);
                }
            }
        }

        if (!authMethodsMap.isEmpty()) {
            operations.put("authMethods", authMethodsMap.values());
            operations.put("hasAuthMethods", true);
        }

        objs.put("apiFilename", getApiFilenameFromClassname(objs.get("classname").toString()));

        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("filename", im.get("import"));
            im.put("classname", getModelNameFromModelFilename(im.get("filename").toString()));
        }

        return operations;
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
