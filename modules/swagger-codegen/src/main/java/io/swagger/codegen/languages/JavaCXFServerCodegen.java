
package io.swagger.codegen.languages;

import java.io.File;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;

import org.slf4j.LoggerFactory;


import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.Operation;

/**
 * TODO #2017:
 * - api_test.mustache: add switch for using gzip in test cases?
 * 
 * 
 *
 */
public class JavaCXFServerCodegen extends AbstractJavaJAXRSServerCodegen
{   
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaCXFServerCodegen.class);
    
    public static final String USE_BEANVALIDATION = "useBeanValidation";
    
    public static final String GENERATE_SPRING_APPLICATION = "generateSpringApplication";

    protected boolean useBeanValidation = false;
    
    protected boolean generateSpringApplication = false;
    
    
    public JavaCXFServerCodegen()
    {
        super();
        apiTestTemplateFiles.clear(); // TODO: add test template

        supportsInheritance = true;
        
        artifactId = "swagger-cxf-server";
        
        sourceFolder = "gen" + File.separator + "java";
        outputFolder = "generated-code/JavaJaxRS-CXF";
        
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        
        // clear model and api doc template as this codegen
        // does not support auto-generated markdown doc at the moment
        //TODO: add doc templates
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");


        typeMapping.put("date", "LocalDate");
        typeMapping.put("DateTime", "javax.xml.datatype.XMLGregorianCalendar"); // Map DateTime fields to Java standart class 'XMLGregorianCalendar'

        importMapping.put("LocalDate", "org.joda.time.LocalDate");

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf";

        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));
        cliOptions.add(CliOption.newBoolean(GENERATE_SPRING_APPLICATION, "Generate Spring application"));
        
    }


    @Override
    public void processOpts()
    {
        super.processOpts();
        
        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
              boolean useBeanValidationProp = Boolean.valueOf(additionalProperties.get(USE_BEANVALIDATION).toString());
              this.setUseBeanValidation(useBeanValidationProp);
           
           // write back as boolean
           additionalProperties.put(USE_BEANVALIDATION, useBeanValidationProp);
        }
        
        if (additionalProperties.containsKey(GENERATE_SPRING_APPLICATION)) {
            boolean generateSpringApplicationProp = Boolean.valueOf(additionalProperties.get(GENERATE_SPRING_APPLICATION).toString());
            this.setGenerateSpringApplication(generateSpringApplicationProp);
         
            // write back as boolean
            additionalProperties.put(GENERATE_SPRING_APPLICATION, generateSpringApplicationProp);
        }
        
       
        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        
        if (this.generateSpringApplication) {
        	writeOptional(outputFolder, new SupportingFile("readme.md", "", "readme.md"));
            
            writeOptional(outputFolder, new SupportingFile("web.mustache",
                    ("src/main/webapp/WEB-INF"), "web.xml"));
            writeOptional(outputFolder, new SupportingFile("context.xml.mustache",
                    ("src/main/webapp/WEB-INF"), "context.xml"));
            writeOptional(outputFolder, new SupportingFile("jboss-web.xml.mustache",
                    ("src/main/webapp/WEB-INF"), "jboss-web.xml"));
        }
        
        
    } 
    
    @Override
    public String getName()
    {
        return "jaxrs-cxf";
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);        
        co.subresourceOperation = !co.path.isEmpty();
    }
    
    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        model.imports.remove("ApiModelProperty");
        model.imports.remove("ApiModel");
        model.imports.remove("JsonSerialize");
        model.imports.remove("ToStringSerializer");
    }
    
    @Override
    public String getHelp()
    {
        return "Generates a Java JAXRS Server application based on Apache CXF framework.";
    }
    
    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }
    
    public void setGenerateSpringApplication(boolean generateSpringApplication) {
        this.generateSpringApplication = generateSpringApplication;
    }
}
