package io.swagger.codegen.generators;

import java.io.File;
import java.io.IOException;

import org.junit.rules.TemporaryFolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

/**
 * commandline of jaxrs-spec-petstore-server.sh
 * 
 * # if you've executed sbt assembly previously it will use that instead. export
 * JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M
 * -DloggerPath=conf/log4j.properties" ags="$@ generate -t
 * modules/swagger-codegen/src/main/resources/JavaJaxRS -i
 * modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml
 * -l jaxrs-spec -o samples/server/petstore/jaxrs-spec
 * -DhideGenerationTimestamp=true"
 *
 * 
 */
public class JaxrsSpec_Generator_Petstore {

    private static final Logger LOGGER = LoggerFactory.getLogger(JaxrsSpec_Generator_Petstore.class);

    /**
     * by default generate to samples folder
     * @param args
     */
    public static void main(String[] args) {
    	File targetFolder = new File("../../samples/server/petstore/jaxrs-spec");
    	generateToFolder(targetFolder);
    }
    
    public static void generateToFolder(File output) {
    	final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml");

        CodegenConfig codegenConfig = new JavaJAXRSSpecServerCodegen();
        codegenConfig.additionalProperties().put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, true);

        codegenConfig.setOutputDir(output.getAbsolutePath());

        ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

        DefaultGenerator gen = new DefaultGenerator();
        gen.opts(clientOptInput);

        gen.generate();

    }
    
    /**
     * for unittests generate to temporary folder
     * @return
     */
    public static TemporaryFolder generateToTemporaryFolder() {
        try {
            TemporaryFolder folder = new TemporaryFolder();
            folder.create();
            File output = folder.getRoot();
            generateToFolder(output);
            
            return folder;

        } catch (IOException e) {
            LOGGER.info("unable to create temporary folder");
            e.printStackTrace();
            return null;
        }

    }

}
