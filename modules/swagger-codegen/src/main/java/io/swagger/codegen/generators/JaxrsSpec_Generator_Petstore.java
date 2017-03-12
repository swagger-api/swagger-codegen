package io.swagger.codegen.generators;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen;

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
public class JaxrsSpec_Generator_Petstore extends AbstractGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(JaxrsSpec_Generator_Petstore.class);

    public JaxrsSpec_Generator_Petstore() {
		super();
		this.codegenConfig = new JavaJAXRSSpecServerCodegen();
		this.samplesFolder = "../../samples/server/petstore/jaxrs-spec";
		this.contractPath = "src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml";
				
	}
	
    public static void main(String[] args) {
    	JaxrsSpec_Generator_Petstore generator = new JaxrsSpec_Generator_Petstore();
    	File targetFolder = new File(generator.samplesFolder);
    	generator.generateToFolder(targetFolder);
    }
}
