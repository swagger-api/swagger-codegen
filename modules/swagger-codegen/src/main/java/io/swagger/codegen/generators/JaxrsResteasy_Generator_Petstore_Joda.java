package io.swagger.codegen.generators;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;

/**
 * commandline of jaxrs-resteasy-joda-petstore-server.sh.sh
 * 
 */
public class JaxrsResteasy_Generator_Petstore_Joda extends AbstractGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(JaxrsResteasy_Generator_Petstore_Joda.class);

    public JaxrsResteasy_Generator_Petstore_Joda() {
		super();
		this.codegenConfig = new JavaResteasyServerCodegen();
		
		this.configFile = "../../bin/jaxrs-resteasy-joda-petstore-server.json";
				
		this.samplesFolder = "../../samples/server/petstore/jaxrs-resteasy/joda";
		this.contractPath = "src/test/resources/2_0/petstore.yaml";
				
	}
	
    public static void main(String[] args) {
    	JaxrsResteasy_Generator_Petstore_Joda generator = new JaxrsResteasy_Generator_Petstore_Joda();
    	File targetFolder = new File(generator.samplesFolder);
    	generator.generateToFolder(targetFolder);
    }
}
