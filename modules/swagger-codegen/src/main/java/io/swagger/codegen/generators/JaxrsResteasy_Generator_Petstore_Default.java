package io.swagger.codegen.generators;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;

/**
 * commandline of jaxrs-resteasy-petstore-server.sh
 * 
 */
public class JaxrsResteasy_Generator_Petstore_Default extends AbstractGenerator {

    private static final Logger LOGGER = LoggerFactory.getLogger(JaxrsResteasy_Generator_Petstore_Default.class);

    public JaxrsResteasy_Generator_Petstore_Default() {
		super();
		this.codegenConfig = new JavaResteasyServerCodegen();
		this.samplesFolder = "../../samples/server/petstore/jaxrs-resteasy/default";
		this.contractPath = "src/test/resources/2_0/petstore.yaml";
				
	}
	
    public static void main(String[] args) {
    	JaxrsResteasy_Generator_Petstore_Default generator = new JaxrsResteasy_Generator_Petstore_Default();
    	File targetFolder = new File(generator.samplesFolder);
    	generator.generateToFolder(targetFolder);
    }
}
