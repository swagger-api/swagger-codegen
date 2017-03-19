package io.swagger.codegen.generators;

import java.io.File;

import io.swagger.codegen.languages.JavaCXFServerCodegen;

/**
 * commandline of jaxrs-cxf-petstore-server.sh
 */
public class JavaCXFServer_Generator_Petstore extends AbstractGenerator {

	public JavaCXFServer_Generator_Petstore() {
		super();
		this.codegenConfig = new JavaCXFServerCodegen();
		this.samplesFolder = "../../samples/server/petstore/jaxrs-cxf";
		this.contractPath = "src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml";
				
	}
	
   public static void main(String[] args) {
    	JavaCXFServer_Generator_Petstore generator = new JavaCXFServer_Generator_Petstore();
    	File targetFolder = new File(generator.samplesFolder);
    	generator.generateToFolder(targetFolder);
    }
    

}
