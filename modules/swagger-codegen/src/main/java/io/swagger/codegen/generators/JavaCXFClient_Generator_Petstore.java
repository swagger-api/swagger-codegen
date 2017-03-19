package io.swagger.codegen.generators;

import java.io.File;

import io.swagger.codegen.languages.JavaCXFClientCodegen;

/**
 * commandline invocation for jaxrs-cxf client petstore
 * 
 */
public class JavaCXFClient_Generator_Petstore extends AbstractGenerator {

	public JavaCXFClient_Generator_Petstore() {
		super();
		this.codegenConfig = new JavaCXFClientCodegen();
		this.samplesFolder = "../../samples/client/petstore/jaxrs-cxf";
		this.contractPath = "src/test/resources/2_0/petstore.yaml";
				
	}
	
	public static void main(String[] args) {
		JavaCXFClient_Generator_Petstore generator = new JavaCXFClient_Generator_Petstore();
    	File targetFolder = new File(generator.samplesFolder);
    	generator.generateToFolder(targetFolder);
    }
    

}
