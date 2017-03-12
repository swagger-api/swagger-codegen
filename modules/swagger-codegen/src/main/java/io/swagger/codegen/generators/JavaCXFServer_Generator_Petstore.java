package io.swagger.codegen.generators;

import java.io.File;

import io.swagger.codegen.languages.JavaCXFServerCodegen;

/**
 * commandline of jaxrs-cxf-petstore-server.sh
 * 
 * # if you've executed sbt assembly previously it will use that instead. 
 * export JAVA_OPTS="${JAVA_OPTS} -XX:MaxPermSize=256M -Xmx1024M -DloggerPath=conf/log4j.properties"
 * ags="$@ generate -t modules/swagger-codegen/src/main/resources/JavaJaxRS/cxf -i modules/swagger-codegen/src/test/resources/2_0/petstore-with-fake-endpoints-models-for-testing.yaml -l jaxrs-cxf -o samples/server/petstore/jaxrs-cxf -DhideGenerationTimestamp=true"
 *
 * 
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
