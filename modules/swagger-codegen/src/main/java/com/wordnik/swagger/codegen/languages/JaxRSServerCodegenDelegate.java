package com.wordnik.swagger.codegen.languages;

import java.io.File;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.wordnik.swagger.codegen.Codegen;
import com.wordnik.swagger.codegen.CodegenConfig;
import com.wordnik.swagger.codegen.SupportingFile;
import com.wordnik.swagger.codegen.languages.JaxRSServerCodegen;

public class JaxRSServerCodegenDelegate extends JaxRSServerCodegen {
	
	static String argApiPackage="gen.api";
	static String argModelPackage="gen.model";
	
	public String getName() {
		return "jaxrsDelegate";
	}

	public JaxRSServerCodegenDelegate() {
		super();
	}

	public void init(CommandLine cmd) {
		super.init(cmd);
		String jsonFileName = null;
		jsonFileName = cmd.getOptionValue("i");

		String resourcePath = getResourcePathFromFile(jsonFileName);
		if (resourcePath == null) {
			resourcePath = "";
		} else if (resourcePath.length() > 0) {
			resourcePath = resourcePath.replace('"', ' ');
			resourcePath = resourcePath.trim();
			if (resourcePath.charAt(0) == '/') {
				resourcePath = resourcePath.substring(1);
			}

			resourcePath = resourcePath.replace('/', '.');
			resourcePath = "." + resourcePath;
		}
		apiPackage = argApiPackage + resourcePath;
		modelPackage = argModelPackage + resourcePath;
		templateDir = "JavaJaxRSDelegate";
		apiTemplateFiles.clear();
		apiTemplateFiles.put("api.mustache", ".java");
		apiTemplateFiles.put("api.interface.mustache", "Interface.java");
		apiTemplateFiles.put("api.stub.mustache", "STUB.java");
//		apiTemplateFiles.put("api.stub.mustache", "STUB.java");
		
		
		
	    supportingFiles.clear();
	    supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
	    supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
	    supportingFiles.add(new SupportingFile("ApiException.mustache", (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiException.java"));
	    supportingFiles.add(new SupportingFile("ApiOriginFilter.mustache", 
	      (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiOriginFilter.java"));
	    supportingFiles.add(new SupportingFile("ApiResponseMessage.mustache", 
	      (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "ApiResponseMessage.java"));
	    supportingFiles.add(new SupportingFile("NotFoundException.mustache", 
	      (sourceFolder + File.separator + apiPackage).replace(".", java.io.File.separator), "NotFoundException.java"));
	    supportingFiles.add(new SupportingFile("web.mustache", 
	      ("src/main/webapp/WEB-INF"), "web.xml"));

	    
	    
	    
	}

	private static String getResourcePathFromFile(String jsonFileName) {
		if (jsonFileName == null)
			return null;
		ObjectMapper mapper = new ObjectMapper();
		try {

			JsonNode rootNode = mapper.readValue(new File(jsonFileName),
					JsonNode.class); // src can be a File, URL, InputStream etc
			if(rootNode.get("resourcePath")!=null)
				return rootNode.get("resourcePath").toString();
			if(rootNode.get("info")!=null){
				JsonNode x=rootNode.get("info").get("title");
				if(x!=null){
					return x.toString().trim().replaceAll(" ", "");
				}
			}
			return "generic";

		} catch (Throwable e) {

			e.printStackTrace();

		}

		return null;
	}

}
