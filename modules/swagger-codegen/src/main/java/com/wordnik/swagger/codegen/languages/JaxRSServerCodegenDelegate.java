package com.wordnik.swagger.codegen.languages;

import java.io.File;

import com.wordnik.swagger.codegen.SupportingFile;

public class JaxRSServerCodegenDelegate extends JaxRSServerCodegen {
	@Override
	public String getName() {
		return "jaxrsDelegate";
	}
	protected String getArgApiPackage(){
		return "gen.api";
	}
	protected String getArgModelPackage(){
		return "gen.model";
	}
	private String resourcePath=null;
	protected String getArgResourcePath(){
		return resourcePath;
	}



	public JaxRSServerCodegenDelegate() {
		super();
		String resourcePath=getArgResourcePath();
		if(resourcePath!=null){
			apiPackage = getArgApiPackage() +"."+ resourcePath;
			modelPackage = getArgModelPackage() +"."+ resourcePath;
		}
		else{
			apiPackage = getArgApiPackage() ;
			modelPackage = getArgModelPackage() ;
		}
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
/*
	public void init(CommandLine cmd) {
		super.init(cmd);
		 resourcePath= getResourcePathFromFile(cmd.getOptionValue("i"));
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
*/
}
