package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaCXFServerCodegen;

public class JavaCXFServerOptionsProvider extends JavaOptionsProvider {
	
	public static final String GENERATE_SPRING_APPLICATION = "false";
	
	public static final String IMPL_FOLDER_VALUE = "src/main/java";
	
    @Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs-cxf";
    }

    @Override
    public Map<String, String> createOptions() {
    	
    	 Map<String, String> parentOptions = super.createOptions();
    	
    	ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>()
                .putAll(parentOptions);
        
    	builder.put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE);
        builder.put("title", "Test title");

        builder.put(JavaCXFServerCodegen.USE_BEANVALIDATION, "false");
        builder.put(JavaCXFServerCodegen.GENERATE_SPRING_APPLICATION, "false");
        
        return builder.build();
        
    }
}
