package io.cellstore.codegen;

import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.parameters.Parameter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

public class CellStoreCodegenParameter extends CodegenParameter {
    public Boolean isPatternParam;
    public String pattern, patternSuffix;
    public String conversion;
    
    /**
     * Determines whether this parameter is mandatory. If the parameter is in "path",
     * this property is required and its value MUST be true. Otherwise, the property
     * MAY be included and its default value is false.
     */
    public Boolean required;
    
    public enum Kind 
    {
    	NORMAL,
    	PATTERN,
    	HARDCODED
    }    
    
    public Kind getParameterKind()
    {
      if (vendorExtensions.size() > 0)
      {
        Object bindingName = vendorExtensions.get("x-name-pattern");
        Object hardcodedValue = vendorExtensions.get("x-binding-value");
        if (bindingName != null && hardcodedValue != null)
          throw new RuntimeException("x-name-pattern and x-binding-value are not allowed on the same parameter");

        if (bindingName == null && hardcodedValue == null)
          return Kind.NORMAL;
        
        if (bindingName != null)
           return Kind.PATTERN;
        else
          return Kind.HARDCODED;
      }
      return Kind.NORMAL;
    }

    public void setParamName(DefaultCodegen codegen, Parameter param){
      if (vendorExtensions.size() > 0)
      {
        Object bindingName = vendorExtensions.get("x-binding-name");
        if (bindingName != null)
        {
          if (bindingName instanceof String)
          {
            this.paramName = codegen.toParamName((String)bindingName);
            return;
          }
          else
          {
            String msg = "Invalid value for x-binding-name, only strings are allowed\n";      
            throw new RuntimeException(msg);
          }
        }
      }
      this.paramName = codegen.toParamName(param.getName());
    }
    
    public void setDescription(DefaultCodegen codegen, Parameter param){
      if (vendorExtensions.size() > 0)
      {
        Object bindingDescription = vendorExtensions.get("x-binding-description");
        if (bindingDescription != null)
        {
          if (bindingDescription instanceof String)
          {
            this.description = codegen.escapeText((String)bindingDescription);
            return;
          }
          else
          {
            String msg = "Invalid value for x-binding-description, only strings are allowed\n";      
            throw new RuntimeException(msg);
          }
        }
      }
      this.description = codegen.escapeText(param.getDescription());
    }
    
    public String toString() {
      String output = "Parameter " + this.paramName + ":\n";;
      output += "  isFile: " + this.isFile + "\n";
      output += "  notFile: " + this.notFile + "\n";
      output += "  hasMore: " + this.hasMore + "\n";
      output += "  isContainer: " + this.isContainer + "\n";
      output += "  secondaryParam: " + this.secondaryParam + "\n";
      output += "  baseName: " + this.baseName + "\n";
      output += "  paramName: " + this.paramName + "\n";
      output += "  dataType: " + this.dataType + "\n";
      output += "  collectionFormat: " + this.collectionFormat + "\n";
      output += "  description: " + this.description + "\n";
      output += "  baseType: " + this.baseType + "\n";
      output += "  isFormParam: " + this.isFormParam + "\n";
      output += "  isQueryParam: " + this.isQueryParam + "\n";
      output += "  isPathParam: " + this.isPathParam + "\n";
      output += "  isHeaderParam: " + this.isHeaderParam + "\n";
      output += "  isCookieParam: " + this.isCookieParam + "\n";
      output += "  isBodyParam: " + this.isBodyParam + "\n";
      output += "  isPatternParam: " + this.isPatternParam + "\n";
      output += "  required: " + this.required + "\n";
      output += "  jsonSchema: " + this.jsonSchema + "\n";
      output += "  defaultValue: " + this.defaultValue + "\n";
      output += "  patternSuffix: " + this.patternSuffix + "\n";
      output += "  pattern: " + this.pattern + "\n";
      output += "  isEnum: " + this.isEnum + "\n";
      if (this._enum != null) {
          output += "  _enum: " + this._enum.toString() + "\n";
      }
      if (this.allowableValues != null) {
          output += "  allowableValues: " + this.allowableValues.toString() + "\n";
      }
      output += "  vendorExtensions: " + this.vendorExtensions.toString() + "\n";
      output += "  conversion: " + this.conversion + "\n";
      output += "  parameterKind: " + getParameterKind() + "\n";
      return output;
    }
    
    public CellStoreCodegenParameter copy() {
        CellStoreCodegenParameter output = new CellStoreCodegenParameter();
        super.copy(output);
        output.isPatternParam = this.isPatternParam;
        output.patternSuffix = this.patternSuffix;
        output.pattern = this.pattern;
        output.conversion = this.conversion;
        return output;
    }
}
