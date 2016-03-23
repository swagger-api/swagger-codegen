package io.cellstore.codegen;

import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;

import java.util.ArrayList;
import java.util.List;

public class CellStoreCodegenOperation extends CodegenOperation {
    public List<CodegenParameter> patternQueryParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> hardcodedQueryParams = new ArrayList<CodegenParameter>();
    
    public boolean includeOperation()
    {
      if (vendorExtensions.size() > 0)
      {
        Object excludeFromBindings = vendorExtensions.get("x-exclude-from-bindings");
        if (excludeFromBindings != null)
        {
          if (excludeFromBindings instanceof Boolean)
          {
            if (((Boolean)excludeFromBindings).booleanValue())
              return false;
          }
          else
          {
            String msg = "Invalid value for x-exclude-from-bindings, only booleans are allowed\n";      
            throw new RuntimeException(msg);
          }
        }
      }
      return true;
    }
    
}
