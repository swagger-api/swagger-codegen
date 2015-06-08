using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;



namespace io.swagger.Model {
  public class ApiResponse {
    

    
    public int? Code { get; set; }

    

    
    public string Type { get; set; }

    

    
    public string Message { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class ApiResponse {\n");
      
      sb.Append("  Code: ").Append(Code).Append(Environment.NewLine);
      
      sb.Append("  Type: ").Append(Type).Append(Environment.NewLine);
      
      sb.Append("  Message: ").Append(Message).Append(Environment.NewLine);
      
      sb.Append("}");
      sb.Append(Environment.NewLine);
      return sb.ToString();
    }
  }
  
  
}