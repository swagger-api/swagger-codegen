using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;



namespace io.swagger.Model {
  public class Category {
    

    
    public long? Id { get; set; }

    

    
    public string Name { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Category {\n");
      
      sb.Append("  Id: ").Append(Id).Append(Environment.NewLine);
      
      sb.Append("  Name: ").Append(Name).Append(Environment.NewLine);
      
      sb.Append("}");
      sb.Append(Environment.NewLine);
      return sb.ToString();
    }
  }
  
  
}