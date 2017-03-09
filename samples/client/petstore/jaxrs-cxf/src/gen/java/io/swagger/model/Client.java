package io.swagger.model;


import io.swagger.annotations.ApiModelProperty;

public class Client  {
  
  @ApiModelProperty(example = "null", value = "")
  private String client = null;

 /**
   * Get client
   * @return client
  **/
  public String getClient() {
    return client;
  }

  public void setClient(String client) {
    this.client = client;
  }

  public Client client(String client) {
    this.client = client;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Client {\n");
    
    sb.append("    client: ").append(toIndentedString(client)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

