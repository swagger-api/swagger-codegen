package com.wordnik.swagger.codegen;


public class CodegenResponse {
  // TODO rename "message" to "description" (according to com.wordnik.swagger.models.Response and specification)?
  public String code, message;
  public Boolean hasMore; // TODO I think this is not neede, because JMustache supports "-last"
  Object schema;
}