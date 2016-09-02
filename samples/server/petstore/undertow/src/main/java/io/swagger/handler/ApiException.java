package io.swagger.handler;

@javax.annotation.Generated(value = "class io.swagger.codegen.languages.UndertowCodegen", date = "2016-09-02T01:09:51.378-04:00")
public class ApiException extends Exception{
	private int code;
	public ApiException (int code, String msg) {
		super(msg);
		this.code = code;
	}
}
