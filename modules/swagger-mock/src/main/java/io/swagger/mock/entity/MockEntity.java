package io.swagger.mock.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "mock")
public class MockEntity implements Serializable {

	private static final long serialVersionUID = -1801714432822866390L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private long id;

	@Column(name = "operationId", nullable = false)
	private String operationId;

	@Column(name = "input")
	private String input;

	@Column(name = "output")
	private String output;

	@Column(name = "httpStatusCode")
	private String httpStatusCode;

	@Column(name = "excludeList")
	private String excludeList;

	@Column(name = "availableParamsList")
	private String availableParamsList;

	public String getAvailableParamsList() {
		return availableParamsList;
	}

	public void setAvailableParamsList(String availableParamsList) {
		this.availableParamsList = availableParamsList;
	}

	public MockEntity() {

	}

	public MockEntity(String operationId, String input, String output) {
		this.operationId = operationId;
		this.input = input;
		this.output = output;
		this.httpStatusCode = httpStatusCode;
	}

	public String getExcludeList() {
		return excludeList;
	}

	public void setExcludeList(String excludeList) {
		this.excludeList = excludeList;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getOperationId() {
		return operationId;
	}

	public void setOperationId(String operationId) {
		this.operationId = operationId;
	}

	public String getInput() {
		return input;
	}

	public void setInput(String input) {
		this.input = input;
	}

	public String getOutput() {
		return output;
	}

	public void setOutput(String output) {
		this.output = output;
	}

	public String getHttpStatusCode() {
		return httpStatusCode;
	}

	public void setHttpStatusCode(String httpStatusCode) {
		this.httpStatusCode = httpStatusCode;
	}

}