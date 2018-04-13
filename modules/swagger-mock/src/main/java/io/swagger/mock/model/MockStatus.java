package io.swagger.mock.model;

public class MockStatus {

	private String code;
	private MockTransferObject mockTransferObject; 
	
	public MockStatus() {

	}

	public MockTransferObject getMockTransferObject() {
		return mockTransferObject;
	}

	public void setMockTransferObject(MockTransferObject mockTransferObject) {
		this.mockTransferObject = mockTransferObject;
	}

	public MockStatus(String code, MockTransferObject mockTransferObject) {
		this.code = code;
		this.mockTransferObject = mockTransferObject;
	}
	
	public MockStatus(String code) {
		this.code = code;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

}
