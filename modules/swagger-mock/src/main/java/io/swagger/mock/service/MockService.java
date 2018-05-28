package io.swagger.mock.service;

import java.util.List;

import io.swagger.mock.model.MockTransferObject;

public interface MockService {

	MockTransferObject findById(long id);

	List<MockTransferObject> readByOperationId(String resource, String name);

	MockTransferObject saveMockRequest(MockTransferObject mockRequest);

	void updateMockRequest(MockTransferObject mockRequest);

	void deleteMockRequestById(long id);

	List<MockTransferObject> findAllMockRequests();

	boolean isMockRequestExist(MockTransferObject mockRequest);

}