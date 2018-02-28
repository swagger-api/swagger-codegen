package io.swagger.mock;

import java.util.List;

import io.swagger.mock.model.MockTransferObject;

public interface MockService {

	MockTransferObject findById(long id);

	List<MockTransferObject> readByOperationId(String name);

	void saveMockRequest(MockTransferObject mockRequest);

	void updateMockRequest(MockTransferObject mockRequest);

	void deleteMockRequestById(long id);

	List<MockTransferObject> findAllMockRequests();

	boolean isMockRequestExist(MockTransferObject mockRequest);

}