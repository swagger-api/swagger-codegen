package io.swagger.mock;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import io.swagger.mock.entity.MockEntity;
import io.swagger.mock.model.MockKeyValue;
import io.swagger.mock.model.MockTransferObject;

@Service("mockService")
public class MockServiceImpl implements MockService {

	@Autowired
	MockRepository mockRepository;

	public List<MockTransferObject> findAllMockRequests() {
		Iterable<MockEntity> mockEntityList  = mockRepository.findAll();
		List<MockTransferObject> requestList = new ArrayList<>();
		for (MockEntity mockEntity : mockEntityList) {
				requestList.add(converterEToR(mockEntity));
		}
		return requestList;
	}

	public MockTransferObject findById(long id) {

		return converterEToR(mockRepository.findOne(id));
	}

	public MockTransferObject saveMockRequest(MockTransferObject mockRequest) {
		return  converterEToR(mockRepository.save(converterRToE(mockRequest)));
	}	

	public void updateMockRequest(MockTransferObject mockRequest) {
		mockRepository.save(converterRToE(mockRequest));
	}

	public void deleteMockRequestById(long id) {
		mockRepository.delete(id);
	}

	public boolean isMockRequestExist(MockTransferObject mockRequest) {
		return true; // findById(mockRequest.getId())!=null;
	}

	public MockTransferObject converterEToR(MockEntity mockEntity) {
		MockTransferObject request = new MockTransferObject();
		BeanUtils.copyProperties(mockEntity, request);
		List<MockKeyValue> availableParams = new LinkedList<>();
		if (mockEntity.getAvailableParamsList() != null) {
			String[] availableParamsList = mockEntity.getAvailableParamsList().split(":_:");
			if (availableParamsList != null && availableParamsList.length > 0) {
				for (String availableParamsStr : availableParamsList) {
					if (availableParamsStr.split("=").length == 2) {
						availableParams.add(
								new MockKeyValue(availableParamsStr.split("=")[0], availableParamsStr.split("=")[1]));
						//System.out.println(availableParamsStr.split("=")[0] + "  " + availableParamsStr.split("=")[1]);
					}
				}
			}
		}
		request.setAvailableParams(availableParams);
		return request;
	}

	public MockEntity converterRToE(MockTransferObject mockRequest) {
		MockEntity mockEntity = new MockEntity();
		BeanUtils.copyProperties(mockRequest, mockEntity);
		StringBuffer availableParamList = new StringBuffer();
		if (mockRequest.getAvailableParams() != null && mockRequest.getAvailableParams().size() > 0) {
			for (MockKeyValue availableParam : mockRequest.getAvailableParams()) {
				if (availableParam.getValue() != null) {
					availableParamList.append(availableParam.getKey() + "=" + availableParam.getValue() + ":_:");
				}
			}
			String availableParamStr = availableParamList.toString();
			if(availableParamStr.lastIndexOf(":_:") > 0 ) {
				mockEntity.setAvailableParamsList(availableParamStr.substring(0, availableParamStr.lastIndexOf(":_:")));
			} else if(availableParamStr != null && availableParamStr.trim().length() > 0) {
				mockEntity.setAvailableParamsList(availableParamStr);
			}
		}
		return mockEntity;
	}

	@Override
	public List<MockTransferObject> readByOperationId(String resource, String name) {
		Iterable<MockEntity> mockTransferObjectList = mockRepository.findAll();
		List<MockTransferObject> requestList = new ArrayList<>();
		for (MockEntity mockEntity : mockTransferObjectList) {
			if (mockEntity.getResource().equals(resource) && mockEntity.getOperationId().equals(name)) {
				requestList.add(converterEToR(mockEntity));
			}
		}
		return requestList;
	}

}