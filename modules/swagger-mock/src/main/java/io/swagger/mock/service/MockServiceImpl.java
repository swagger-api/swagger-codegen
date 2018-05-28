package io.swagger.mock.service;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.swagger.mock.dao.MockRepository;
import io.swagger.mock.entity.MockEntity;
import io.swagger.mock.model.MockKeyValue;
import io.swagger.mock.model.MockTransferObject;

@Service("mockService")
public class MockServiceImpl implements MockService {

	@Autowired
	MockRepository mockRepository;

	static Map<Long, MockTransferObject> cacheMap;

	@PostConstruct
	public void init() {
		loadInitial();
	}

	public void loadInitial() {
		cacheMap = new ConcurrentHashMap<Long, MockTransferObject>();
		Iterable<MockEntity> mockEntityList = mockRepository.findAll();
		List<MockTransferObject> requestList = new ArrayList<>();
		for (MockEntity mockEntity : mockEntityList) {
			MockTransferObject mockTransferObject = converterEToR(mockEntity);
			requestList.add(mockTransferObject);
			cacheMap.put(mockTransferObject.getId(), mockTransferObject);
		}
	}

	@Transactional
	public List<MockTransferObject> findAllMockRequests() {
		if (cacheMap == null) {
			loadInitial();
		}
		return new ArrayList<MockTransferObject>(cacheMap.values());
	}

	@Transactional
	public MockTransferObject findById(long id) {
		return converterEToR(mockRepository.findOne(id));
	}

	
	// @CacheEvict(value = "mock", key = "{
	// #mockTransferObject.resource+'_'+#mockTransferObject.operationId}")
	public MockTransferObject saveMockRequest(MockTransferObject mockTransferObject) {
		MockTransferObject mockTransferObjectResponse = converterEToR(
				mockRepository.save(converterRToE(mockTransferObject)));
		cacheMap.put(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
		return mockTransferObjectResponse;
	}

	public void updateMockRequest(MockTransferObject mockRequest) {
		MockTransferObject mockTransferObjectResponse = converterEToR(mockRepository.save(converterRToE(mockRequest)));
		cacheMap.put(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
	}

	public void deleteMockRequestById(long id) {
		MockTransferObject mockTransferObject = findById(id);
		mockRepository.delete(id);
		cacheMap.remove(id);
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
						// System.out.println(availableParamsStr.split("=")[0] +
						// " " + availableParamsStr.split("=")[1]);
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
			if (availableParamStr.lastIndexOf(":_:") > 0) {
				mockEntity.setAvailableParamsList(availableParamStr.substring(0, availableParamStr.lastIndexOf(":_:")));
			} else if (availableParamStr != null && availableParamStr.trim().length() > 0) {
				mockEntity.setAvailableParamsList(availableParamStr);
			}
		}
		return mockEntity;
	}

	/*public static Predicate<MockTransferObject> filterOperationIdAndResource(String resource, String operationId) {
		return p -> (p.getResource().equalsIgnoreCase(resource) && p.getOperationId().equalsIgnoreCase(operationId));
	}*/

	/*
	 * public interface DomainOperations<T> { default List<T>
	 * filter(Predicate<T> predicate) { return persons.stream().filter(
	 * predicate ) .collect(Collectors.<Person>toList()); } }
	 */

	@Override
	@Transactional
	// @Cacheable(value = "mock", key = "{ #resource+'_'+#operationId}",
	// unless="#result==null")
	public List<MockTransferObject> readByOperationId(String resource, String operationId) {
		long startTime = System.currentTimeMillis();
		/*
		 * Iterable<MockEntity> mockTransferObjectList =
		 * mockRepository.findByOperationIdAndResource(operationId, resource);
		 * long endTime = System.currentTimeMillis(); System.out.println(
		 * "DB Call   " + (endTime-startTime)/1000); startTime =
		 * System.currentTimeMillis(); List<MockTransferObject> requestList =
		 * new ArrayList<>(); for (MockEntity mockEntity :
		 * mockTransferObjectList) { if
		 * (mockEntity.getResource().equals(resource) &&
		 * mockEntity.getOperationId().equals(operationId)) {
		 * requestList.add(converterEToR(mockEntity)); } }
		 */
		/*List<MockTransferObject> requestList = findAllMockRequests().stream()
				.filter(filterOperationIdAndResource(resource, operationId))
				.collect(Collectors.<MockTransferObject> toList());*/
		
		List<MockTransferObject> requestList = new ArrayList<>();
		for(MockTransferObject transferObject : findAllMockRequests()){
			if(transferObject.getResource().equalsIgnoreCase(resource) 
					&& transferObject.getOperationId().equalsIgnoreCase(operationId)){
				requestList.add(transferObject);
			}
		}
		long endTime = System.currentTimeMillis();
		System.out.println("converter Calls   " + (endTime - startTime) / 1000);
		return requestList;
	}

}