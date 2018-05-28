package io.swagger.mock;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.mock.model.APIResponse;
import io.swagger.mock.model.MockKeyValue;
import io.swagger.mock.model.MockRequest;
import io.swagger.mock.model.MockResponse;
import io.swagger.mock.model.MockServiceRequest;
import io.swagger.mock.model.MockTransferObject;
import io.swagger.mock.service.MockService;

@Service("mockUtil")
public class MockUtil {

	private static final Logger log = LoggerFactory.getLogger(MockUtil.class);

	@Autowired
	private MockService mockService;

	@Autowired
	private ObjectMapper objectMapper;
	
	@Autowired
	VirtualServiceInfo virtualServiceInfo; // Service which will do all data 

	
	private ObjectMapper getObjectMapper(){
		objectMapper.findAndRegisterModules();
		return objectMapper.enable(
                DeserializationFeature.FAIL_ON_INVALID_SUBTYPE,
                DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES
                //,DeserializationFeature.FAIL_ON_IGNORED_PROPERTIES
        ) ;
	}
	
	public Map<String, String> getHttpStatusMap() {
		Map<String, String> map = new LinkedHashMap<>();
		for (HttpStatus status : HttpStatus.values()) {
			map.put(String.valueOf(status.value()), status.name());
		}
		return map;
	}

	public JsonObject readAsJsonObject(String jsonStr) {
		JsonObject jsonObject = null;
		try {
			Reader reader = new StringReader(jsonStr);
			// create JsonReader object
			JsonReader jsonReader = Json.createReader(reader);
			// get JsonObject from JsonReader
			jsonObject = jsonReader.readObject();
			// we can close IO resource and JsonReader now
			jsonReader.close();
			reader.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return jsonObject;
	}

	public Map<MockRequest, MockResponse> readDynamicResponse(String resource, String operationId) throws IOException {

		Map<MockRequest, MockResponse> mockResponseMap = new HashMap();
		try {
			List<MockTransferObject> mockTransferObjectList = mockService.readByOperationId(resource, operationId);
			for (MockTransferObject mockTransferObject : mockTransferObjectList) {
				String input = mockTransferObject.getInput();
				String output = mockTransferObject.getOutput();

				Set excludeSet = null;
				if (mockTransferObject.getExcludeList() != null) {
					excludeSet = new HashSet<String>(Arrays.asList(mockTransferObject.getExcludeList().split(",")));
				}
				MockRequest mockRequest = new MockRequest(input, excludeSet, mockTransferObject.getAvailableParams());
				MockResponse mockResponse = new MockResponse(output, mockTransferObject.getHttpStatusCode());
				mockResponseMap.put(mockRequest, mockResponse);
			}
		} catch (Exception e) {
			e.printStackTrace();
			log.error("Rest Mock API Response for " + operationId + " has not loaded : " + e.getMessage());
		}
		return mockResponseMap;
	}

	
	
	public boolean compareQueryParams(MockRequest mockRequest, Map<String, String> actualQueryMap) {
		if(mockRequest.getAvailableParams() == null || mockRequest.getAvailableParams().size()==0) {
			return isEmptyRequest(actualQueryMap);
		}  else {
			return isParameterMatch(mockRequest, actualQueryMap);
		}
	}

	private boolean isParameterMatch(MockRequest mockRequest, Map<String, String> actualQueryMap) {
		for (MockKeyValue mockKeyValueParams : mockRequest.getAvailableParams()) {
			if (mockRequest.getExcludeSet() == null
					|| !mockRequest.getExcludeSet().contains(mockKeyValueParams.getKey())) {
				if (!mockKeyValueParams.getValue().equals(actualQueryMap.get(mockKeyValueParams.getKey()))) {
					return false;
				}
			}
		}
		return true;
	}

	private boolean isEmptyRequest(Map<String, String> actualQueryMap) {
		for (Map.Entry<String, String> checkEmpty  : actualQueryMap.entrySet()) {
			if(!"null".equals(checkEmpty.getValue())){
				return false;
			}
		}
		return true;
	}

	public boolean isMockAlreadyExists(MockTransferObject mockTransferObject) {
		
		boolean isValid = false;
		
		try {
			String inputObjectType = virtualServiceInfo.getInputType(mockTransferObject); 

			HashMap<String, String> availableParamMap = new HashMap<>();
			if (mockTransferObject.getAvailableParams() != null && mockTransferObject.getAvailableParams().size() > 0) {
				for (MockKeyValue availableParam : mockTransferObject.getAvailableParams()) {
					availableParamMap.put(availableParam.getKey(), availableParam.getValue());
				}
			}

			Map<MockRequest, MockResponse> mockDataSetupMap = readDynamicResponse(mockTransferObject.getResource(),  mockTransferObject.getOperationId());
			for (Map.Entry<MockRequest, MockResponse> mockRequestResponse : mockDataSetupMap.entrySet()) {
				if (availableParamMap != null && availableParamMap.size() > 0
						&& mockTransferObject.getInput() != null) {
					if (compareQueryParams(mockRequestResponse.getKey(), availableParamMap)
							&& EqualsBuilder.reflectionEquals(
									getObjectMapper().readValue(mockRequestResponse.getKey().getInput(),
											Class.forName(inputObjectType)),
									getObjectMapper().readValue(mockTransferObject.getInput(),
											Class.forName(inputObjectType)),
									mockRequestResponse.getKey().getExcludeSet())) {
						return true;
					}
				} else if (availableParamMap != null && availableParamMap.size() > 0
						&& compareQueryParams(mockRequestResponse.getKey(), availableParamMap)) {
					return true;
				} else if (mockTransferObject.getInput() != null && EqualsBuilder.reflectionEquals(
						getObjectMapper().readValue(mockRequestResponse.getKey().getInput(),
								Class.forName(inputObjectType)),
						getObjectMapper().readValue(mockTransferObject.getInput(),
								Class.forName(inputObjectType)),
						mockRequestResponse.getKey().getExcludeSet())) {
					return true;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return isValid;
	}

	public boolean isMockRequestBodyValid(MockTransferObject mockTransferObject) {
		boolean isValid = true;
		String inputObjectType = virtualServiceInfo.getInputType(mockTransferObject);
		try {
			 
			if (mockTransferObject.getInput() != null && mockTransferObject.getInput().length() > 0
					&& inputObjectType != null
					&& inputObjectType.length() > 0) {
				Class classs  = Class.forName(inputObjectType);
				getObjectMapper().readValue(mockTransferObject.getInput(),classs);				
				isValid = true;
			} else if (inputObjectType == null
					&& (mockTransferObject.getInput() == null || mockTransferObject.getInput().length() == 0)) {
				isValid = true;
			} else {
				isValid = false;
			}
		} catch (Exception e) {
			e.printStackTrace();
			isValid = false;
		}
		
		return isValid;
	}

	public boolean isMockResponseBodyValid(MockTransferObject mockTransferObject) {
		boolean isValid = true;
		try {
			MockTransferObject mockTransferObjectActual = virtualServiceInfo.getResponseType(mockTransferObject);
			
			if (mockTransferObjectActual.getResponseType().size() > 0 && validResponse(mockTransferObjectActual, mockTransferObject)) {
				isValid = true;
			/*} else if (mockTransferObjectActual.getResponseType().get(mockTransferObject.getHttpStatusCode()) == null) {
				isValid = true;*/
			} else {
				isValid = false;
			}
		} catch (Exception e) {
			e.printStackTrace();
			isValid = false;
		}
		return isValid;
	}

	private boolean validResponse(MockTransferObject mockTransferObjectActual, MockTransferObject mockTransferObject)
			throws JsonParseException, JsonMappingException, ClassNotFoundException, IOException {
		APIResponse apiResponse = mockTransferObjectActual.getResponseType().get(mockTransferObject.getHttpStatusCode());
		if (apiResponse != null && apiResponse.getObjectType() != null) {
			objectMapper.readValue(mockTransferObject.getOutput(), Class.forName(apiResponse.getObjectType()));
		}
		return true;
	}
	
	
	public ResponseEntity returnResponse(MockServiceRequest mockServiceRequest) throws ClassNotFoundException, IOException {
		Map<MockRequest, MockResponse> mockDataSetupMap = readDynamicResponse(mockServiceRequest.getResource(), mockServiceRequest.getOperationId());
		for (Map.Entry<MockRequest, MockResponse> mockRequestResponse : mockDataSetupMap.entrySet()) {
			if(mockServiceRequest.getParams() != null && mockServiceRequest.getParams().size() > 0 
					&& mockServiceRequest.getInputObjectType() != null	&& mockServiceRequest.getInputObject() != null ) {
				if (compareQueryParams(mockRequestResponse.getKey(), mockServiceRequest.getParams()) && EqualsBuilder.reflectionEquals(
						getObjectMapper().readValue(mockRequestResponse.getKey().getInput(), mockServiceRequest.getInputObjectType()),
						mockServiceRequest.getInputObjectType().cast(mockServiceRequest.getInputObject()),
						mockRequestResponse.getKey().getExcludeSet())) {
						return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
							HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
				}
			} else if(mockServiceRequest.getParams() != null && mockServiceRequest.getParams().size() > 0 ) {
					if (compareQueryParams(mockRequestResponse.getKey(), mockServiceRequest.getParams())) {
							return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
								HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
					}
			} else if(mockServiceRequest.getInputObjectType() != null ) {
				if (EqualsBuilder.reflectionEquals(
						getObjectMapper().readValue(mockRequestResponse.getKey().getInput(), mockServiceRequest.getInputObjectType()),
						mockServiceRequest.getInputObjectType().cast(mockServiceRequest.getInputObject()),
						mockRequestResponse.getKey().getExcludeSet())) {
						return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
							HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
				}
			}
			
		}
		if (mockDataSetupMap.size() > 0) {
			return new ResponseEntity(
					"{\"code\": \"MISSING_MOCK_DATA\", \"message\":\"Mock response was not added for the given parameter\"}",
					HttpStatus.INTERNAL_SERVER_ERROR);
		} else {
			log.error("Mock Response was not defined for the given input createDocument");
			return new ResponseEntity(
					"{\"code\": \"MOCK_DATA_NOT_SET\", \"message\":\"Mock Response was not defined for the given input\"}",
					HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	
}
