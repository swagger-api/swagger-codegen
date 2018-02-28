package io.swagger.mock;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
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
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.mock.model.APIResponse;
import io.swagger.mock.model.MockKeyValue;
import io.swagger.mock.model.MockRequest;
import io.swagger.mock.model.MockResponse;
import io.swagger.mock.model.MockTransferObject;

@Service("mockUtil")
public class MockUtil {

	private static final Logger log = LoggerFactory.getLogger(MockUtil.class);

	@Autowired
	private MockService mockService;

	@Autowired
	private ObjectMapper objectMapper;

	public Map<String, Map<String, MockTransferObject>> loadMockRequests(RequestMappingHandlerMapping handlerMapping)
			throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException {
		
		Map<String, Map<String, MockTransferObject>> mockLoadChoice = new LinkedHashMap<>();
		Map<RequestMappingInfo, HandlerMethod> mapSwaggerAPI = handlerMapping.getHandlerMethods();
		for (Map.Entry<RequestMappingInfo, HandlerMethod> mapSwaggerAPIEntry : mapSwaggerAPI.entrySet()) {
			if (mapSwaggerAPIEntry.getValue().getBeanType().toString().matches("class io.swagger.api.*Controller")) {
				String interfaceName = mapSwaggerAPIEntry.getValue().getBeanType().getName().replace("Controller", "");
				Class intefaceController = Class.forName(interfaceName);
				interfaceName = interfaceName.substring(interfaceName.lastIndexOf(".") + 1, interfaceName.length());
				if (!mockLoadChoice.containsKey(interfaceName)) {
					Map<String, MockTransferObject> mockAPILoadChoice = new LinkedHashMap<String, MockTransferObject>();
					for (Method method : intefaceController.getDeclaredMethods()) {
						Annotation[][] annotations = method.getParameterAnnotations();
						Class[] parameterTypes = method.getParameterTypes();
						RequestMapping[] annotInstance = method.getAnnotationsByType(RequestMapping.class);
						MockTransferObject mockLoadRequest = new MockTransferObject();
						if (annotInstance != null && annotInstance.length > 0) {
							RequestMapping requestMapping = ((RequestMapping) annotInstance[0]);
							if (requestMapping.value() != null && requestMapping.value().length > 0) {
								mockLoadRequest.setUrl(requestMapping.value()[0]);
							}
							if (requestMapping.method() != null && requestMapping.method().length > 0) {
								mockLoadRequest.setMethod(requestMapping.method()[0].name());
							}

							ApiResponses[] apiResponsesAnno = method.getAnnotationsByType(ApiResponses.class);
							if (apiResponsesAnno != null) {
								Map<String, APIResponse> responseType = new HashMap<>();
								System.out.println(method.getName());
								for (ApiResponses apiResponses : apiResponsesAnno) {
									for (ApiResponse apiResponse : apiResponses.value()) {
										System.out.println("apiResponse.response().getCanonicalName() >>"+ apiResponse.response().getCanonicalName() );
										if (apiResponse.response().getCanonicalName() != null && !apiResponse.response()
												.getCanonicalName().contains("java.lang.Void")) {
											responseType
													.put(String.valueOf(apiResponse.code()),
															new APIResponse(String.valueOf(apiResponse.code()),
																	apiResponse.response().getCanonicalName(),
																	objectMapper.writerWithDefaultPrettyPrinter()
																			.writeValueAsString(Class
																					.forName(apiResponse.response()
																							.getCanonicalName())
																					.newInstance()),
																	apiResponse.message()));
										} else {
											responseType.put(String.valueOf(apiResponse.code()),
													new APIResponse(String.valueOf(apiResponse.code()), null, null,
															apiResponse.message()));
										}
										mockLoadRequest.setResponseType(responseType);
									}
								}

							}

							int i = 0;
							List<MockKeyValue> availableParams = new ArrayList();
							for (Annotation[] anns : annotations) {
								Class parameterType = parameterTypes[i++];
								for (Annotation paramAnnotation : anns) {
									if (paramAnnotation.annotationType().equals(RequestParam.class)) {
										RequestParam requestParam = (RequestParam) paramAnnotation;
										availableParams.add(new MockKeyValue(requestParam.value(), null));
									} else if (paramAnnotation.annotationType().equals(PathVariable.class)) {
										PathVariable pathVariable = (PathVariable) paramAnnotation;
										availableParams.add(new MockKeyValue(pathVariable.value(), null));
									} else if (paramAnnotation.annotationType().equals(RequestBody.class)) {
										mockLoadRequest.setInputObjectType(parameterType.getName());
										mockLoadRequest.setInput(
												objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(
														Class.forName(parameterType.getName()).newInstance()));
									}
								}
							}
							// TO build Return Json object
							String returnObject = method.getGenericReturnType().getTypeName();
							mockLoadRequest.setOperationId(method.getName());
							mockLoadRequest.setAvailableParams(availableParams);
							mockLoadRequest.setHttpStatusMap(getHttpStatusMap());
							mockAPILoadChoice.put(method.getName(), mockLoadRequest);
						}
					}
					mockLoadChoice.put(interfaceName, mockAPILoadChoice);
				}
			}

		}
		return mockLoadChoice;
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

	public Map<MockRequest, MockResponse> readDynamicResponse(String operationId) throws IOException {

		Map<MockRequest, MockResponse> mockResponseMap = new HashMap();
		try {
			List<MockTransferObject> mockTransferObjectList = mockService.readByOperationId(operationId);
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

	public boolean isMockAlreadyExists(MockTransferObject mockTransferObject) {
		objectMapper.findAndRegisterModules();
		boolean isValid = false;
		try {
			HashMap<String, String> availableParamMap = new HashMap<>();
			if (mockTransferObject.getAvailableParams() != null && mockTransferObject.getAvailableParams().size() > 0) {
				for (MockKeyValue availableParam : mockTransferObject.getAvailableParams()) {
					availableParamMap.put(availableParam.getKey(), availableParam.getValue());
				}
			}

			Map<MockRequest, MockResponse> mockDataSetupMap = readDynamicResponse(mockTransferObject.getOperationId());
			for (Map.Entry<MockRequest, MockResponse> mockRequestResponse : mockDataSetupMap.entrySet()) {
				if (availableParamMap != null && availableParamMap.size() > 0
						&& mockTransferObject.getInput() != null) {
					if (compareQueryParams(mockRequestResponse.getKey(), availableParamMap)
							&& EqualsBuilder.reflectionEquals(
									objectMapper.readValue(mockRequestResponse.getKey().getInput(),
											Class.forName(mockTransferObject.getInputObjectType())),
									objectMapper.readValue(mockTransferObject.getInput(),
											Class.forName(mockTransferObject.getInputObjectType())),
									mockRequestResponse.getKey().getExcludeSet())) {
						return true;
					}
				} else if (availableParamMap != null && availableParamMap.size() > 0
						&& compareQueryParams(mockRequestResponse.getKey(), availableParamMap)) {
					return true;
				} else if (mockTransferObject.getInput() != null && EqualsBuilder.reflectionEquals(
						objectMapper.readValue(mockRequestResponse.getKey().getInput(),
								Class.forName(mockTransferObject.getInputObjectType())),
						objectMapper.readValue(mockTransferObject.getInput(),
								Class.forName(mockTransferObject.getInputObjectType())),
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
		objectMapper.findAndRegisterModules();
		boolean isValid = true;
		try {

			if (mockTransferObject.getInput() != null && mockTransferObject.getInput().length() > 0
					&& mockTransferObject.getInputObjectType() != null
					&& mockTransferObject.getInputObjectType().length() > 0) {
				objectMapper.readValue(mockTransferObject.getInput(),
						Class.forName(mockTransferObject.getInputObjectType()));
				isValid = true;
			} else if (mockTransferObject.getInputObjectType() == null
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
			if (mockTransferObject.getResponseType().size() > 0 && validResponse(mockTransferObject)) {
				isValid = true;
			} else if (mockTransferObject.getResponseType().get(mockTransferObject.getHttpStatusCode()) == null) {
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

	private boolean validResponse(MockTransferObject mockTransferObject)
			throws JsonParseException, JsonMappingException, ClassNotFoundException, IOException {
		objectMapper.findAndRegisterModules();
		APIResponse apiResponse = mockTransferObject.getResponseType().get(mockTransferObject.getHttpStatusCode());
		if (apiResponse != null && apiResponse.getObjectType() != null) {
			objectMapper.readValue(mockTransferObject.getOutput(), Class.forName(apiResponse.getObjectType()));
		}
		return true;
	}

}
