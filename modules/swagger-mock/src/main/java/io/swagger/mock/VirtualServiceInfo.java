package io.swagger.mock;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import io.swagger.mock.model.APIResponse;
import io.swagger.mock.model.MockKeyValue;
import io.swagger.mock.model.MockTransferObject;

@Service("virtualServiceInfo")
public class VirtualServiceInfo {

	private static final String CURLY_PATH = "Curly";

	private static final Logger log = LoggerFactory.getLogger(VirtualServiceInfo.class);

	@Autowired
	private ObjectMapper objectMapper;

	final String rxpCurly = "\\{(.*?)\\}";
	final Pattern pattern = Pattern.compile(rxpCurly, Pattern.MULTILINE);
	ResourceMapper resourceParent;

	public ResourceMapper getResourceParent() {
		return resourceParent;
	}

	public void setResourceParent(ResourceMapper resourceParent) {
		this.resourceParent = resourceParent;
	}

	Map<String, Map<String, MockTransferObject>> mockLoadChoice;

	public Map<String, Class> findVirtualServices(RequestMappingHandlerMapping handlerMapping) {
		Map<String, Class> virtualInterfaces = new HashMap<>();
		Pattern p = Pattern.compile("[class\\sa-zA-Z0-9]Controller");
		for (Map.Entry<RequestMappingInfo, HandlerMethod> mapSwaggerAPIEntry : handlerMapping.getHandlerMethods()
				.entrySet()) {
			Matcher m = p.matcher(mapSwaggerAPIEntry.getValue().getBeanType().toString()); 
			if (m.find()) {
				String interfaceName = mapSwaggerAPIEntry.getValue().getBeanType().getName().replace("Controller", "");
				Class intefaceController = null;
				try {
					intefaceController = Class.forName(interfaceName);
					interfaceName = interfaceName.substring(interfaceName.lastIndexOf(".") + 1, interfaceName.length());
					virtualInterfaces.put(interfaceName, intefaceController);
				} catch (Exception e) {
					System.out.println("Not a interfaceName for  Virtual API : " + interfaceName);
				}
			}
		}
		return virtualInterfaces;
	}

	@SuppressWarnings("deprecation")
	private MockTransferObject getResource(Map.Entry<String, Class> virtualServiceEntry) {
		String resource = null;
		MockTransferObject mockTransferObject = new MockTransferObject();
		Api[] apiOperationAnnos = (Api[]) virtualServiceEntry.getValue().getAnnotationsByType(Api.class);
		if (apiOperationAnnos != null) {
			for (Api api : apiOperationAnnos) {
				if (api.value() != null) {
					mockTransferObject.setResource(api.value());
				}
			}
		}
		return mockTransferObject;
	}

	public Map<String, Map<String, MockTransferObject>> loadVirtualServices(RequestMappingHandlerMapping handlerMapping)
			throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException {
		if (mockLoadChoice == null) {
			mockLoadChoice = new TreeMap<>();
			for (Map.Entry<String, Class> virtualServiceEntry : findVirtualServices(handlerMapping).entrySet()) {
				mockLoadChoice.put(virtualServiceEntry.getKey(), buildVirtualServiceInfo(virtualServiceEntry));
			}
		}
		return mockLoadChoice;
	}

	private Map<String, MockTransferObject>  buildVirtualServiceInfo(Map.Entry<String, Class> virtualServiceEntry)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		Map<String, MockTransferObject>  mockAPILoadChoice = new LinkedHashMap<String, MockTransferObject>();
		for (Method method : virtualServiceEntry.getValue().getDeclaredMethods()) {
			MockTransferObject mockTransferObjectReturn  = buildServiceDetails(virtualServiceEntry, method);
			if(mockTransferObjectReturn != null){
				mockAPILoadChoice.put(method.getName(), mockTransferObjectReturn);
			}
		}
		return mockAPILoadChoice;
	}

	private MockTransferObject buildServiceDetails(Map.Entry<String, Class> virtualServiceEntry, Method method)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		RequestMapping[] annotInstance = method.getAnnotationsByType(RequestMapping.class);
		MockTransferObject mockLoadRequest = getResource(virtualServiceEntry);
		mockLoadRequest .setDesc(getResourceDesc(method));
		if (annotInstance != null && annotInstance.length > 0) {
			RequestMapping requestMapping = ((RequestMapping) annotInstance[0]);
			if (requestMapping.value() != null && requestMapping.value().length > 0) {
				mockLoadRequest.setUrl(requestMapping.value()[0]);
			}
			if (requestMapping.method() != null && requestMapping.method().length > 0) {
				mockLoadRequest.setMethod(requestMapping.method()[0].name());
			}
			mockLoadRequest.setResponseType(buildResponseType(method));
			buildInput(method, mockLoadRequest);
			mockLoadRequest.setOperationId(method.getName());
			mockLoadRequest.setHttpStatusMap(getHttpStatusMap());
			return mockLoadRequest;
		}
		return null;
	}

	private String getResourceDesc(Method method) {
		ApiOperation[] apiOperationAnno = method.getAnnotationsByType(ApiOperation.class);
		if(apiOperationAnno != null && apiOperationAnno.length > 0){
			return apiOperationAnno[0].notes();
		} 
		return null; 
	}

	private void buildInput(Method method, MockTransferObject mockLoadRequest)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		int i = 0;
		List<MockKeyValue> availableParams = new ArrayList();
		Annotation[][] annotations = method.getParameterAnnotations();
		Class[] parameterTypes = method.getParameterTypes();
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
					if (parameterType.getName().contains("List")) {
						// TODO fix
						mockLoadRequest.setInput(objectMapper.writerWithDefaultPrettyPrinter()
								.writeValueAsString(new TypeReference<List<?>>() {
								}));
					} else {
						mockLoadRequest.setInput(objectMapper.writerWithDefaultPrettyPrinter()
								.writeValueAsString(Class.forName(parameterType.getName()).newInstance()));
					}
				}
			}
		}
		mockLoadRequest.setAvailableParams(availableParams);
	}

	private Map<String, APIResponse> buildResponseType(Method method)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		Map<String, APIResponse> responseType = new HashMap<>();
		ApiResponses[] apiResponsesAnno = method.getAnnotationsByType(ApiResponses.class);
		if (apiResponsesAnno != null) {
			for (ApiResponses apiResponses : apiResponsesAnno) {
				for (ApiResponse apiResponse : apiResponses.value()) {
					if (apiResponse.response().getCanonicalName() != null
							&& apiResponse.response().getCanonicalName().contains("io.swagger")) {
						responseType.put(String.valueOf(apiResponse.code()),
								new APIResponse(String.valueOf(apiResponse.code()),
										apiResponse.response().getCanonicalName(),
										objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(
												Class.forName(apiResponse.response().getCanonicalName()).newInstance()),
										apiResponse.message()));
					} else {
						responseType.put(String.valueOf(apiResponse.code()),
								new APIResponse(String.valueOf(apiResponse.code()), null, null, apiResponse.message()));
					}
				}
			}

		}
		return responseType;
	}

	public String getInputType(MockTransferObject mockTransferInput) {
		String inputType = null;
		if (mockTransferInput.getResource() != null) {
			String resource = mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + "Api";
			if (mockTransferInput.getOperationId() != null) {
				MockTransferObject mockTransferActual = mockLoadChoice.get(resource)
						.get(mockTransferInput.getOperationId());
				if (mockTransferActual != null) {
					inputType = mockTransferActual.getInputObjectType();
				}
			} else {
				String resourceUrl = mockTransferInput.getUrl().substring(1, mockTransferInput.getUrl().length());
				List<String> resouceSplitterList = new LinkedList(Arrays.asList(resourceUrl.split("/")));
				if (resouceSplitterList.size() > 0) {
					String operationId = getOperationId(mockTransferInput.getMethod(), resourceParent,
							resouceSplitterList);
					System.out.println(" ORG(" + mockTransferInput.getOperationId() + ") >>>>>>>>>>>>>>>> FOUND ("
							+ operationId + ") ");
					MockTransferObject mockTransferActual = mockLoadChoice.get(resource).get(operationId);
					if (mockTransferActual != null) {
						inputType = mockTransferActual.getInputObjectType();
					}
				}
			}
		}
		return inputType;
	}

	public String getOperationId(String httpVerb, ResourceMapper resourceParent, List<String> resouceSplitterList) {
		if (resouceSplitterList.size() == 0) {
			return resourceParent.getOperationId(httpVerb);
		}
		String resource = resouceSplitterList.get(0);
		ResourceMapper mapper = resourceParent.findResource(resource);
		if (mapper != null) {
			return getOperationId(httpVerb, mapper, resouceSplitterList.subList(1, resouceSplitterList.size()));
		} else {
			return getOperationId(httpVerb, resourceParent.findResource(CURLY_PATH),
					resouceSplitterList.subList(1, resouceSplitterList.size()));
		}

	}

	public MockTransferObject getResponseType(MockTransferObject mockTransferInput) {
		if (mockTransferInput.getResource() != null) {
			String resource = mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + "Api";
			if (mockTransferInput.getOperationId() != null) {
				return mockLoadChoice.get(resource).get(mockTransferInput.getOperationId());
			}
		}
		return null;
	}

	public Map<String, String> getHttpStatusMap() {
		Map<String, String> map = new LinkedHashMap<>();
		for (HttpStatus status : HttpStatus.values()) {
			map.put(String.valueOf(status.value()), status.name());
		}
		return map;
	}

	public void loadMapper() {
		Set<ResourceMapper> resourceMapperList = new LinkedHashSet<>();
		resourceParent = new ResourceMapper("Parent-Root", resourceMapperList);
		ResourceMapper resourceParent = new ResourceMapper("Parent-Root", resourceMapperList);
		for (Entry<String, Map<String, MockTransferObject>> obj : mockLoadChoice.entrySet()) {
			for (Entry<String, MockTransferObject> requestMockObject : obj.getValue().entrySet()) {
				String resource = requestMockObject.getValue().getUrl().substring(1,
						requestMockObject.getValue().getUrl().length());
				List<String> resouceSplitterList = new LinkedList(Arrays.asList(resource.split("/")));
				if (resouceSplitterList.size() > 0) {
					ResourceMapper mapperChild = buildHierarchyObject(requestMockObject.getValue().getMethod(),
							resourceParent, resouceSplitterList, requestMockObject.getKey());
					resourceParent.addResourceMapper(mapperChild);
				}
			}
		}
	}

	public ResourceMapper buildHierarchyObject(String httpVerb, ResourceMapper resourceParent,
			List<String> resouceSplitterList, String operationId) {
		String resource = resouceSplitterList.get(0);
		String actualResource = resouceSplitterList.get(0);
		final Matcher matcher = pattern.matcher(resouceSplitterList.get(0));
		if (matcher.find()) {
			resource = CURLY_PATH;
		}
		if (resouceSplitterList.size() == 1) {
			ResourceMapper resourceMapper = resourceParent.findResource(resource);
			if (resourceMapper == null) {
				resourceMapper = new ResourceMapper(resource);
				resourceMapper.setActualResource(actualResource);
			}
			resourceMapper.setOperationId(httpVerb, operationId);
			return resourceMapper;
		} else if (resourceParent.findResource(resource) != null) {
			ResourceMapper resourceChild = resourceParent.findResource(resource);
			ResourceMapper resourceMapperReturn = buildHierarchyObject(httpVerb, resourceChild,
					resouceSplitterList.subList(1, resouceSplitterList.size()), operationId);
			resourceChild.addResourceMapper(resourceMapperReturn);
			return resourceChild;
		} else {
			Set<ResourceMapper> mapperSet = new LinkedHashSet<>();
			ResourceMapper resourceMapper = new ResourceMapper(resource, mapperSet);
			resourceMapper.setActualResource(actualResource);
			ResourceMapper resourceMapperReturn = buildHierarchyObject(httpVerb, resourceMapper,
					resouceSplitterList.subList(1, resouceSplitterList.size()), operationId);
			resourceMapper.addResourceMapper(resourceMapperReturn);

			return resourceMapper;
		}
	}

	class ResourceMapper {
		String resource;
		String actualResource;
		Set<ResourceMapper> resourceMapperList = new LinkedHashSet<>();
		Map<String, String> operationIdMap = new HashMap<>();

		ResourceMapper(String resource, Set<ResourceMapper> resourceMapperList) {
			this.resource = resource;
			this.resourceMapperList = resourceMapperList;
		}

		ResourceMapper(String resource) {
			this.resource = resource;
		}

		public Map<String, String> getOperationIdMap() {
			return operationIdMap;
		}

		public void setOperationIdMap(Map<String, String> operationIdMap) {
			this.operationIdMap = operationIdMap;
		}

		public void setResourceMapperList(Set<ResourceMapper> resourceMapperList) {
			this.resourceMapperList = resourceMapperList;
		}

		@Override
		public String toString() {
			return "ResourceMapper [resource=" + resource + ", actualResource=" + actualResource
					+ ", resourceMapperList=" + resourceMapperList + ", operationId=" + operationIdMap + "]";
		}

		public ResourceMapper findResource(String resource) {
			if (resourceMapperList != null) {
				for (ResourceMapper mapper : resourceMapperList) {
					if (mapper.getResource().equalsIgnoreCase(resource)) {
						return mapper;
					}
				}
			}
			return null;
		}

		public void addResourceMapper(ResourceMapper resourceMapper) {
			if (resourceMapperList != null && resourceMapper != null) {
				resourceMapperList.add(resourceMapper);
			}
		}

		public String getActualResource() {
			return actualResource;
		}

		public void setActualResource(String actualResource) {
			this.actualResource = actualResource;
		}

		public String getResource() {
			return resource;
		}

		public void setResource(String resource) {
			this.resource = resource;
		}

		public Set<ResourceMapper> getResourceMapperList() {
			return resourceMapperList;
		}

		public void setResourceMapper(Set<ResourceMapper> resourceMapperList) {
			this.resourceMapperList = resourceMapperList;
		}

		public String getOperationId(String httpVerb) {
			return operationIdMap.get(httpVerb);
		}

		public void setOperationId(String httpVerb, String operationId) {
			operationIdMap.put(httpVerb, operationId);
			;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + ((resource == null) ? 0 : resource.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			ResourceMapper other = (ResourceMapper) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (resource == null) {
				if (other.resource != null)
					return false;
			} else if (!resource.equals(other.resource))
				return false;
			return true;
		}

		private VirtualServiceInfo getOuterType() {
			return VirtualServiceInfo.this;
		}

	}

}
