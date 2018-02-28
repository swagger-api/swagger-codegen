package io.swagger.mock;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import io.swagger.mock.model.MockStatus;
import io.swagger.mock.model.MockTransferObject;

@RestController
public class MockController {

	@Autowired
	MockService mockService; // Service which will do all data 

	@Autowired
	private MockUtil mockUtil;

	private final RequestMappingHandlerMapping handlerMapping;

	@Autowired
	public MockController(RequestMappingHandlerMapping handlerMapping) {
		this.handlerMapping = handlerMapping;
	}

	@RequestMapping(value = "/mockload/", method = RequestMethod.GET)
	public Map<String, Map<String, MockTransferObject>> listAllMockLoadRequest() throws InstantiationException,
			IllegalAccessException, ClassNotFoundException, JsonParseException, JsonMappingException, IOException {
		return mockUtil.loadMockRequests(handlerMapping);
	}

	@RequestMapping(value = "/mockservice/", method = RequestMethod.GET)
	public ResponseEntity<List<MockTransferObject>> listAllMockLoadRequests() {
		List<MockTransferObject> MockLoadRequests = mockService.findAllMockRequests();
		if (MockLoadRequests.isEmpty()) {
			return new ResponseEntity<List<MockTransferObject>>(HttpStatus.NO_CONTENT);
		}
		return new ResponseEntity<List<MockTransferObject>>(MockLoadRequests, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<MockTransferObject> getMockLoadRequest(@PathVariable("id") long id) {
		MockTransferObject mockLoadRequest = mockService.findById(id);
		if (mockLoadRequest == null) {
			return new ResponseEntity<MockTransferObject>(HttpStatus.NOT_FOUND);
		}
		return new ResponseEntity<MockTransferObject>(mockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice/", method = RequestMethod.POST)
	public ResponseEntity<MockStatus> createMockRequest(@RequestBody MockTransferObject mockLoadRequest) {// ,UriComponentsBuilder
		try {

			if (!mockUtil.isMockRequestBodyValid(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("Check input Json for the Mock Request Body, Please correct the Json!!!"),
						HttpStatus.BAD_REQUEST);
			}

			if (!mockUtil.isMockResponseBodyValid(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("Check input Json for the Mock Response Body, Please correct the Json!!!"),
						HttpStatus.BAD_REQUEST);
			}

			if (mockUtil.isMockAlreadyExists(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("This Mock request already Present, Please change input Data!!!"),
						HttpStatus.BAD_REQUEST);
			}
			mockService.saveMockRequest(mockLoadRequest);
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<MockStatus>(new MockStatus("Unexpected error please retry....."),
					HttpStatus.BAD_REQUEST);
		}

		// HttpHeaders headers = new HttpHeaders();
		// headers.setLocation(ucBuilder.path("/mockservice/{id}").buildAndExpand(mockLoadRequest.getId()).toUri());
		return new ResponseEntity<MockStatus>(new MockStatus("Mock created successfully"), HttpStatus.CREATED);
	}

	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.PUT)
	public ResponseEntity<MockTransferObject> updateMockRequest(@PathVariable("id") long id,
			@RequestBody MockTransferObject mockLoadRequest) {

		MockTransferObject currentMockLoadRequest = mockService.findById(id);
		if (currentMockLoadRequest == null) {
			return new ResponseEntity<MockTransferObject>(HttpStatus.NOT_FOUND);
		}

		currentMockLoadRequest.setInput(mockLoadRequest.getInput());
		currentMockLoadRequest.setOutput(mockLoadRequest.getOutput());
		currentMockLoadRequest.setOperationId(mockLoadRequest.getOperationId());

		mockService.updateMockRequest(currentMockLoadRequest);
		return new ResponseEntity<MockTransferObject>(currentMockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.DELETE)
	public ResponseEntity<MockTransferObject> deleteMockRequest(@PathVariable("id") long id) {
		MockTransferObject MockLoadRequest = mockService.findById(id);
		if (MockLoadRequest == null) {
			return new ResponseEntity<MockTransferObject>(HttpStatus.NOT_FOUND);
		}
		mockService.deleteMockRequestById(id);
		return new ResponseEntity<MockTransferObject>(HttpStatus.NO_CONTENT);
	}
}