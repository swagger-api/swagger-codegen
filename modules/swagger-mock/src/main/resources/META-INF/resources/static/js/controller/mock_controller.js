'use strict';

myApp.controller('MockController', ['$scope',  '$filter', '$modal', 'MockService', function($scope, $filter,  $modal, MockService) {
    var self = this;
    self.mockRequest={id:'',resource:'',url:'',method:'',operationId:'',input:'',output:'',excludeList:'', httpStatus:'',availableParams:[]};
    self.mockCreateRequest= {id:'',resource:'',method:'',url:'',operationId:'',input:'',output:'',excludeList:'', httpStatus:'',availableParams:[]};
    self.mockRequests=[];
    self.mockLoadRequests=[];
    self.mockLoadRequest='';
    self.selectedOperationId ='';
    self.submit = submit;
    self.edit = edit;
    self.remove = remove;
    self.reset = reset;
    self.message ='';
    self.classCode = '';
    self.filtered = {};
    self.searchText ='';
    self.currentPage = 1;
    self.viewby = 5;
    self.perPage = 5;
    self.maxSize = 5;
	self.prettyJson = '';
	self.treeJson = '';
    
    loadAllMockRequest();
    
    self.setPage = function (pageNo) {
    	self.currentPage = pageNo;
    };
    
    
   self.jsonObj = JSON.parse("{  \"errorCode\": \"____NOT_FOUND\",  \"errorMessage\": \"sh e tehte besssssg egeh\"}");
   
    self.loadJson = function (value) {
 		console.log(value);
 		self.jsonObj = JSON.parse(value);
 		self.jsonStr = JSON.parse(value);
    }; 

    
    $scope.$watch(self.searchText, function (term) {
      var obj = term;
      self.filterList = $filter('filter')(self.mockRequests, obj);
      self.currentPage = 1;
    }); 
    
  
    self.setItemsPerPage = function(num) {
	    self.perPage = num;
	    self.currentPage = 1; //reset to first page
    };
        
    self.loadData = fetchAllMockRequest();
    	  
    function fetchAllMockRequest(){
    	MockService.fetchAllMockRequest()
            .then(
            function(d) {
                self.mockRequests = d;
                self.filterList = self.mockRequests;
            },
            function(errResponse){
                console.error('Error while fetching Mocks');
            }
        );
    };
    
    function loadAllMockRequest(){
        MockService.loadAllMockRequest()
            .then(
            function(d) {
            	self.mockLoadRequests = d;
           	 console.log("ALL API's", self.mockLoadRequests);
            },
            function(errResponse){
                console.error('Error while fetching mockLoadRequests');
            }
        );
    }

    
    function createMockRequest(mockRequest){
    	self.selectedOperationId = mockRequest.operationId;
    	self.message = 'Mock response added successfully for the given request parameter(s)!!!!';
    	self.classCode ='valid';
    	MockService.createMockRequest(mockRequest)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
            	self.message = errResponse.data.code;
            	self.classCode ='invalid';
                console.error('Error while creating MockRequest');
            }
        );
    }

    function updateMockRequest(mockRequest, id){
        MockService.updateMockRequest(mockRequest, id)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
                console.error('Error while updating MockRequest');
            }
        );
    }

    function deleteMockRequest(id){
        MockService.deleteMockRequest(id)
            .then(
            		fetchAllMockRequest,
            function(errResponse){
                console.error('Error while deleting MockRequest');
            }
        );
    }

    function submit() {
        console.log('Saving New mockRequest', self.mockRequest);
        self.mockCreateRequest= {id:'', 
        			resource:self.mockRequest.resource,
        			url:self.mockRequest.url,
        			operationId:self.mockRequest.operationId,
        			input:self.mockRequest.input,
        			output:self.mockRequest.output,
        			excludeList:self.mockRequest.excludeList, 
        			httpStatusCode:self.mockRequest.httpStatusCode,
        			method:self.mockRequest.method,
        			availableParams:self.mockRequest.availableParams};
        createMockRequest(self.mockCreateRequest);

    }

    function edit(id){
        console.log('id to be edited', id);
        for(var i = 0; i < self.mockRequests.length; i++){
            if(self.mockRequests[i].id === id) {
                self.mockRequest = angular.copy(self.mockRequests[i]);
                break;
            }
        }
    }

    function remove(id){
        console.log('id to be deleted', id);
        if(self.mockRequest.id === id) {//clean form if the mockRequest to be deleted is shown there.
            reset();
        }
        deleteMockRequest(id);
    }


    function reset(){
        self.message ='';
        self.classCode = '';
        self.mockRequest={id:null,operationId:'',input:'',output:'',excludeList:'', httpStatus:'',paramList:'' };
        $scope.myForm.$setPristine(); //reset Form
    }

}]);

