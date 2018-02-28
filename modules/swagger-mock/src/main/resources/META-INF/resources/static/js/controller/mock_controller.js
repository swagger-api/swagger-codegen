'use strict';

myApp.controller('MockController', ['$scope', 'MockService', function($scope, MockService) {
    var self = this;
    self.mockRequest={id:'',operationId:'',input:'',output:'',excludeList:'', httpStatus:'',availableParams:[]};
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
    
    fetchAllMockRequest();
    loadAllMockRequest();

    function fetchAllMockRequest(){
        MockService.fetchAllMockRequest()
            .then(
            function(d) {
                self.mockRequests = d;
            },
            function(errResponse){
                console.error('Error while fetching Mocks');
            }
        );
    }

    
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
        self.mockRequest.id = '';
        createMockRequest(self.mockRequest);

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

