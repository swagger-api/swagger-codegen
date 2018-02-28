'use strict';

myApp.factory('MockService', ['$http', '$q', function($http, $q){

    var REST_SERVICE_URI = '/mockservice/mockservice/';
    var REST_SERVICE_URI_LOAD = '/mockservice/mockload/';

    var factory = {
    	loadAllMockRequest: loadAllMockRequest,
        fetchAllMockRequest: fetchAllMockRequest,
        createMockRequest: createMockRequest,
        updateMockRequest:updateMockRequest,
        deleteMockRequest:deleteMockRequest
    };

    return factory;

    function loadAllMockRequest() {
        var deferred = $q.defer();
        $http.get(REST_SERVICE_URI_LOAD)
            .then(
            function (response) {
                deferred.resolve(response.data);
            },
            function(errResponse){
                console.error('Error while fetching loadAllMockRequest');
                deferred.reject(errResponse);
            }
        );
        return deferred.promise;
    }
    
    
    function fetchAllMockRequest() {
        var deferred = $q.defer();
        $http.get(REST_SERVICE_URI)
            .then(
            function (response) {
                deferred.resolve(response.data);
            },
            function(errResponse){
                console.error('Error while fetching MockRequests');
                deferred.reject(errResponse);
            }
        );
        return deferred.promise;
    }

    function createMockRequest(mockLoadRequest) {
        var deferred = $q.defer();
        $http.post(REST_SERVICE_URI, mockLoadRequest)
            .then(
            function (response) {
                deferred.resolve(response.data);
            },
            function(errResponse){
                console.error('Error while creating MockRequest');
                deferred.reject(errResponse);
                return errResponse;
            }
        );
        return deferred.promise;
    }


    function updateMockRequest(mockLoadRequest, id) {
        var deferred = $q.defer();
        $http.put(REST_SERVICE_URI+id, mockLoadRequest)
            .then(
            function (response) {
                deferred.resolve(response.data);
            },
            function(errResponse){
                console.error('Error while updating MockRequest');
                deferred.reject(errResponse);
            }
        );
        return deferred.promise;
    }

    function deleteMockRequest(id) {
        var deferred = $q.defer();
        $http.delete(REST_SERVICE_URI+id)
            .then(
            function (response) {
                deferred.resolve(response.data);
            },
            function(errResponse){
                console.error('Error while deleting MockRequest');
                deferred.reject(errResponse);
            }
        );
        return deferred.promise;
    }

}]);


