(function () {
    'use strict';
	myApp.config(config)
        .run(run);

    function config($stateProvider, $urlRouterProvider) {
        // default route
        $urlRouterProvider.otherwise("/");

        // app routes
        $stateProvider
            .state('mock', {
                url: '/mock',
                templateUrl: '/virtualservices/static/home/mock.html',
            })
            
            .state('swagger', {
                url: '/swagger',
                templateUrl: '/virtualservices/static/home/swagger.html',
            })
        
            .state('utils', {
                url: '/utils',
                templateUrl: '/virtualservices/static/home/utils.html',
            })
        
           }

    function run() {
    }
})();