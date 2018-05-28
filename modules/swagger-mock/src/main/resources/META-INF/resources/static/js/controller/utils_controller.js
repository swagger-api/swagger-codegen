'use strict';
myApp.controller('UtilsController', [ '$scope', '$window',
		function($scope, $window) {

			var self = this;

			self.uctrl.OpenJsonFormatter = function() {
				loadUrl("/virtualservices/jsonbeautifier.html");
			};
			
			function loadUrl(url) {
				$window.open(url, "_blank", "location=no,toolbar=yes,scrollbars=yes,resizable=yes,top=500,left=500,width=1000,height=600");
			}
		} ]);
