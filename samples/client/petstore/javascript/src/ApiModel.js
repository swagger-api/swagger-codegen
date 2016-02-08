(function(root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define([], factory);
  } else if (typeof module === 'object' && module.exports) {
    // CommonJS-like environments that support module.exports, like Node.
    module.exports = factory();
  } else {
    // Browser globals (root is window)
    if (!root.SwaggerPetstore) {
      root.SwaggerPetstore = {};
    }
    root.SwaggerPetstore.ApiModel = factory();
  }
}(this, function() {
	'use strict';

	/**
	 Base class for all generated model classes,
	 containing common functionality.
	**/
	var ApiModel = function(){}
	ApiModel.prototype.toJson = function() { 
		return JSON.stringify(this); 
	}

	if (module) {
		module.ApiModel = ApiModel;
	}

	return ApiModel;

}));
