'use strict';


/**
 * Test serialization of outer boolean types
 *
 * body Boolean Input boolean as post body (optional)
 * returns OuterBoolean
 **/
exports.fakeOuterBooleanSerialize = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = true;
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Test serialization of object with outer number type
 *
 * body OuterComposite Input composite as post body (optional)
 * returns OuterComposite
 **/
exports.fakeOuterCompositeSerialize = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = {
  "my_string" : "my_string",
  "my_number" : 0.8008281904610115,
  "my_boolean" : true
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Test serialization of outer number types
 *
 * body BigDecimal Input number as post body (optional)
 * returns OuterNumber
 **/
exports.fakeOuterNumberSerialize = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = 0.8008281904610115;
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Test serialization of outer string types
 *
 * body String Input string as post body (optional)
 * returns OuterString
 **/
exports.fakeOuterStringSerialize = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = "";
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * To test \"client\" model
 * To test \"client\" model
 *
 * body Client client model
 * returns Client
 **/
exports.testClientModel = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = {
  "client" : "client"
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 *
 * body Body_2 
 * no response value expected for this operation
 **/
exports.testEndpointParameters = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * To test enum parameters
 * To test enum parameters
 *
 * enum_header_string_array List Header parameter enum test (string array) (optional)
 * enum_header_string String Header parameter enum test (string) (optional)
 * enum_query_string_array List Query parameter enum test (string array) (optional)
 * enum_query_string String Query parameter enum test (string) (optional)
 * enum_query_integer Integer Query parameter enum test (double) (optional)
 * no response value expected for this operation
 **/
exports.testEnumParameters = function(enum_header_string_array,enum_header_string,enum_query_string_array,enum_query_string,enum_query_integer) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * To test enum parameters
 * To test enum parameters
 *
 * body Body_4  (optional)
 * no response value expected for this operation
 **/
exports.testEnumRequestBody = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * test inline additionalProperties
 *
 * body Map request body
 * no response value expected for this operation
 **/
exports.testInlineAdditionalProperties = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * test json serialization of form data
 *
 * body Body_5 
 * no response value expected for this operation
 **/
exports.testJsonFormData = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}

