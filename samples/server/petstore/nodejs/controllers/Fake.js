'use strict';

var utils = require('../utils/writer.js');
var Fake = require('../service/FakeService');

module.exports.fakeOuterBooleanSerialize = function fakeOuterBooleanSerialize (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.fakeOuterBooleanSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterCompositeSerialize = function fakeOuterCompositeSerialize (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.fakeOuterCompositeSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterNumberSerialize = function fakeOuterNumberSerialize (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.fakeOuterNumberSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterStringSerialize = function fakeOuterStringSerialize (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.fakeOuterStringSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testClientModel = function testClientModel (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.testClientModel(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEndpointParameters = function testEndpointParameters (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.testEndpointParameters(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEnumParameters = function testEnumParameters (req, res, next) {
  var enum_header_string_array = req.swagger.params['enum_header_string_array'].value;
  var enum_header_string = req.swagger.params['enum_header_string'].value;
  var enum_query_string_array = req.swagger.params['enum_query_string_array'].value;
  var enum_query_string = req.swagger.params['enum_query_string'].value;
  var enum_query_integer = req.swagger.params['enum_query_integer'].value;
  Fake.testEnumParameters(enum_header_string_array,enum_header_string,enum_query_string_array,enum_query_string,enum_query_integer)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEnumRequestBody = function testEnumRequestBody (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.testEnumRequestBody(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testInlineAdditionalProperties = function testInlineAdditionalProperties (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.testInlineAdditionalProperties(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testJsonFormData = function testJsonFormData (req, res, next) {
  var body = req.swagger.params['body'].value;
  Fake.testJsonFormData(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
