'use strict';

var utils = require('../utils/writer.js');
var Fake = require('../service/FakeService');

module.exports.fakeOuterBooleanSerialize = function fakeOuterBooleanSerialize (req, res, next, body) {
  Fake.fakeOuterBooleanSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterCompositeSerialize = function fakeOuterCompositeSerialize (req, res, next, body) {
  Fake.fakeOuterCompositeSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterNumberSerialize = function fakeOuterNumberSerialize (req, res, next, body) {
  Fake.fakeOuterNumberSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.fakeOuterStringSerialize = function fakeOuterStringSerialize (req, res, next, body) {
  Fake.fakeOuterStringSerialize(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testClientModel = function testClientModel (req, res, next, body) {
  Fake.testClientModel(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEndpointParameters = function testEndpointParameters (req, res, next, body) {
  Fake.testEndpointParameters(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEnumParameters = function testEnumParameters (req, res, next, enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer) {
  Fake.testEnumParameters(enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testEnumRequestBody = function testEnumRequestBody (req, res, next, body) {
  Fake.testEnumRequestBody(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testInlineAdditionalProperties = function testInlineAdditionalProperties (req, res, next, body) {
  Fake.testInlineAdditionalProperties(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.testJsonFormData = function testJsonFormData (req, res, next, body) {
  Fake.testJsonFormData(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
