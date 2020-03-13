'use strict';

var utils = require('../utils/writer.js');
var FakeClassnameTags123 = require('../service/FakeClassnameTags123Service');

module.exports.testClassname = function testClassname (req, res, next, body) {
  FakeClassnameTags123.testClassname(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
