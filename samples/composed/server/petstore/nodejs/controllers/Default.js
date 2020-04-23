'use strict';

var utils = require('../utils/writer.js');
var Default = require('../service/DefaultService');

module.exports.testMethod = function testMethod (req, res, next) {
  Default.testMethod()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
