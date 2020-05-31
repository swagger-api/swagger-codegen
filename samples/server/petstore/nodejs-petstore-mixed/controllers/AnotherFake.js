'use strict';

var utils = require('../utils/writer.js');
var AnotherFake = require('../service/AnotherFakeService');

module.exports.test_special_tags = function test_special_tags (req, res, next, body) {
  AnotherFake.test_special_tags(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
