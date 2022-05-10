'use strict';

var utils = require('../utils/writer.js');
var Master = require('../service/MasterService');

module.exports.masterOperation = function masterOperation (req, res, next) {
  Master.masterOperation()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
