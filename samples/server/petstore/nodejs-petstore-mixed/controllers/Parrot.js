'use strict';

var utils = require('../utils/writer.js');
var Parrot = require('../service/ParrotService');

module.exports.addParrot = function addParrot (req, res, next, body) {
  Parrot.addParrot(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getParrots = function getParrots (req, res, next) {
  Parrot.getParrots()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updateParrots = function updateParrots (req, res, next, body) {
  Parrot.updateParrots(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
