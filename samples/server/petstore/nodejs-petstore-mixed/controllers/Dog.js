'use strict';

var utils = require('../utils/writer.js');
var Dog = require('../service/DogService');

module.exports.addDog = function addDog (req, res, next, body) {
  Dog.addDog(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.deleteDog = function deleteDog (req, res, next, dogId, api_key) {
  Dog.deleteDog(dogId, api_key)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getDogById = function getDogById (req, res, next, dogId) {
  Dog.getDogById(dogId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updateDog = function updateDog (req, res, next, body) {
  Dog.updateDog(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updateDogWithForm = function updateDogWithForm (req, res, next, name, status, dogId) {
  Dog.updateDogWithForm(name, status, dogId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
