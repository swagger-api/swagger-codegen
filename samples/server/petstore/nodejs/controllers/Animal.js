'use strict';

var utils = require('../utils/writer.js');
var Animal = require('../service/AnimalService');

module.exports.addAnimal = function addAnimal (req, res, next, body) {
  Animal.addAnimal(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.deleteAnimal = function deleteAnimal (req, res, next, animalId, api_key) {
  Animal.deleteAnimal(animalId, api_key)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getAnimalById = function getAnimalById (req, res, next, animalId) {
  Animal.getAnimalById(animalId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updateAnimal = function updateAnimal (req, res, next, body) {
  Animal.updateAnimal(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updateAnimalWithForm = function updateAnimalWithForm (req, res, next, name, status, animalId) {
  Animal.updateAnimalWithForm(name, status, animalId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
