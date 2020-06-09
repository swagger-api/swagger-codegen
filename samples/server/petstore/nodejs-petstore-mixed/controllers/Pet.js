'use strict';

var utils = require('../utils/writer.js');
var Pet = require('../service/PetService');

module.exports.addPet = function addPet (req, res, next, body) {
  Pet.addPet(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.deletePet = function deletePet (req, res, next, petId, api_key) {
  Pet.deletePet(petId, api_key)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.doCategoryStuff = function doCategoryStuff (req, res, next, body) {
  Pet.doCategoryStuff(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.findPetsByStatus = function findPetsByStatus (req, res, next, status) {
  Pet.findPetsByStatus(status)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.findPetsByTags = function findPetsByTags (req, res, next, tags) {
  Pet.findPetsByTags(tags)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getAllPets = function getAllPets (req, res, next) {
  Pet.getAllPets()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getPetById = function getPetById (req, res, next, petId) {
  Pet.getPetById(petId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.getRandomPet = function getRandomPet (req, res, next) {
  Pet.getRandomPet()
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updatePet = function updatePet (req, res, next, body) {
  Pet.updatePet(body)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.updatePetWithForm = function updatePetWithForm (req, res, next, name, status, petId) {
  Pet.updatePetWithForm(name, status, petId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};

module.exports.uploadFile = function uploadFile (req, res, next, additionalMetadata, file, petId) {
  Pet.uploadFile(additionalMetadata, file, petId)
    .then(function (response) {
      utils.writeJson(res, response);
    })
    .catch(function (response) {
      utils.writeJson(res, response);
    });
};
