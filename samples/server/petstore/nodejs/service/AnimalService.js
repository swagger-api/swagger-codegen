'use strict';


/**
 * Add a new animal to the store
 *
 * body Animal Animal object that needs to be added to the store
 * no response value expected for this operation
 **/
exports.addAnimal = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Deletes a animal
 *
 * animalId Long Animal id to delete
 * api_key String  (optional)
 * no response value expected for this operation
 **/
exports.deleteAnimal = function(animalId,api_key) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Find animal by ID
 * Returns a single animal
 *
 * animalId Long ID of pet to return
 * returns Animal
 **/
exports.getAnimalById = function(animalId) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = {
  "color" : "red",
  "className" : "className"
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Update an existing animal
 *
 * body Animal Animal object that needs to be added.
 * no response value expected for this operation
 **/
exports.updateAnimal = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Updates a animal
 *
 * name String  (optional)
 * status String  (optional)
 * animalId Long ID of animal that needs to be updated
 * no response value expected for this operation
 **/
exports.updateAnimalWithForm = function(name,status,animalId) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}

