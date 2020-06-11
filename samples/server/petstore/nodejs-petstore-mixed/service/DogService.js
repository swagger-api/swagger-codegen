'use strict';


/**
 * Add a new dog to the store
 *
 * body Dog Dog object that needs to be added to the store
 * no response value expected for this operation
 **/
exports.addDog = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Deletes a dog
 *
 * dogId Long Dog id to delete
 * api_key String  (optional)
 * no response value expected for this operation
 **/
exports.deleteDog = function(dogId,api_key) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Find dog by ID
 * Returns a single dog
 *
 * dogId Long ID of dog to return
 * returns Dog
 **/
exports.getDogById = function(dogId) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = "";
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}


/**
 * Update an existing dog
 *
 * body Dog Dog object that needs to be added.
 * no response value expected for this operation
 **/
exports.updateDog = function(body) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}


/**
 * Updates a dog
 *
 * name String  (optional)
 * status String  (optional)
 * dogId Long ID of dog that needs to be updated
 * no response value expected for this operation
 **/
exports.updateDogWithForm = function(name,status,dogId) {
  return new Promise(function(resolve, reject) {
    resolve();
  });
}

