'use strict';


/**
 * To test class name in snake case
 *
 * body Client client model
 * returns Client
 **/
exports.testClassname = function(body) {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = {
  "client" : "client"
};
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}

