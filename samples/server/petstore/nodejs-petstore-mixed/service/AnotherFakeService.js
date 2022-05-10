'use strict';


/**
 * To test special tags
 * To test special tags
 *
 * body Client client model
 * returns Client
 **/
exports.test_special_tags = function(body) {
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

