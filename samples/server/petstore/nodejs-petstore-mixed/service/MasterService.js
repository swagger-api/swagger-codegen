'use strict';


/**
 *
 * returns List
 **/
exports.masterOperation = function() {
  return new Promise(function(resolve, reject) {
    var examples = {};
    examples['application/json'] = [ {
  "origin" : "",
  "destination" : ""
}, {
  "origin" : "",
  "destination" : ""
} ];
    if (Object.keys(examples).length > 0) {
      resolve(examples[Object.keys(examples)[0]]);
    } else {
      resolve();
    }
  });
}

