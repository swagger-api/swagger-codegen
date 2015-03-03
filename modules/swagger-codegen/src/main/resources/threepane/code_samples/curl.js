/* generated for the 'petstore' spec file */
// Create the parnet object if it doesn't exist...
window.code = window.code || {};


// The schema for each language file is as follows...
// {
//   <apiClassName>: {

//     <operationId>: {
//       <language>: [
//         {
//           "header": <string> // THe header displayed, possibly "request", "response",
//           "syntax": <string> // see https://highlightjs.org/static/demo/ for the supported syntaxes...
//                            // ...although we would need to make a custom build as the current one only covers the basic syntaxes
//           "content": <code sample> //escaped for JSON and formated for a <pre> block
//                                  // ... ie: it needs \n's instead of <br>'s
//         },
//         {
//           "header": <string>
//           "syntax": <string>
//           "content": <code sample>
//         }
//         ...
//       ]
//     },

//     <operationId> : {...}

//   },

//   <apiClassName> { ... }

// }


// Here is a live version, that is filled with dummy content

window.code.curl = {

  "UserApi": {
    "createUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash", // see segmentio/highlight for a list of supported syntaxes
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "createUsersWithArrayInput": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "createUsersWithListInput": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "loginUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "logoutUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "getUserByName": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "updateUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "deleteUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
  },

  "PetApi": {
    "updatePet": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "addPet": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "findPetsByStatus": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "findPetsByTags": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "getPetById": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "updatePetWithForm": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "deletePet": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
  },

  "StoreApi": {
    "placeOrder": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "getOrderById": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
    "deleteOrder": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl 'http://example.com/some/path' \\\n  -d bob=\"hello\" \\\n  -X GET"
        }
      ]
    },
  }
}
