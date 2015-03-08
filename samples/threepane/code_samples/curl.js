window.code = window.code || {};

window.code.curl = {
  "UserApi": {
    "createUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/user\'"
        }
      ]
    },
    "createUsersWithArrayInput": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/user/createWithArray\'"
        }
      ]
    },
    "createUsersWithListInput": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/user/createWithList\'"
        }
      ]
    },
    "loginUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/user/login?[username={username}],[password={password}]\'"
        }
      ]
    },
    "logoutUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/user/logout\'"
        }
      ]
    },
    "getUserByName": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/user/{username}\'"
        }
      ]
    },
    "updateUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X put \'http://petstore.swagger.io/v2/user/{username}\'"
        }
      ]
    },
    "deleteUser": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X delete \'http://petstore.swagger.io/v2/user/{username}\'"
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
          "content": "curl -X put \'http://petstore.swagger.io/v2/pet\'"
        }
      ]
    },
    "addPet": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/pet\'"
        }
      ]
    },
    "findPetsByStatus": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/pet/findByStatus?[status={status1&amp;status=status2}]\'"
        }
      ]
    },
    "findPetsByTags": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/pet/findByTags?[tags={tags1&amp;tags=tags2}]\'"
        }
      ]
    },
    "getPetById": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/pet/{petId}\'"
        }
      ]
    },
    "updatePetWithForm": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/pet/{petId}\'"
        }
      ]
    },
    "deletePet": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X delete \'http://petstore.swagger.io/v2/pet/{petId}\'\ \n  -H 'api_key:{ api_key }'"
        }
      ]
    },
    "uploadFile": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/pet/{petId}/uploadImage\'"
        }
      ]
    },
  },
  "StoreApi": {
    "getInventory": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/store/inventory\'"
        }
      ]
    },
    "placeOrder": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X post \'http://petstore.swagger.io/v2/store/order\'"
        }
      ]
    },
    "getOrderById": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X get \'http://petstore.swagger.io/v2/store/order/{orderId}\'"
        }
      ]
    },
    "deleteOrder": {
      "curl": [
        {
          "header": "request",
          "syntax": "bash",
          "content": "curl -X delete \'http://petstore.swagger.io/v2/store/order/{orderId}\'"
        }
      ]
    },
  },
}
