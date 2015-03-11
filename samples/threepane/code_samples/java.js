window.code = window.code || {};

window.code.java = {
  "UserApi": {
    "createUser": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().createUser(body)"
        }
      ]
    },
    "createUsersWithArrayInput": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().createUsersWithArrayInput(body)"
        }
      ]
    },
    "createUsersWithListInput": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().createUsersWithListInput(body)"
        }
      ]
    },
    "loginUser": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().loginUser(username, password)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "&quot;aeiou&quot;"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "string"
        }
        
      ]
    },
    "logoutUser": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().logoutUser()"
        }
      ]
    },
    "getUserByName": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().getUserByName(username)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "{\n  &quot;id&quot; : 123456789,\n  &quot;lastName&quot; : &quot;aeiou&quot;,\n  &quot;phone&quot; : &quot;aeiou&quot;,\n  &quot;username&quot; : &quot;aeiou&quot;,\n  &quot;email&quot; : &quot;aeiou&quot;,\n  &quot;userStatus&quot; : 123,\n  &quot;firstName&quot; : &quot;aeiou&quot;,\n  &quot;password&quot; : &quot;aeiou&quot;\n}"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;User&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;username&gt;string&lt;/username&gt;\n  &lt;firstName&gt;string&lt;/firstName&gt;\n  &lt;lastName&gt;string&lt;/lastName&gt;\n  &lt;email&gt;string&lt;/email&gt;\n  &lt;password&gt;string&lt;/password&gt;\n  &lt;phone&gt;string&lt;/phone&gt;\n  &lt;userStatus&gt;0&lt;/userStatus&gt;\n&lt;/User&gt;"
        }
        
      ]
    },
    "updateUser": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().updateUser(username, body)"
        }
      ]
    },
    "deleteUser": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new UserApi().deleteUser(username)"
        }
      ]
    },
  },
  "PetApi": {
    "updatePet": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().updatePet(body)"
        }
      ]
    },
    "addPet": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().addPet(body)"
        }
      ]
    },
    "findPetsByStatus": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().findPetsByStatus(status)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "[ {\n  &quot;tags&quot; : [ {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  } ],\n  &quot;id&quot; : 123456789,\n  &quot;category&quot; : {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  },\n  &quot;status&quot; : &quot;aeiou&quot;,\n  &quot;name&quot; : &quot;doggie&quot;,\n  &quot;photoUrls&quot; : [ &quot;aeiou&quot; ]\n} ]"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;Pet&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;Category&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Category&gt;\n  &lt;name&gt;doggie&lt;/name&gt;\n  &lt;photoUrls&gt;string&lt;/photoUrls&gt;\n  &lt;Tag&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Tag&gt;\n  &lt;status&gt;string&lt;/status&gt;\n&lt;/Pet&gt;"
        }
        
      ]
    },
    "findPetsByTags": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().findPetsByTags(tags)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "[ {\n  &quot;tags&quot; : [ {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  } ],\n  &quot;id&quot; : 123456789,\n  &quot;category&quot; : {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  },\n  &quot;status&quot; : &quot;aeiou&quot;,\n  &quot;name&quot; : &quot;doggie&quot;,\n  &quot;photoUrls&quot; : [ &quot;aeiou&quot; ]\n} ]"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;Pet&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;Category&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Category&gt;\n  &lt;name&gt;doggie&lt;/name&gt;\n  &lt;photoUrls&gt;string&lt;/photoUrls&gt;\n  &lt;Tag&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Tag&gt;\n  &lt;status&gt;string&lt;/status&gt;\n&lt;/Pet&gt;"
        }
        
      ]
    },
    "getPetById": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().getPetById(petId)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "{\n  &quot;tags&quot; : [ {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  } ],\n  &quot;id&quot; : 123456789,\n  &quot;category&quot; : {\n    &quot;id&quot; : 123456789,\n    &quot;name&quot; : &quot;aeiou&quot;\n  },\n  &quot;status&quot; : &quot;aeiou&quot;,\n  &quot;name&quot; : &quot;doggie&quot;,\n  &quot;photoUrls&quot; : [ &quot;aeiou&quot; ]\n}"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;Pet&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;Category&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Category&gt;\n  &lt;name&gt;doggie&lt;/name&gt;\n  &lt;photoUrls&gt;string&lt;/photoUrls&gt;\n  &lt;Tag&gt;\n    &lt;id&gt;123456&lt;/id&gt;\n    &lt;name&gt;string&lt;/name&gt;\n  &lt;/Tag&gt;\n  &lt;status&gt;string&lt;/status&gt;\n&lt;/Pet&gt;"
        }
        
      ]
    },
    "updatePetWithForm": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().updatePetWithForm(petId, name, status)"
        }
      ]
    },
    "deletePet": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().deletePet(api_key, petId)"
        }
      ]
    },
    "uploadFile": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new PetApi().uploadFile(petId, additionalMetadata, file)"
        }
      ]
    },
  },
  "StoreApi": {
    "getInventory": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new StoreApi().getInventory()"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "{\n  &quot;key&quot; : 123\n}"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": ""
        }
        
      ]
    },
    "placeOrder": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new StoreApi().placeOrder(body)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "{\n  &quot;id&quot; : 123456789,\n  &quot;petId&quot; : 123456789,\n  &quot;complete&quot; : true,\n  &quot;status&quot; : &quot;aeiou&quot;,\n  &quot;quantity&quot; : 123,\n  &quot;shipDate&quot; : &quot;2015-03-11T15:29:25.659+0000&quot;\n}"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;Order&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;petId&gt;123456&lt;/petId&gt;\n  &lt;quantity&gt;0&lt;/quantity&gt;\n  &lt;shipDate&gt;2015-03-11T08:29:25.663Z&lt;/shipDate&gt;\n  &lt;status&gt;string&lt;/status&gt;\n  &lt;complete&gt;true&lt;/complete&gt;\n&lt;/Order&gt;"
        }
        
      ]
    },
    "getOrderById": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new StoreApi().getOrderById(orderId)"
        }
        , {
          "header": "response: application/json",
          "syntax": "java",
          "content": "{\n  &quot;id&quot; : 123456789,\n  &quot;petId&quot; : 123456789,\n  &quot;complete&quot; : true,\n  &quot;status&quot; : &quot;aeiou&quot;,\n  &quot;quantity&quot; : 123,\n  &quot;shipDate&quot; : &quot;2015-03-11T15:29:25.663+0000&quot;\n}"
        }
        
        , {
          "header": "response: application/xml",
          "syntax": "java",
          "content": "&lt;Order&gt;\n  &lt;id&gt;123456&lt;/id&gt;\n  &lt;petId&gt;123456&lt;/petId&gt;\n  &lt;quantity&gt;0&lt;/quantity&gt;\n  &lt;shipDate&gt;2015-03-11T08:29:25.663Z&lt;/shipDate&gt;\n  &lt;status&gt;string&lt;/status&gt;\n  &lt;complete&gt;true&lt;/complete&gt;\n&lt;/Order&gt;"
        }
        
      ]
    },
    "deleteOrder": {
      "java": [
        {
          "header": "request",
          "syntax": "java",
          "content": "new StoreApi().deleteOrder(orderId)"
        }
      ]
    },
  },
}
