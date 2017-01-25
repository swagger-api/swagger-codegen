"use strict";
var api = require('./api');
var fs = require('fs');
function deepCheck(objectA, objectB) {
    var a = objectA;
    var b = objectB;
    if (a instanceof Array && b instanceof Array) {
        for (var i = 0; i < a.length; i++) {
            if (!deepCheck(a[i], b[i])) {
                return false;
            }
        }
        return true;
    }
    else if ((typeof a == "string" && typeof b == "string") || (typeof a == "boolean" && typeof b == "boolean") || (typeof a == "number" && typeof b == "number")) {
        return a === b;
    }
    else if (typeof a == "object" && typeof b == "object") {
        for (var key in a) {
            if (!deepCheck(a[key], b[key])) {
                return false;
            }
        }
        return true;
    }
    else {
        return a === b;
    }
}
var petApi = new api.PetApi();
petApi.setApiKey(api.PetApiApiKeys.api_key, 'special-key');
var tag1 = new api.Tag();
tag1.id = 18291;
tag1.name = 'TS tag 1';
var pet = new api.Pet();
pet.name = 'TypeScriptDoggie';
pet.id = 18291;
pet.photoUrls = ["http://url1", "http://url2"];
pet.tags = [tag1];
var petId;
var exitCode = 0;
var rewire = require("rewire");
var rewiredApi = rewire("./api");
var objectSerializer = rewiredApi.__get__("ObjectSerializer");
var type = "Pet";
var serializedData = {
    "id": pet.id,
    "category": {
        "id": 18291,
        "name": "TS category 1"
    },
    "name": pet.name,
    "photoUrls": pet.photoUrls,
    "tags": [
        {
            "id": 18291,
            "name": "TS tag 1"
        }
    ],
    "status": "available"
};
var deserializedPet = objectSerializer.deserialize(serializedData, "Pet");
console.log(deserializedPet);
var petType = deserializedPet instanceof rewiredApi.Pet;
var tagType1 = deserializedPet.tags[0] instanceof rewiredApi.Tag;
var categoryType = deserializedPet.category instanceof rewiredApi.Category;
var checks = {};
for (var key in deserializedPet) {
    checks[key] = {};
    checks[key]["isCorrect"] = deepCheck(deserializedPet[key], serializedData[key]);
    checks[key]["is"] = deserializedPet[key];
    checks[key]["should"] = serializedData[key];
}
var correctTypes = petType && tagType1 && categoryType;
if (!correctTypes) {
    exitCode = 1;
    console.log("PetType correct: ", petType);
    console.log("TagType1 correct: ", tagType1);
    console.log("CategoryType correct: ", categoryType);
}
for (var key in checks) {
    var check = checks[key];
    if (!check["isCorrect"]) {
        exitCode = 1;
        console.log("Incorrect ", key, ": ", check["isCorrect"], ";is: ", check["is"], ";should: ", check["should"]);
    }
}
var reserializedData = objectSerializer.serialize(deserializedPet, "Pet");
if (!deepCheck(reserializedData, serializedData)) {
    exitCode = 1;
    console.log("Reserialized Data incorrect! is: ", reserializedData, "should: ", serializedData);
}
petApi.addPet(pet)
    .then(function (res) {
    var newPet = res.body;
    petId = newPet.id;
    console.log("Created pet with ID " + petId);
    newPet.status = api.Pet.StatusEnum.Available;
    return petApi.updatePet(newPet);
})
    .then(function (res) {
    console.log('Updated pet using POST body');
    return petApi.updatePetWithForm(petId, undefined, "pending");
})
    .then(function (res) {
    console.log('Updated pet using POST form');
    return petApi.uploadFile(petId, undefined, fs.createReadStream('sample.png'));
})
    .then(function (res) {
    console.log('Uploaded image');
    return petApi.getPetById(petId);
})
    .then(function (res) {
    console.log('Got pet by ID: ' + JSON.stringify(res.body));
    console.log("EnumValue: ", api.Pet.StatusEnum.Pending);
    console.log("Typeof EnumValue:", typeof api.Pet.StatusEnum.Pending);
    console.log("Res:", res.body.status);
    if (res.body.status != api.Pet.StatusEnum.Pending) {
        throw new Error("Unexpected pet status");
    }
})
    .catch(function (err) {
    console.error(err);
    exitCode = 1;
})
    .then(function () {
    return petApi.deletePet(petId);
})
    .then(function (res) {
    console.log('Deleted pet');
    process.exit(exitCode);
});
//# sourceMappingURL=client.js.map