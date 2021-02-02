part of swagger.api;

class SinglePetResponse {
  
  OneOfSinglePetResponsePet pet = null;

  SinglePetResponse();

  @override
  String toString() {
    return 'SinglePetResponse[pet=$pet, ]';
  }

  SinglePetResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    pet = new OneOfSinglePetResponsePet.fromJson(json['pet']);
  }

  Map<String, dynamic> toJson() {
    return {
      'pet': pet
     };
  }

  static List<SinglePetResponse> listFromJson(List<dynamic> json) {
    return json == null ? new List<SinglePetResponse>() : json.map((value) => new SinglePetResponse.fromJson(value)).toList();
  }

  static Map<String, SinglePetResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, SinglePetResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new SinglePetResponse.fromJson(value));
    }
    return map;
  }
}
