part of swagger.api;

class OneOfSinglePetResponsePet {
  
  OneOfSinglePetResponsePet();

  @override
  String toString() {
    return 'OneOfSinglePetResponsePet[]';
  }

  OneOfSinglePetResponsePet.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<OneOfSinglePetResponsePet> listFromJson(List<dynamic> json) {
    return json == null ? new List<OneOfSinglePetResponsePet>() : json.map((value) => new OneOfSinglePetResponsePet.fromJson(value)).toList();
  }

  static Map<String, OneOfSinglePetResponsePet> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, OneOfSinglePetResponsePet>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new OneOfSinglePetResponsePet.fromJson(value));
    }
    return map;
  }
}
