part of swagger.api;

class DogAllOf2 {
  
  bool bark = null;

  String breed = null;
  //enum breedEnum {  Dingo,  Husky,  Retriever,  Shepherd,  };

  DogAllOf2();

  @override
  String toString() {
    return 'DogAllOf2[bark=$bark, breed=$breed, ]';
  }

  DogAllOf2.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    bark = json['bark'];
    breed = json['breed'];
  }

  Map<String, dynamic> toJson() {
    return {
      'bark': bark,
      'breed': breed
     };
  }

  static List<DogAllOf2> listFromJson(List<dynamic> json) {
    return json == null ? new List<DogAllOf2>() : json.map((value) => new DogAllOf2.fromJson(value)).toList();
  }

  static Map<String, DogAllOf2> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, DogAllOf2>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new DogAllOf2.fromJson(value));
    }
    return map;
  }
}
