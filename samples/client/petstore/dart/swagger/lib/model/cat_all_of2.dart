part of swagger.api;

class CatAllOf2 {
  
  bool hunts = null;

  int age = null;

  CatAllOf2();

  @override
  String toString() {
    return 'CatAllOf2[hunts=$hunts, age=$age, ]';
  }

  CatAllOf2.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    hunts = json['hunts'];
    age = json['age'];
  }

  Map<String, dynamic> toJson() {
    return {
      'hunts': hunts,
      'age': age
     };
  }

  static List<CatAllOf2> listFromJson(List<dynamic> json) {
    return json == null ? new List<CatAllOf2>() : json.map((value) => new CatAllOf2.fromJson(value)).toList();
  }

  static Map<String, CatAllOf2> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, CatAllOf2>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new CatAllOf2.fromJson(value));
    }
    return map;
  }
}
