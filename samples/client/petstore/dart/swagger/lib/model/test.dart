part of swagger.api;

class Test {
  
  Test();

  @override
  String toString() {
    return 'Test[]';
  }

  Test.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<Test> listFromJson(List<dynamic> json) {
    return json == null ? new List<Test>() : json.map((value) => new Test.fromJson(value)).toList();
  }

  static Map<String, Test> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Test>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Test.fromJson(value));
    }
    return map;
  }
}
