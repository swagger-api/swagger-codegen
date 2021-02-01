part of swagger.api;

class AllOfSubCategoryCategory {
  
  int id = null;

  String name = null;

  bool foo = null;

  int bar = null;

  String beer = null;

  User drunk = null;

  AllOfSubCategoryCategory();

  @override
  String toString() {
    return 'AllOfSubCategoryCategory[id=$id, name=$name, foo=$foo, bar=$bar, beer=$beer, drunk=$drunk, ]';
  }

  AllOfSubCategoryCategory.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    name = json['name'];
    foo = json['foo'];
    bar = json['bar'];
    beer = json['beer'];
    drunk = new User.fromJson(json['drunk']);
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
      'foo': foo,
      'bar': bar,
      'beer': beer,
      'drunk': drunk
     };
  }

  static List<AllOfSubCategoryCategory> listFromJson(List<dynamic> json) {
    return json == null ? new List<AllOfSubCategoryCategory>() : json.map((value) => new AllOfSubCategoryCategory.fromJson(value)).toList();
  }

  static Map<String, AllOfSubCategoryCategory> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, AllOfSubCategoryCategory>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new AllOfSubCategoryCategory.fromJson(value));
    }
    return map;
  }
}
