part of swagger.api;

class Category {
  
  int id = null;
  

  String name = null;
  
  Category();

  @override
  String toString() {
    return 'Category[id=$id, name=$name, ]';
  }

  Category.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    name = json['name'];
  }

  static List<Category> listFromJson(Map<String, dynamic> json) {
    var list = new List<Category>();
    if (json != null && json.length > 0) {
      json.forEach((String key, dynamic value) => list.add(new Category.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<Category> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((Category it) => listResult.add(it.toMap()));
    return listResult;
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name
     };
  }
}

