part of swagger.api;

class Tag {
  
  int id = null;
  

  String name = null;
  
  Tag();

  @override
  String toString() {
    return 'Tag[id=$id, name=$name, ]';
  }

  Tag.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    name = json['name'];
  }

  static List<Tag> listFromJson(Map<String, dynamic> json) {
    var list = new List<Tag>();
    if (json != null && json.length > 0) {
      json.forEach((String key, dynamic value) => list.add(new Tag.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<Tag> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((Tag it) => listResult.add(it.toMap()));
    return listResult;
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name
     };
  }
}

