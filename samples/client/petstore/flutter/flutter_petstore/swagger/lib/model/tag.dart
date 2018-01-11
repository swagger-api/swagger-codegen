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

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name
     };
  }
}

