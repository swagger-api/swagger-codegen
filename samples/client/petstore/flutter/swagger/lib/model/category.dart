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

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name
     };
  }
}

