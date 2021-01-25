part of swagger.api;

class Cat {
  
  bool hunts = null;

  int age = null;

  int id = null;

  Category category = null;

  String name = null;

  List<String> photoUrls = [];

  List<Tag> tags = [];
/* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };

  Cat();

  @override
  String toString() {
    return 'Cat[hunts=$hunts, age=$age, id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Cat.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    hunts = json['hunts'];
    age = json['age'];
    id = json['id'];
    category = new Category.fromJson(json['category']);
    name = json['name'];
    photoUrls = (json['photoUrls'] as List).map((item) => item as String).toList();
    tags = Tag.listFromJson(json['tags']);
    status = json['status'];
  }

  Map<String, dynamic> toJson() {
    return {
      'hunts': hunts,
      'age': age,
      'id': id,
      'category': category,
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags,
      'status': status
     };
  }

  static List<Cat> listFromJson(List<dynamic> json) {
    return json == null ? new List<Cat>() : json.map((value) => new Cat.fromJson(value)).toList();
  }

  static Map<String, Cat> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Cat>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Cat.fromJson(value));
    }
    return map;
  }
}
