part of swagger.api;

class Dog {
  
  bool bark = null;

  String breed = null;
  //enum breedEnum {  Dingo,  Husky,  Retriever,  Shepherd,  };

  int id = null;

  Category category = null;

  String name = null;

  List<String> photoUrls = [];

  List<Tag> tags = [];
/* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };

  Dog();

  @override
  String toString() {
    return 'Dog[bark=$bark, breed=$breed, id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Dog.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    bark = json['bark'];
    breed = json['breed'];
    id = json['id'];
    category = new Category.fromJson(json['category']);
    name = json['name'];
    photoUrls = (json['photoUrls'] as List).map((item) => item as String).toList();
    tags = Tag.listFromJson(json['tags']);
    status = json['status'];
  }

  Map<String, dynamic> toJson() {
    return {
      'bark': bark,
      'breed': breed,
      'id': id,
      'category': category,
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags,
      'status': status
     };
  }

  static List<Dog> listFromJson(List<dynamic> json) {
    return json == null ? new List<Dog>() : json.map((value) => new Dog.fromJson(value)).toList();
  }

  static Map<String, Dog> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Dog>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Dog.fromJson(value));
    }
    return map;
  }
}
