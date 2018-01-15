part of swagger.api;

class Pet {
  
  int id = null;
  

  Category category = null;
  

  String name = null;
  

  List<String> photoUrls = [];
  

  List<Tag> tags = [];
  
/* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };
  Pet();

  @override
  String toString() {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Pet.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    category = new Category.fromJson(json['category']);
    name = json['name'];
    photoUrls = json['photoUrls'];
    tags = Tag.listFromJson(json['tags']);
    status = json['status'];
  }

  static List<Pet> listFromJson(Map<String, dynamic> json) {
    var list = new List<Pet>();
    if (json != null && json.length > 0) {
      json.forEach((String key, dynamic value) => list.add(new Pet.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<Pet> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((Pet it) => listResult.add(it.toMap()));
    return listResult;
  }


  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'category': category == null ? null : category.toMap(),
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags == null ? null : Tag.toMapList(tags),
      'status': status
     };
  }
}

