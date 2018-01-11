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
    tags = new Tag.fromJson(json['tags']);
    status = json['status'];
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'category': category == null ? null : category.toMap(),
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags == null ? null : tags.toMap(),
      'status': status
     };
  }
}

