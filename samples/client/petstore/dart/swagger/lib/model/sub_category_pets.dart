part of swagger.api;

class SubCategoryPets {
  
  int id = null;

  Category category = null;

  String name = null;

  List<String> photoUrls = [];

  List<Tag> tags = [];
/* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };

  SubCategoryPets();

  @override
  String toString() {
    return 'SubCategoryPets[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  SubCategoryPets.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    category = new Category.fromJson(json['category']);
    name = json['name'];
    photoUrls = (json['photoUrls'] as List).map((item) => item as String).toList();
    tags = Tag.listFromJson(json['tags']);
    status = json['status'];
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'category': category,
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags,
      'status': status
     };
  }

  static List<SubCategoryPets> listFromJson(List<dynamic> json) {
    return json == null ? new List<SubCategoryPets>() : json.map((value) => new SubCategoryPets.fromJson(value)).toList();
  }

  static Map<String, SubCategoryPets> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, SubCategoryPets>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new SubCategoryPets.fromJson(value));
    }
    return map;
  }
}
