part of swagger.api;

class SubCategory {
  
  AllOfSubCategoryCategory category = null;

  Category category2 = null;

  List<SubCategoryPets> pets = [];

  SubCategory();

  @override
  String toString() {
    return 'SubCategory[category=$category, category2=$category2, pets=$pets, ]';
  }

  SubCategory.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    category = new AllOfSubCategoryCategory.fromJson(json['category']);
    category2 = new Category.fromJson(json['category2']);
    pets = SubCategoryPets.listFromJson(json['pets']);
  }

  Map<String, dynamic> toJson() {
    return {
      'category': category,
      'category2': category2,
      'pets': pets
     };
  }

  static List<SubCategory> listFromJson(List<dynamic> json) {
    return json == null ? new List<SubCategory>() : json.map((value) => new SubCategory.fromJson(value)).toList();
  }

  static Map<String, SubCategory> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, SubCategory>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new SubCategory.fromJson(value));
    }
    return map;
  }
}
