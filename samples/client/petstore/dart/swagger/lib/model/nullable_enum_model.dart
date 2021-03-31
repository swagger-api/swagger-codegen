part of swagger.api;

class NullableEnumModel {
  
  String enumProp = null;
  //enum enumPropEnum {  a,  b,  ,  };

  NullableEnumModel();

  @override
  String toString() {
    return 'NullableEnumModel[enumProp=$enumProp, ]';
  }

  NullableEnumModel.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    enumProp = json['enumProp'];
  }

  Map<String, dynamic> toJson() {
    return {
      'enumProp': enumProp
     };
  }

  static List<NullableEnumModel> listFromJson(List<dynamic> json) {
    return json == null ? new List<NullableEnumModel>() : json.map((value) => new NullableEnumModel.fromJson(value)).toList();
  }

  static Map<String, NullableEnumModel> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, NullableEnumModel>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new NullableEnumModel.fromJson(value));
    }
    return map;
  }
}
