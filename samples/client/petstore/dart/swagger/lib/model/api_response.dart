part of swagger.api;

class ApiResponse {
  
  int code = null;
  

  String type = null;
  

  String message = null;
  
  ApiResponse();

  @override
  String toString() {
    return 'ApiResponse[code=$code, type=$type, message=$message, ]';
  }

  ApiResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    code = json['code'];
    type = json['type'];
    message = json['message'];
  }

  static List<ApiResponse> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<ApiResponse>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new ApiResponse.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<ApiResponse> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((ApiResponse it) => listResult.add(it.toMap()));
    return listResult;
  }

  Map<String, dynamic> toMap() {
    return {
      'code': code,
      'type': type,
      'message': message
     };
  }
}

