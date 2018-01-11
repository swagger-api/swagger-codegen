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

  Map<String, dynamic> toMap() {
    return {
      'code': code,
      'type': type,
      'message': message
     };
  }
}

