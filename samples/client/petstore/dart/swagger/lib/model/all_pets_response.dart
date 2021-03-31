part of swagger.api;

class AllPetsResponse {
  
  AllPetsResponse();

  @override
  String toString() {
    return 'AllPetsResponse[]';
  }

  AllPetsResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<AllPetsResponse> listFromJson(List<dynamic> json) {
    return json == null ? new List<AllPetsResponse>() : json.map((value) => new AllPetsResponse.fromJson(value)).toList();
  }

  static Map<String, AllPetsResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, AllPetsResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new AllPetsResponse.fromJson(value));
    }
    return map;
  }
}
