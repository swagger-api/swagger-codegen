part of swagger.api;

class OneOfinlineArrayItemsAllPetsResponse {
  
  OneOfinlineArrayItemsAllPetsResponse();

  @override
  String toString() {
    return 'OneOfinlineArrayItemsAllPetsResponse[]';
  }

  OneOfinlineArrayItemsAllPetsResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<OneOfinlineArrayItemsAllPetsResponse> listFromJson(List<dynamic> json) {
    return json == null ? new List<OneOfinlineArrayItemsAllPetsResponse>() : json.map((value) => new OneOfinlineArrayItemsAllPetsResponse.fromJson(value)).toList();
  }

  static Map<String, OneOfinlineArrayItemsAllPetsResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, OneOfinlineArrayItemsAllPetsResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new OneOfinlineArrayItemsAllPetsResponse.fromJson(value));
    }
    return map;
  }
}
