part of swagger.api;

class InlineArrayItemsAllPetsResponse {
  
  InlineArrayItemsAllPetsResponse();

  @override
  String toString() {
    return 'InlineArrayItemsAllPetsResponse[]';
  }

  InlineArrayItemsAllPetsResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<InlineArrayItemsAllPetsResponse> listFromJson(List<dynamic> json) {
    return json == null ? new List<InlineArrayItemsAllPetsResponse>() : json.map((value) => new InlineArrayItemsAllPetsResponse.fromJson(value)).toList();
  }

  static Map<String, InlineArrayItemsAllPetsResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, InlineArrayItemsAllPetsResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new InlineArrayItemsAllPetsResponse.fromJson(value));
    }
    return map;
  }
}
