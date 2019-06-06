part of swagger.api;

class Currency {
    Currency();

  @override
  String toString() {
    return 'Currency[]';
  }

  Currency.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
  }

  Map<String, dynamic> toJson() {
    return {
     };
  }

  static List<Currency> listFromJson(List<dynamic> json) {
    return json == null ? new List<Currency>() : json.map((value) => new Currency.fromJson(value)).toList();
  }

  static Map<String, Currency> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Currency>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Currency.fromJson(value));
    }
    return map;
  }
}

