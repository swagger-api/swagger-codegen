part of swagger.api;

class Amount {
  /* some description  */
  double value = null;
   // range from 0.01 to 1000000000000000//

  Currency currency = null;
  
  Amount();

  @override
  String toString() {
    return 'Amount[value=$value, currency=$currency, ]';
  }

  Amount.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    value =
        json['value'] == null ? null : json['value'].toDouble()
    ;
    currency =
      
      
      new Currency.fromJson(json['currency'])
;
  }

  Map<String, dynamic> toJson() {
    return {
      'value': value,
      'currency': currency
     };
  }

  static List<Amount> listFromJson(List<dynamic> json) {
    return json == null ? new List<Amount>() : json.map((value) => new Amount.fromJson(value)).toList();
  }

  static Map<String, Amount> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Amount>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Amount.fromJson(value));
    }
    return map;
  }
}

