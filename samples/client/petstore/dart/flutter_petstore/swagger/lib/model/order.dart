part of swagger.api;

class Order {
  
  int id = null;
  

  int petId = null;
  

  int quantity = null;
  

  DateTime shipDate = null;
  
/* Order Status */
  String status = null;
  //enum statusEnum {  placed,  approved,  delivered,  };

  bool complete = null;
  
  Order();

  @override
  String toString() {
    return 'Order[id=$id, petId=$petId, quantity=$quantity, shipDate=$shipDate, status=$status, complete=$complete, ]';
  }

  Order.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    petId = json['petId'];
    quantity = json['quantity'];
    shipDate = json['shipDate'] == null ? null : DateTime.parse(json['shipDate']);
    status = json['status'];
    complete = json['complete'];
  }

  static List<Order> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<Order>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new Order.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<Order> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((Order it) => listResult.add(it.toMap()));
    return listResult;
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'petId': petId,
      'quantity': quantity,
      'shipDate': shipDate == null ? '' : shipDate.toUtc().toIso8601String(),
      'status': status,
      'complete': complete
     };
  }
}

