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
    shipDate = new DateTime.fromJson(json['shipDate']);
    status = json['status'];
    complete = json['complete'];
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'petId': petId,
      'quantity': quantity,
      'shipDate': shipDate == null ? null : shipDate.toMap(),
      'status': status,
      'complete': complete
     };
  }
}

