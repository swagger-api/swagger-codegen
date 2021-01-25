import 'package:swagger/api.dart';
import 'package:test/test.dart';


/// tests for StoreApi
void main() {
  var instance = new StoreApi();

  group('tests for StoreApi', () {
    // Delete purchase order by ID
    //
    // For valid response try integer IDs with positive integer value.\\ \\ Negative or non-integer values will generate API errors
    //
    //Future deleteOrder(int orderId) async
    test('test deleteOrder', () async {
      // TODO
    });

    // Returns pet inventories by status
    //
    // Returns a map of status codes to quantities
    //
    //Future<Map<String, int>> getInventory() async
    test('test getInventory', () async {
      // TODO
    });

    // Find purchase order by ID
    //
    // For valid response try integer IDs with value >= 1 and <= 10.\\ \\ Other values will generated exceptions
    //
    //Future<Order> getOrderById(int orderId) async
    test('test getOrderById', () async {
      // TODO
    });

    // Place an order for a pet
    //
    //Future<Order> placeOrder(Order body) async
    test('test placeOrder', () async {
      // TODO
    });

  });
}
