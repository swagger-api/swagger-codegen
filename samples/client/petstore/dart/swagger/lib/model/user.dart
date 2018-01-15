part of swagger.api;

class User {
  
  int id = null;
  

  String username = null;
  

  String firstName = null;
  

  String lastName = null;
  

  String email = null;
  

  String password = null;
  

  String phone = null;
  
/* User Status */
  int userStatus = null;
  
  User();

  @override
  String toString() {
    return 'User[id=$id, username=$username, firstName=$firstName, lastName=$lastName, email=$email, password=$password, phone=$phone, userStatus=$userStatus, ]';
  }

  User.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    username = json['username'];
    firstName = json['firstName'];
    lastName = json['lastName'];
    email = json['email'];
    password = json['password'];
    phone = json['phone'];
    userStatus = json['userStatus'];
  }

  static List<User> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<User>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new User.fromJson(value)));
    }
    return list;
  }

  static List<Map<String, dynamic>> toMapList(List<User> list) {
    var listResult = new List<Map<String, dynamic>>();
    list.forEach((User it) => listResult.add(it.toMap()));
    return listResult;
  }

  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'username': username,
      'firstName': firstName,
      'lastName': lastName,
      'email': email,
      'password': password,
      'phone': phone,
      'userStatus': userStatus
     };
  }
}

