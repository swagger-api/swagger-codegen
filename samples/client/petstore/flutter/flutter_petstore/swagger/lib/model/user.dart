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

