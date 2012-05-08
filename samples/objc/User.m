#import "User.h"

@implementation User

@synthesize _id = __id;
@synthesize lastName = _lastName;
@synthesize username = _username;
@synthesize phone = _phone;
@synthesize email = _email;
@synthesize userStatus = _userStatus;
@synthesize firstName = _firstName;
@synthesize password = _password;
- (id) _id: (NSNumber*) _id
       lastName: (NSString*) lastName
       username: (NSString*) username
       phone: (NSString*) phone
       email: (NSString*) email
       userStatus: (NSNumber*) userStatus
       firstName: (NSString*) firstName
       password: (NSString*) password
       {
          __id = _id;
          _lastName = lastName;
          _username = username;
          _phone = phone;
          _email = email;
          _userStatus = userStatus;
          _firstName = firstName;
          _password = password;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _lastName = [dict objectForKey:@"lastName"];
    _username = [dict objectForKey:@"username"];
    _phone = [dict objectForKey:@"phone"];
    _email = [dict objectForKey:@"email"];
    _userStatus = [dict objectForKey:@"userStatus"];
    _firstName = [dict objectForKey:@"firstName"];
    _password = [dict objectForKey:@"password"];
    return self;
}
@end

