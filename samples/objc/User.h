#import <Foundation/Foundation.h>

@interface User : NSObject {
@private
    NSNumber* __id;
    NSString* _lastName;
    NSString* _username;
    NSString* _phone;
    NSString* _email;
    NSNumber* _userStatus;
    NSString* _firstName;
    NSString* _password;
    }


@property(nonatomic, readonly) NSNumber* _id;
@property(nonatomic, readonly) NSString* lastName;
@property(nonatomic, readonly) NSString* username;
@property(nonatomic, readonly) NSString* phone;
@property(nonatomic, readonly) NSString* email;
@property(nonatomic, readonly) NSNumber* userStatus;
@property(nonatomic, readonly) NSString* firstName;
@property(nonatomic, readonly) NSString* password;
- (id) _id: (NSNumber*) _id
     lastName: (NSString*) lastName
     username: (NSString*) username
     phone: (NSString*) phone
     email: (NSString*) email
     userStatus: (NSNumber*) userStatus
     firstName: (NSString*) firstName
     password: (NSString*) password;

- (id) initWithValues: (NSDictionary*)dict;


@end

