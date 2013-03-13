#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKInitializeUser : NIKSwaggerObject

@property(nonatomic) NSString* streetAddress;
@property(nonatomic) NSString* lastName;
@property(nonatomic) NSString* keywords;
@property(nonatomic) NSString* state;
@property(nonatomic) NSNumber* children;
@property(nonatomic) NSString* maritalStatus;
@property(nonatomic) NSString* education;
@property(nonatomic) NSString* homePhone;
@property(nonatomic) NSString* city;
@property(nonatomic) NSString* country;
@property(nonatomic) NSString* politics;
@property(nonatomic) NSString* mobilePhone;
@property(nonatomic) NSString* email;
@property(nonatomic) NSString* zipCode;
@property(nonatomic) NSString* gender;
@property(nonatomic) NSString* birthDate;
@property(nonatomic) NSString* firstName;
@property(nonatomic) NSString* ethnicity;
- (id) streetAddress: (NSString*) streetAddress
     lastName: (NSString*) lastName
     keywords: (NSString*) keywords
     state: (NSString*) state
     children: (NSNumber*) children
     maritalStatus: (NSString*) maritalStatus
     education: (NSString*) education
     homePhone: (NSString*) homePhone
     city: (NSString*) city
     country: (NSString*) country
     politics: (NSString*) politics
     mobilePhone: (NSString*) mobilePhone
     email: (NSString*) email
     zipCode: (NSString*) zipCode
     gender: (NSString*) gender
     birthDate: (NSString*) birthDate
     firstName: (NSString*) firstName
     ethnicity: (NSString*) ethnicity;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

