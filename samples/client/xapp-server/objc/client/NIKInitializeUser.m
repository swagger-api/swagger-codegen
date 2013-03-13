#import "NIKDate.h"
#import "NIKInitializeUser.h"

@implementation NIKInitializeUser

-(id)streetAddress: (NSString*) streetAddress
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
    ethnicity: (NSString*) ethnicity
{
  _streetAddress = streetAddress;
  _lastName = lastName;
  _keywords = keywords;
  _state = state;
  _children = children;
  _maritalStatus = maritalStatus;
  _education = education;
  _homePhone = homePhone;
  _city = city;
  _country = country;
  _politics = politics;
  _mobilePhone = mobilePhone;
  _email = email;
  _zipCode = zipCode;
  _gender = gender;
  _birthDate = birthDate;
  _firstName = firstName;
  _ethnicity = ethnicity;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _streetAddress = dict[@"streetAddress"]; 
        _lastName = dict[@"lastName"]; 
        _keywords = dict[@"keywords"]; 
        _state = dict[@"state"]; 
        _children = dict[@"children"]; 
        _maritalStatus = dict[@"maritalStatus"]; 
        _education = dict[@"education"]; 
        _homePhone = dict[@"homePhone"]; 
        _city = dict[@"city"]; 
        _country = dict[@"country"]; 
        _politics = dict[@"politics"]; 
        _mobilePhone = dict[@"mobilePhone"]; 
        _email = dict[@"email"]; 
        _zipCode = dict[@"zipCode"]; 
        _gender = dict[@"gender"]; 
        _birthDate = dict[@"birthDate"]; 
        _firstName = dict[@"firstName"]; 
        _ethnicity = dict[@"ethnicity"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_streetAddress != nil) dict[@"streetAddress"] = _streetAddress ;
    if(_lastName != nil) dict[@"lastName"] = _lastName ;
    if(_keywords != nil) dict[@"keywords"] = _keywords ;
    if(_state != nil) dict[@"state"] = _state ;
    if(_children != nil) dict[@"children"] = _children ;
    if(_maritalStatus != nil) dict[@"maritalStatus"] = _maritalStatus ;
    if(_education != nil) dict[@"education"] = _education ;
    if(_homePhone != nil) dict[@"homePhone"] = _homePhone ;
    if(_city != nil) dict[@"city"] = _city ;
    if(_country != nil) dict[@"country"] = _country ;
    if(_politics != nil) dict[@"politics"] = _politics ;
    if(_mobilePhone != nil) dict[@"mobilePhone"] = _mobilePhone ;
    if(_email != nil) dict[@"email"] = _email ;
    if(_zipCode != nil) dict[@"zipCode"] = _zipCode ;
    if(_gender != nil) dict[@"gender"] = _gender ;
    if(_birthDate != nil) dict[@"birthDate"] = _birthDate ;
    if(_firstName != nil) dict[@"firstName"] = _firstName ;
    if(_ethnicity != nil) dict[@"ethnicity"] = _ethnicity ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

