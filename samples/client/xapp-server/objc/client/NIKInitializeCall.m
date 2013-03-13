#import "NIKDate.h"
#import "NIKInitializeCall.h"

@implementation NIKInitializeCall

-(id)pluginVersion: (NSString*) pluginVersion
    location: (NIKXSCoordinates*) location
    debugEnabled: (NSNumber*) debugEnabled
    applicationKey: (NSString*) applicationKey
    device: (NIKDevice*) device
    user: (NIKInitializeUser*) user
    apiKey: (NSString*) apiKey
    xappAdsEnabled: (NSNumber*) xappAdsEnabled
{
  _pluginVersion = pluginVersion;
  _location = location;
  _debugEnabled = debugEnabled;
  _applicationKey = applicationKey;
  _device = device;
  _user = user;
  _apiKey = apiKey;
  _xappAdsEnabled = xappAdsEnabled;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _pluginVersion = dict[@"pluginVersion"]; 
        id location_dict = dict[@"location"];
        _location = [[NIKXSCoordinates alloc]initWithValues:location_dict];
        _debugEnabled = dict[@"debugEnabled"]; 
        _applicationKey = dict[@"applicationKey"]; 
        id device_dict = dict[@"device"];
        _device = [[NIKDevice alloc]initWithValues:device_dict];
        id user_dict = dict[@"user"];
        _user = [[NIKInitializeUser alloc]initWithValues:user_dict];
        _apiKey = dict[@"apiKey"]; 
        _xappAdsEnabled = dict[@"xappAdsEnabled"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_pluginVersion != nil) dict[@"pluginVersion"] = _pluginVersion ;
    if(_location != nil){
        if([_location isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKXSCoordinates *location in (NSArray*)_location) {
                [array addObject:[(NIKSwaggerObject*)location asDictionary]];
            }
            dict[@"location"] = array;
        }
        else if(_location && [_location isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_location toString];
            if(dateString){
                dict[@"location"] = dateString;
            }
        }
        else if(_location && [_location isKindOfClass:[NIKSwaggerObject class]]) {
            dict[@"location"] = [(NIKSwaggerObject*)_location asDictionary];
        }
    }
    else {
    if(_location != nil) dict[@"location"] = [(NIKSwaggerObject*)_location asDictionary];
    }
    if(_debugEnabled != nil) dict[@"debugEnabled"] = _debugEnabled ;
    if(_applicationKey != nil) dict[@"applicationKey"] = _applicationKey ;
    if(_device != nil){
        if([_device isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKDevice *device in (NSArray*)_device) {
                [array addObject:[(NIKSwaggerObject*)device asDictionary]];
            }
            dict[@"device"] = array;
        }
        else if(_device && [_device isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_device toString];
            if(dateString){
                dict[@"device"] = dateString;
            }
        }
        else if(_device && [_device isKindOfClass:[NIKSwaggerObject class]]) {
            dict[@"device"] = [(NIKSwaggerObject*)_device asDictionary];
        }
    }
    else {
    if(_device != nil) dict[@"device"] = [(NIKSwaggerObject*)_device asDictionary];
    }
    if(_user != nil){
        if([_user isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKInitializeUser *user in (NSArray*)_user) {
                [array addObject:[(NIKSwaggerObject*)user asDictionary]];
            }
            dict[@"user"] = array;
        }
        else if(_user && [_user isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_user toString];
            if(dateString){
                dict[@"user"] = dateString;
            }
        }
        else if(_user && [_user isKindOfClass:[NIKSwaggerObject class]]) {
            dict[@"user"] = [(NIKSwaggerObject*)_user asDictionary];
        }
    }
    else {
    if(_user != nil) dict[@"user"] = [(NIKSwaggerObject*)_user asDictionary];
    }
    if(_apiKey != nil) dict[@"apiKey"] = _apiKey ;
    if(_xappAdsEnabled != nil) dict[@"xappAdsEnabled"] = _xappAdsEnabled ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

