#import "NIKDate.h"
#import "NIKDevice.h"

@implementation NIKDevice

-(id)deviceType: (NSString*) deviceType
    deviceId: (NSString*) deviceId
{
  _deviceType = deviceType;
  _deviceId = deviceId;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _deviceType = dict[@"deviceType"]; 
        _deviceId = dict[@"deviceId"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_deviceType != nil) dict[@"deviceType"] = _deviceType ;
    if(_deviceId != nil) dict[@"deviceId"] = _deviceId ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

