#import "NIKDate.h"
#import "NIKTerminateCall.h"

@implementation NIKTerminateCall

-(id)sessionKey: (NSString*) sessionKey
{
  _sessionKey = sessionKey;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _sessionKey = dict[@"sessionKey"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_sessionKey != nil) dict[@"sessionKey"] = _sessionKey ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

