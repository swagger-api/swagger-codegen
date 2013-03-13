#import "NIKDate.h"
#import "NIKInitializeReply.h"

@implementation NIKInitializeReply

-(id)errorMessage: (NSString*) errorMessage
    debugEnabled: (NSNumber*) debugEnabled
    errorCode: (NSString*) errorCode
    sessionKey: (NSString*) sessionKey
    success: (NSNumber*) success
{
  _errorMessage = errorMessage;
  _debugEnabled = debugEnabled;
  _errorCode = errorCode;
  _sessionKey = sessionKey;
  _success = success;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _errorMessage = dict[@"errorMessage"]; 
        _debugEnabled = dict[@"debugEnabled"]; 
        _errorCode = dict[@"errorCode"]; 
        _sessionKey = dict[@"sessionKey"]; 
        _success = dict[@"success"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_errorMessage != nil) dict[@"errorMessage"] = _errorMessage ;
    if(_debugEnabled != nil) dict[@"debugEnabled"] = _debugEnabled ;
    if(_errorCode != nil) dict[@"errorCode"] = _errorCode ;
    if(_sessionKey != nil) dict[@"sessionKey"] = _sessionKey ;
    if(_success != nil) dict[@"success"] = _success ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

