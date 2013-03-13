#import "NIKDate.h"
#import "NIKAdResponseCall.h"

@implementation NIKAdResponseCall

-(id)errorMessage: (NSString*) errorMessage
    impressionId: (NSString*) impressionId
    errorCode: (NSString*) errorCode
    sessionKey: (NSString*) sessionKey
    responseText: (NSString*) responseText
    responseAction: (NSString*) responseAction
    responseTimestamp: (NSString*) responseTimestamp
    success: (NSNumber*) success
{
  _errorMessage = errorMessage;
  _impressionId = impressionId;
  _errorCode = errorCode;
  _sessionKey = sessionKey;
  _responseText = responseText;
  _responseAction = responseAction;
  _responseTimestamp = responseTimestamp;
  _success = success;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _errorMessage = dict[@"errorMessage"]; 
        _impressionId = dict[@"impressionId"]; 
        _errorCode = dict[@"errorCode"]; 
        _sessionKey = dict[@"sessionKey"]; 
        _responseText = dict[@"responseText"]; 
        _responseAction = dict[@"responseAction"]; 
        _responseTimestamp = dict[@"responseTimestamp"]; 
        _success = dict[@"success"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_errorMessage != nil) dict[@"errorMessage"] = _errorMessage ;
    if(_impressionId != nil) dict[@"impressionId"] = _impressionId ;
    if(_errorCode != nil) dict[@"errorCode"] = _errorCode ;
    if(_sessionKey != nil) dict[@"sessionKey"] = _sessionKey ;
    if(_responseText != nil) dict[@"responseText"] = _responseText ;
    if(_responseAction != nil) dict[@"responseAction"] = _responseAction ;
    if(_responseTimestamp != nil) dict[@"responseTimestamp"] = _responseTimestamp ;
    if(_success != nil) dict[@"success"] = _success ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

