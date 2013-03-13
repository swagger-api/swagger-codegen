#import "NIKDate.h"
#import "NIKAdResponseReply.h"

@implementation NIKAdResponseReply

-(id)errorMessage: (NSString*) errorMessage
    errorCode: (NSString*) errorCode
    success: (NSNumber*) success
{
  _errorMessage = errorMessage;
  _errorCode = errorCode;
  _success = success;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _errorMessage = dict[@"errorMessage"]; 
        _errorCode = dict[@"errorCode"]; 
        _success = dict[@"success"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_errorMessage != nil) dict[@"errorMessage"] = _errorMessage ;
    if(_errorCode != nil) dict[@"errorCode"] = _errorCode ;
    if(_success != nil) dict[@"success"] = _success ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

