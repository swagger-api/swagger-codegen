#import "NIKDate.h"
#import "NIKAdRequestCall.h"

@implementation NIKAdRequestCall

-(id)duration: (NSNumber*) duration
    context: (NIKAdRequestContext*) context
    sessionKey: (NSString*) sessionKey
    adType: (NSString*) adType
{
  _duration = duration;
  _context = context;
  _sessionKey = sessionKey;
  _adType = adType;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _duration = dict[@"duration"]; 
        id context_dict = dict[@"context"];
        _context = [[NIKAdRequestContext alloc]initWithValues:context_dict];
        _sessionKey = dict[@"sessionKey"]; 
        _adType = dict[@"adType"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_duration != nil) dict[@"duration"] = _duration ;
    if(_context != nil){
        if([_context isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKAdRequestContext *context in (NSArray*)_context) {
                [array addObject:[(NIKSwaggerObject*)context asDictionary]];
            }
            dict[@"context"] = array;
        }
        else if(_context && [_context isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_context toString];
            if(dateString){
                dict[@"context"] = dateString;
            }
        }
        else if(_context && [_context isKindOfClass:[NIKSwaggerObject class]]) {
            dict[@"context"] = [(NIKSwaggerObject*)_context asDictionary];
        }
    }
    else {
    if(_context != nil) dict[@"context"] = [(NIKSwaggerObject*)_context asDictionary];
    }
    if(_sessionKey != nil) dict[@"sessionKey"] = _sessionKey ;
    if(_adType != nil) dict[@"adType"] = _adType ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

