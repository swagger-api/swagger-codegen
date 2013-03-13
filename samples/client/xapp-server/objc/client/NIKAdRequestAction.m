#import "NIKDate.h"
#import "NIKAdRequestAction.h"

@implementation NIKAdRequestAction

-(id)audioUrl: (NSString*) audioUrl
    pollChoices: (NSArray*) pollChoices
    gotoUrl: (NSString*) gotoUrl
    callPhoneNumber: (NSString*) callPhoneNumber
    actionType: (NSString*) actionType
{
  _audioUrl = audioUrl;
  _pollChoices = pollChoices;
  _gotoUrl = gotoUrl;
  _callPhoneNumber = callPhoneNumber;
  _actionType = actionType;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _audioUrl = dict[@"audioUrl"]; 
        _pollChoices = dict[@"pollChoices"]; 
        _gotoUrl = dict[@"gotoUrl"]; 
        _callPhoneNumber = dict[@"callPhoneNumber"]; 
        _actionType = dict[@"actionType"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_audioUrl != nil) dict[@"audioUrl"] = _audioUrl ;
    if(_pollChoices != nil) dict[@"pollChoices"] = _pollChoices ;
    if(_gotoUrl != nil) dict[@"gotoUrl"] = _gotoUrl ;
    if(_callPhoneNumber != nil) dict[@"callPhoneNumber"] = _callPhoneNumber ;
    if(_actionType != nil) dict[@"actionType"] = _actionType ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

