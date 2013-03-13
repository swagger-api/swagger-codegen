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
        id pollChoices_dict = dict[@"pollChoices"];
        if([pollChoices_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)pollChoices_dict count]];

            if([(NSArray*)pollChoices_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)pollChoices_dict) {
                    NIKArray[String]* d = [[NIKArray[String] alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _pollChoices = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _pollChoices = [[NSArray alloc] init];
            }
        }
        else {
            _pollChoices = [[NSArray alloc] init];
        }
        _gotoUrl = dict[@"gotoUrl"]; 
        _callPhoneNumber = dict[@"callPhoneNumber"]; 
        _actionType = dict[@"actionType"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_audioUrl != nil) dict[@"audioUrl"] = _audioUrl ;
    if(_pollChoices != nil){
        if([_pollChoices isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKArray[String] *pollChoices in (NSArray*)_pollChoices) {
                [array addObject:[(NIKSwaggerObject*)pollChoices asDictionary]];
            }
            dict[@"pollChoices"] = array;
        }
        else if(_pollChoices && [_pollChoices isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_pollChoices toString];
            if(dateString){
                dict[@"pollChoices"] = dateString;
            }
        }
    }
    else {
    if(_pollChoices != nil) dict[@"pollChoices"] = [(NIKSwaggerObject*)_pollChoices asDictionary];
    }
    if(_gotoUrl != nil) dict[@"gotoUrl"] = _gotoUrl ;
    if(_callPhoneNumber != nil) dict[@"callPhoneNumber"] = _callPhoneNumber ;
    if(_actionType != nil) dict[@"actionType"] = _actionType ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

