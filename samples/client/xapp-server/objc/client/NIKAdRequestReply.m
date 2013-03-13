#import "NIKDate.h"
#import "NIKAdRequestReply.h"

@implementation NIKAdRequestReply

-(id)errorMessage: (NSString*) errorMessage
    impressionId: (NSString*) impressionId
    imageUrl: (NSString*) imageUrl
    debugEnabled: (NSNumber*) debugEnabled
    promptUrl: (NSString*) promptUrl
    scrollingText: (NSString*) scrollingText
    moreInfoAlias: (NSString*) moreInfoAlias
    duration: (NSNumber*) duration
    moreInfoUrl: (NSString*) moreInfoUrl
    errorCode: (NSString*) errorCode
    adType: (NSString*) adType
    success: (NSNumber*) success
    actions: (NSArray*) actions
{
  _errorMessage = errorMessage;
  _impressionId = impressionId;
  _imageUrl = imageUrl;
  _debugEnabled = debugEnabled;
  _promptUrl = promptUrl;
  _scrollingText = scrollingText;
  _moreInfoAlias = moreInfoAlias;
  _duration = duration;
  _moreInfoUrl = moreInfoUrl;
  _errorCode = errorCode;
  _adType = adType;
  _success = success;
  _actions = actions;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _errorMessage = dict[@"errorMessage"]; 
        _impressionId = dict[@"impressionId"]; 
        _imageUrl = dict[@"imageUrl"]; 
        _debugEnabled = dict[@"debugEnabled"]; 
        _promptUrl = dict[@"promptUrl"]; 
        _scrollingText = dict[@"scrollingText"]; 
        _moreInfoAlias = dict[@"moreInfoAlias"]; 
        _duration = dict[@"duration"]; 
        _moreInfoUrl = dict[@"moreInfoUrl"]; 
        _errorCode = dict[@"errorCode"]; 
        _adType = dict[@"adType"]; 
        _success = dict[@"success"]; 
        id actions_dict = dict[@"actions"];
        if([actions_dict isKindOfClass:[NSArray class]]) {

            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[(NSArray*)actions_dict count]];

            if([(NSArray*)actions_dict count] > 0) {
                for (NSDictionary* dict in (NSArray*)actions_dict) {
                    NIKAdRequestAction* d = [[NIKAdRequestAction alloc] initWithValues:dict];
                    [objs addObject:d];
                }
                
                _actions = [[NSArray alloc] initWithArray:objs];
            }
            else {
                _actions = [[NSArray alloc] init];
            }
        }
        else {
            _actions = [[NSArray alloc] init];
        }
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_errorMessage != nil) dict[@"errorMessage"] = _errorMessage ;
    if(_impressionId != nil) dict[@"impressionId"] = _impressionId ;
    if(_imageUrl != nil) dict[@"imageUrl"] = _imageUrl ;
    if(_debugEnabled != nil) dict[@"debugEnabled"] = _debugEnabled ;
    if(_promptUrl != nil) dict[@"promptUrl"] = _promptUrl ;
    if(_scrollingText != nil) dict[@"scrollingText"] = _scrollingText ;
    if(_moreInfoAlias != nil) dict[@"moreInfoAlias"] = _moreInfoAlias ;
    if(_duration != nil) dict[@"duration"] = _duration ;
    if(_moreInfoUrl != nil) dict[@"moreInfoUrl"] = _moreInfoUrl ;
    if(_errorCode != nil) dict[@"errorCode"] = _errorCode ;
    if(_adType != nil) dict[@"adType"] = _adType ;
    if(_success != nil) dict[@"success"] = _success ;
    if(_actions != nil){
        if([_actions isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKAdRequestAction *actions in (NSArray*)_actions) {
                [array addObject:[(NIKSwaggerObject*)actions asDictionary]];
            }
            dict[@"actions"] = array;
        }
        else if(_actions && [_actions isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_actions toString];
            if(dateString){
                dict[@"actions"] = dateString;
            }
        }
    }
    else {
    if(_actions != nil) dict[@"actions"] = [(NIKSwaggerObject*)_actions asDictionary];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

