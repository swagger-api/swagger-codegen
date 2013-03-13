#import "NIKDate.h"
#import "NIKAdRequestContext.h"

@implementation NIKAdRequestContext

-(id)genre: (NSString*) genre
    location: (NIKXSCoordinates*) location
    stationFormat: (NSString*) stationFormat
{
  _genre = genre;
  _location = location;
  _stationFormat = stationFormat;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        _genre = dict[@"genre"]; 
        id location_dict = dict[@"location"];
        _location = [[NIKXSCoordinates alloc]initWithValues:location_dict];
        _stationFormat = dict[@"stationFormat"]; 
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_genre != nil) dict[@"genre"] = _genre ;
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
    }
    else {
    if(_location != nil) dict[@"location"] = [(NIKSwaggerObject*)_location asDictionary];
    }
    if(_stationFormat != nil) dict[@"stationFormat"] = _stationFormat ;
    NSDictionary* output = [dict copy];
    return output;
}

@end

