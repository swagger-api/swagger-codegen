#import "NIKDate.h"
#import "NIKXSCoordinates.h"

@implementation NIKXSCoordinates

-(id)longitude: (NIKNumber*) longitude
    latitude: (NIKNumber*) latitude
{
  _longitude = longitude;
  _latitude = latitude;
  return self;
}

-(id) initWithValues:(NSDictionary*)dict
{
    self = [super init];
    if(self) {
        id longitude_dict = dict[@"longitude"];
        _longitude = [[NIKNumber alloc]initWithValues:longitude_dict];
        id latitude_dict = dict[@"latitude"];
        _latitude = [[NIKNumber alloc]initWithValues:latitude_dict];
        

    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_longitude != nil){
        if([_longitude isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKNumber *longitude in (NSArray*)_longitude) {
                [array addObject:[(NIKSwaggerObject*)longitude asDictionary]];
            }
            dict[@"longitude"] = array;
        }
        else if(_longitude && [_longitude isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_longitude toString];
            if(dateString){
                dict[@"longitude"] = dateString;
            }
        }
    }
    else {
    if(_longitude != nil) dict[@"longitude"] = [(NIKSwaggerObject*)_longitude asDictionary];
    }
    if(_latitude != nil){
        if([_latitude isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( NIKNumber *latitude in (NSArray*)_latitude) {
                [array addObject:[(NIKSwaggerObject*)latitude asDictionary]];
            }
            dict[@"latitude"] = array;
        }
        else if(_latitude && [_latitude isKindOfClass:[NIKDate class]]) {
            NSString * dateString = [(NIKDate*)_latitude toString];
            if(dateString){
                dict[@"latitude"] = dateString;
            }
        }
    }
    else {
    if(_latitude != nil) dict[@"latitude"] = [(NIKSwaggerObject*)_latitude asDictionary];
    }
    NSDictionary* output = [dict copy];
    return output;
}

@end

