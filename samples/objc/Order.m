#import "Order.h"

@implementation Order

@synthesize _id = __id;
@synthesize petId = _petId;
@synthesize status = _status;
@synthesize quantity = _quantity;
@synthesize shipDate = _shipDate;
- (id) _id: (NSNumber*) _id
       petId: (NSNumber*) petId
       status: (NSString*) status
       quantity: (NSNumber*) quantity
       shipDate: (Date*) shipDate
       {
          __id = _id;
          _petId = petId;
          _status = status;
          _quantity = quantity;
          _shipDate = shipDate;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _petId = [dict objectForKey:@"petId"];
    _status = [dict objectForKey:@"status"];
    _quantity = [dict objectForKey:@"quantity"];
    id shipDate_dict = [dict objectForKey:@"shipDate"];
    if([shipDate_dict isKindOfClass:[NSArray class]]) {
        if([shipDate_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[shipDate_dict count]];
            for (NSDictionary* dict in shipDate_dict) {
                Date* d = [[Date alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _shipDate = [[NSArray alloc] initWithArray:objs];
        }
		}
    else if([shipDate_dict isKindOfClass:[NSDictionary class]] && [shipDate_dict count] > 0) {
        _shipDate = [[Date alloc]initWithValues:shipDate_dict];
    }
    else {
        _shipDate = [[Date alloc]initWithValues:shipDate_dict];
    }
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    [dict setObject:__id forKey:@"id"];
    [dict setObject:_petId forKey:@"petId"];
    [dict setObject:_status forKey:@"status"];
    [dict setObject:_quantity forKey:@"quantity"];
    if(_shipDate != nil){
        if([_shipDate isKindOfClass:[NSArray class]]){
            NSMutableArray * array = [[NSMutableArray alloc] init];
            for( Date * shipDate in _shipDate) {
                [array addObject:[shipDate asDictionary]];
            }
            [dict setObject:array forKey:@"shipDate"];
        }
    }
    else {
    [dict setObject:[_shipDate asDictionary]forKey:@"shipDate"];
    }
    NSDictionary* output = [[dict copy] autorelease];
    return output;
}

@end

