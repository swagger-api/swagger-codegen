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
    if([shipDate_dict isKindOfClass:[NSArray class]]){
        if([shipDate_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[shipDate_dict count]];
            for (NSDictionary* dict in shipDate_dict) {
                Date* d = [[Date alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _shipDate = [[NSArray alloc] initWithArray:objs];
        }
		}
    else {
        _shipDate = [[Date alloc]initWithValues:shipDate_dict];
    }
    return self;
}
@end

