#import "Pet.h"

@implementation Pet

@synthesize _id = __id;
@synthesize tags = _tags;
@synthesize category = _category;
@synthesize status = _status;
@synthesize name = _name;
@synthesize photoUrls = _photoUrls;
- (id) _id: (NSNumber*) _id
       tags: (NSArray*) tags
       category: (Category*) category
       status: (NSString*) status
       name: (NSString*) name
       photoUrls: (NSArray*) photoUrls
       {
          __id = _id;
          _tags = tags;
          _category = category;
          _status = status;
          _name = name;
          _photoUrls = photoUrls;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    id tags_dict = [dict objectForKey:@"tags"];
    if([tags_dict isKindOfClass:[NSArray class]]){
        if([tags_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[tags_dict count]];
            for (NSDictionary* dict in tags_dict) {
                Tag* d = [[Tag alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _tags = [[NSArray alloc] initWithArray:objs];
        }
		}
    else {
        _tags = [[Tag alloc]initWithValues:tags_dict];
    }
    id category_dict = [dict objectForKey:@"category"];
    if([category_dict isKindOfClass:[NSArray class]]){
        if([category_dict count] > 0) {
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[category_dict count]];
            for (NSDictionary* dict in category_dict) {
                Category* d = [[Category alloc]initWithValues:dict];
                [objs addObject:d];
            }
            _category = [[NSArray alloc] initWithArray:objs];
        }
		}
    else {
        _category = [[Category alloc]initWithValues:category_dict];
    }
    _status = [dict objectForKey:@"status"];
    _name = [dict objectForKey:@"name"];
    _photoUrls = [dict objectForKey:@"photoUrls"];
    return self;
}
@end

