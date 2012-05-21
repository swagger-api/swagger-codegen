#import "Tag.h"

@implementation Tag

@synthesize _id = __id;
@synthesize name = _name;
- (id) _id: (NSNumber*) _id
       name: (NSString*) name
       {
          __id = _id;
          _name = name;
          return self;
       }

- (id) initWithValues: (NSDictionary*)dict
{
    __id = [dict objectForKey:@"id"];
    _name = [dict objectForKey:@"name"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    [dict setObject:__id forKey:@"id"];
    [dict setObject:_name forKey:@"name"];
    NSDictionary* output = [[dict copy] autorelease];
    return output;
}

@end

