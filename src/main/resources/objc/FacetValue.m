#import "FacetValue.h"

@implementation FacetValue

@synthesize value = _value;
@synthesize count = _count;

- (id) initWithValues: (NSDictionary*)dict {
    _value = [dict objectForKey:@"value"];
    _count = [dict objectForKey:@"count"];
    return self;
}

-(NSDictionary*) asDictionary {
    NSMutableDictionary* dict = [[NSMutableDictionary alloc] init];
    if(_cite != nil) [dict setObject:_value forKey:@"value"];
    if(_source != nil) [dict setObject:_count forKey:@"count"];
    NSDictionary* output = [[dict copy] autorelease];
    return output;
}

@end
