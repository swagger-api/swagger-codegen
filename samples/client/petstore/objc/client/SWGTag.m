#import "SWGDate.h"
#import "SWGTag.h"

@implementation SWGTag
    
+ (JSONKeyMapper *)keyMapper
{
    return [[JSONKeyMapper alloc] initWithDictionary:@{@"id": @"_id"}];    
}
    
    
+ (JSONKeyMapper *)keyMapper
{
    return [[JSONKeyMapper alloc] initWithDictionary:@{@"name": @"name"}];    
}
    
@end
