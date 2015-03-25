#import "SWGDate.h"
#import "SWGCategory.h"

@implementation SWGCategory
    
+ (JSONKeyMapper *)keyMapper
{
    return [[JSONKeyMapper alloc] initWithDictionary:@{@"id": @"_id"}];    
}
    
    
+ (JSONKeyMapper *)keyMapper
{
    return [[JSONKeyMapper alloc] initWithDictionary:@{@"name": @"name"}];    
}
    
@end
