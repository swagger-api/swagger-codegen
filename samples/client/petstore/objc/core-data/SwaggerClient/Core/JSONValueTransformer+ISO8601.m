#import "JSONValueTransformer+ISO8601.h"
#import "SWGDefaultConfiguration.h"

@implementation JSONValueTransformer (ISO8601)

- (NSDate *) NSDateFromNSString:(NSString *)string
{
    return [[SWGDefaultConfiguration sharedConfig].dateFormatter dateFromString:string];
}

- (NSString *) JSONObjectFromNSDate:(NSDate *)date
{
    return [[SWGDefaultConfiguration sharedConfig].dateFormatter stringFromDate:date];
}

@end
