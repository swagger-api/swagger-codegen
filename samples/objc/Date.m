#import "Date.h"

@implementation Date

@synthesize date = _date;

- (id) initWithValues:(NSNumber *)input {
    NSTimeInterval interval = [input doubleValue];
    _date = [[NSDate alloc] initWithTimeIntervalSince1970:interval];
    return self;
}

@end

