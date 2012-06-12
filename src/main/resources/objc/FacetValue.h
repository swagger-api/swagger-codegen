#import <Foundation/Foundation.h>

@interface FacetValue : NSObject {
@private
    NSNumber* _value;
    NSNumber* _count;
}

@property(nonatomic, readonly) NSNumber* value;
@property(nonatomic, readonly) NSNumber* count;
- (id) value: (NSNumber*) value
     count: (NSNumber*) count;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;

@end

