#import <Foundation/Foundation.h>

@interface SwaggerObject : NSObject
- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;
@end
