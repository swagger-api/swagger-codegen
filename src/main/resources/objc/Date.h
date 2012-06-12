#import <Foundation/Foundation.h>
#import "SwaggerObject.h"

@interface Date : SwaggerObject {
@private
    NSDate *_date;
}
@property(nonatomic, readonly) NSDate* date;

- (id) initWithValues: (NSString*)input;
-(NSString*) toString;
@end

