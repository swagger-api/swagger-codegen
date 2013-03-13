#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NSDecimalNumber.h"
#import "NIKNumber.h"

@interface NIKXSCoordinates : NIKSwaggerObject

@property(nonatomic) NSDecimalNumber* longitude;
@property(nonatomic) NSDecimalNumber* latitude;
- (id) longitude: (NSDecimalNumber*) longitude
     latitude: (NSDecimalNumber*) latitude;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

