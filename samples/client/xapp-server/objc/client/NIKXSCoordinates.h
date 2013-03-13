#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKNumber.h"

@interface NIKXSCoordinates : NIKSwaggerObject

@property(nonatomic) NIKNumber* longitude;
@property(nonatomic) NIKNumber* latitude;
- (id) longitude: (NIKNumber*) longitude
     latitude: (NIKNumber*) latitude;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

