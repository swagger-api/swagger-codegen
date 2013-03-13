#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKXSCoordinates.h"

@interface NIKAdRequestContext : NIKSwaggerObject

@property(nonatomic) NSString* genre;
@property(nonatomic) NIKXSCoordinates* location;
@property(nonatomic) NSString* stationFormat;
- (id) genre: (NSString*) genre
     location: (NIKXSCoordinates*) location
     stationFormat: (NSString*) stationFormat;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

