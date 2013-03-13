#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKDevice : NIKSwaggerObject

@property(nonatomic) NSString* deviceType;
@property(nonatomic) NSString* deviceId;
- (id) deviceType: (NSString*) deviceType
     deviceId: (NSString*) deviceId;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

