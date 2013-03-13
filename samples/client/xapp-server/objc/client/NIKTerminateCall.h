#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKTerminateCall : NIKSwaggerObject

@property(nonatomic) NSString* sessionKey;
- (id) sessionKey: (NSString*) sessionKey;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

