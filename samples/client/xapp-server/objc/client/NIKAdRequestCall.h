#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKAdRequestContext.h"

@interface NIKAdRequestCall : NIKSwaggerObject

@property(nonatomic) NSNumber* duration;
@property(nonatomic) NIKAdRequestContext* context;
@property(nonatomic) NSString* sessionKey;
@property(nonatomic) NSString* adType;
- (id) duration: (NSNumber*) duration
     context: (NIKAdRequestContext*) context
     sessionKey: (NSString*) sessionKey
     adType: (NSString*) adType;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

