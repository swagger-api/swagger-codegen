#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKAdResponseCall : NIKSwaggerObject

@property(nonatomic) NSString* errorMessage;
@property(nonatomic) NSString* impressionId;
@property(nonatomic) NSString* errorCode;
@property(nonatomic) NSString* responseText;
@property(nonatomic) NSString* sessionKey;
@property(nonatomic) NSString* responseAction;
@property(nonatomic) NSString* responseTimestamp;
@property(nonatomic) NSNumber* success;
- (id) errorMessage: (NSString*) errorMessage
     impressionId: (NSString*) impressionId
     errorCode: (NSString*) errorCode
     responseText: (NSString*) responseText
     sessionKey: (NSString*) sessionKey
     responseAction: (NSString*) responseAction
     responseTimestamp: (NSString*) responseTimestamp
     success: (NSNumber*) success;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

