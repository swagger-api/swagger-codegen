#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKInitializeReply : NIKSwaggerObject

@property(nonatomic) NSString* errorMessage;
@property(nonatomic) NSNumber* debugEnabled;
@property(nonatomic) NSString* errorCode;
@property(nonatomic) NSString* sessionKey;
@property(nonatomic) NSNumber* success;
- (id) errorMessage: (NSString*) errorMessage
     debugEnabled: (NSNumber*) debugEnabled
     errorCode: (NSString*) errorCode
     sessionKey: (NSString*) sessionKey
     success: (NSNumber*) success;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

