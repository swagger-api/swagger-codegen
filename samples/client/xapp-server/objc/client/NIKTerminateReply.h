#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"

@interface NIKTerminateReply : NIKSwaggerObject

@property(nonatomic) NSString* errorMessage;
@property(nonatomic) NSString* errorCode;
@property(nonatomic) NSNumber* success;
- (id) errorMessage: (NSString*) errorMessage
     errorCode: (NSString*) errorCode
     success: (NSNumber*) success;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

