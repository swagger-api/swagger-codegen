#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKArray[AdRequestAction].h"

@interface NIKAdRequestReply : NIKSwaggerObject

@property(nonatomic) NSString* errorMessage;
@property(nonatomic) NSString* impressionId;
@property(nonatomic) NSString* imageUrl;
@property(nonatomic) NSNumber* debugEnabled;
@property(nonatomic) NSString* promptUrl;
@property(nonatomic) NSString* scrollingText;
@property(nonatomic) NSString* moreInfoAlias;
@property(nonatomic) NSNumber* duration;
@property(nonatomic) NSString* moreInfoUrl;
@property(nonatomic) NSString* errorCode;
@property(nonatomic) NSString* adType;
@property(nonatomic) NSNumber* success;
@property(nonatomic) NSArray* actions;
- (id) errorMessage: (NSString*) errorMessage
     impressionId: (NSString*) impressionId
     imageUrl: (NSString*) imageUrl
     debugEnabled: (NSNumber*) debugEnabled
     promptUrl: (NSString*) promptUrl
     scrollingText: (NSString*) scrollingText
     moreInfoAlias: (NSString*) moreInfoAlias
     duration: (NSNumber*) duration
     moreInfoUrl: (NSString*) moreInfoUrl
     errorCode: (NSString*) errorCode
     adType: (NSString*) adType
     success: (NSNumber*) success
     actions: (NSArray*) actions;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

