#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKArray[String].h"

@interface NIKAdRequestAction : NIKSwaggerObject

@property(nonatomic) NSString* audioUrl;
@property(nonatomic) NSArray* pollChoices;
@property(nonatomic) NSString* gotoUrl;
@property(nonatomic) NSString* callPhoneNumber;
@property(nonatomic) NSString* actionType;
- (id) audioUrl: (NSString*) audioUrl
     pollChoices: (NSArray*) pollChoices
     gotoUrl: (NSString*) gotoUrl
     callPhoneNumber: (NSString*) callPhoneNumber
     actionType: (NSString*) actionType;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

