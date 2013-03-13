#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "NIKInitializeUser.h"
#import "NIKXSCoordinates.h"
#import "NIKDevice.h"

@interface NIKInitializeCall : NIKSwaggerObject

@property(nonatomic) NSString* pluginVersion;
@property(nonatomic) NIKXSCoordinates* location;
@property(nonatomic) NSNumber* debugEnabled;
@property(nonatomic) NSString* applicationKey;
@property(nonatomic) NIKDevice* device;
@property(nonatomic) NIKInitializeUser* user;
@property(nonatomic) NSString* apiKey;
@property(nonatomic) NSNumber* xappAdsEnabled;
- (id) pluginVersion: (NSString*) pluginVersion
     location: (NIKXSCoordinates*) location
     debugEnabled: (NSNumber*) debugEnabled
     applicationKey: (NSString*) applicationKey
     device: (NIKDevice*) device
     user: (NIKInitializeUser*) user
     apiKey: (NSString*) apiKey
     xappAdsEnabled: (NSNumber*) xappAdsEnabled;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

