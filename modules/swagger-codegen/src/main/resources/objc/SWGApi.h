#import <Foundation/Foundation.h>
#import "SWGApiClient.h"

@interface SWGApi : NSObject

@property (nonatomic, strong, readonly) SWGApiClient *apiClient;
@property (nonatomic, strong, readonly) NSString *basePath;

+ (instancetype) apiWithBasePath:(NSString *)basePath;

-(unsigned long) requestQueueSize;

@end