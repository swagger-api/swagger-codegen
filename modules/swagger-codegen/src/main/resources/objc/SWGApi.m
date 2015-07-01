#import "SWGApi.h"

@implementation SWGApi

@dynamic apiClient;
- (SWGApiClient *) apiClient {
    return [SWGApiClient sharedClientFromPool:self.basePath];
}

+ (instancetype) apiWithBasePath:(NSString *)basePath {
    return [[self alloc] initWithBasePath:basePath];
}

- (id) initWithBasePath:(NSString *)basePath {
    if (self = [super init]) {
        _basePath = basePath;

        [self apiClient];
    }
    return self;
}

- (unsigned long) requestQueueSize {
    return [SWGApiClient requestQueueSize];
}

@end