#import "NIKRestApi.h"
#import "NIKTerminateReply.h"
#import "NIKAdRequestReply.h"
#import "NIKAdResponseReply.h"
#import "NIKInitializeReply.h"



@implementation NIKRestApi
static NSString * basePath = @"http://localhost:9000";

@synthesize queue = _queue;
@synthesize api = _api;

+(NIKRestApi*) apiWithHeader:(NSString*)headerValue key:(NSString*)key {
    static NIKRestApi* singletonAPI = nil;

    if (singletonAPI == nil) {
        singletonAPI = [[NIKRestApi alloc] init];
        [singletonAPI addHeader:headerValue forKey:key];
    }
    return singletonAPI;
}

-(id) init {
    self = [super init];
    _queue = [[NSOperationQueue alloc] init];
    _api = [NIKApiInvoker sharedInstance];

    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_api addHeader:value forKey:key];
}

-(void) initializeWithCompletionBlock:(NIKInitializeCall*) 
        completionHandler: (void (^)(NIKInitializeReply* output, NSError* error))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/initialize", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        completionBlock( [[NIKInitializeReply alloc]initWithValues: data], nil);}];
    

}

-(void) terminateWithCompletionBlock:(NIKTerminateCall*) 
        completionHandler: (void (^)(NIKTerminateReply* output, NSError* error))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/terminate", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        completionBlock( [[NIKTerminateReply alloc]initWithValues: data], nil);}];
    

}

-(void) adResponseWithCompletionBlock:(NIKAdResponseCall*) 
        completionHandler: (void (^)(NIKAdResponseReply* output, NSError* error))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/ad_response", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        completionBlock( [[NIKAdResponseReply alloc]initWithValues: data], nil);}];
    

}

-(void) adRequestWithCompletionBlock:(NIKAdRequestCall*) 
        completionHandler: (void (^)(NIKAdRequestReply* output, NSError* error))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/ad_request", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
        [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        completionBlock( [[NIKAdRequestReply alloc]initWithValues: data], nil);}];
    

}

-(void) initializeAsJsonWithCompletionBlock :(NIKInitializeCall*)  

        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/initialize", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        

    }];


}

-(void) terminateAsJsonWithCompletionBlock :(NIKTerminateCall*)  

        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/terminate", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        

    }];


}

-(void) adResponseAsJsonWithCompletionBlock :(NIKAdResponseCall*)  

        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/ad_response", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        

    }];


}

-(void) adRequestAsJsonWithCompletionBlock :(NIKAdRequestCall*)  

        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/rest/xapp_session/ad_request", basePath];

    // remove format in URL if needed
    if ([requestUrl rangeOfString:@".{format}"].location != NSNotFound)
        [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@""];

    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if( != nil)
        queryParams[@""] = ;
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    id bodyDictionary = nil;
    [_api dictionary:requestUrl 
              method:@"POST" 
         queryParams:queryParams 
                body:bodyDictionary 
        headerParams:headerParams
     completionBlock:^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }

        NSData * responseData = nil;
            if([data isKindOfClass:[NSDictionary class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            else if ([data isKindOfClass:[NSArray class]]){
                responseData = [NSJSONSerialization dataWithJSONObject:data
                                                               options:kNilOptions error:&error];
            }
            NSString * json = [[NSString alloc]initWithData:responseData encoding:NSUTF8StringEncoding];
            completionBlock(json, nil);
        

    }];


}


@end
