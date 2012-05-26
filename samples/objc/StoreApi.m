#import "StoreApi.h"
#import "Order.h"



@implementation StoreApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

@synthesize queue = _queue;
@synthesize api = _api;

- (id) init {
    [super init];
    _queue = [[NSOperationQueue alloc] init];
    _api = [[ApiInvoker alloc] init];

    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_api addHeader:value forKey:key];
}

-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(Order*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(orderId == nil) {
        // error
    }
    [_api invokeWithCompletionBlock: requestUrl 
                             method: @"GET" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        completionBlock( [[Order alloc]initWithValues: data], nil);
    }];
}


-(Order*) getOrderById :(NSString*) orderId
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString:orderId];
    if(orderId == nil) {
        // error
    }
    NSError* error = nil;
    NSURLResponse* response = nil;
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];
    
    NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:&error];
    
    if (error) {
        NSLog(@"Error performing request %@", requestUrl);
        return 0;
    }
    NSString* jsonString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];    
    id results = [jsonString objectFromJSONString];

    return [[Order alloc]initWithValues: results];
    
}
-(void) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString: [_api escapeString:orderId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(orderId == nil) {
        // error
    }
    [_api invokeWithCompletionBlock: requestUrl 
                             method: @"DELETE" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }
        
        completionBlock(nil);
    }];
}


-(void) deleteOrder :(NSString*) orderId
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString:orderId];
    if(orderId == nil) {
        // error
    }
    NSError* error = nil;
    NSURLResponse* response = nil;
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];
    
    NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:&error];
    
    if (error) {
        NSLog(@"Error performing request %@", requestUrl);
        }
    return;
    
}
-(void) placeOrderWithCompletionBlock :(Order*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(body != nil && [body isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] init];
        for (id dict in body) {
            if([dict respondsToSelector:@selector(asDictionary:)]) {
                [objs addObject:[dict asDictionary]];
            }
            else{
                [objs addObject:dict];
            }
        }
        bodyDictionary = objs;
    }
    else if([body respondsToSelector:@selector(asDictionary)]) {
        bodyDictionary = [body asDictionary];
    }
    else{
        NSLog(@"don't know what to do with %@", body);
    }

    if(body == nil) {
        // error
    }
    [_api invokeWithCompletionBlock: requestUrl 
                             method: @"POST" 
                        queryParams: queryParams 
                               body: bodyDictionary 
                       headerParams: headerParams
                  completionHandler: ^(NSDictionary *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }
        
        completionBlock(nil);
    }];
}


-(void) placeOrder :(Order*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(body == nil) {
        // error
    }
    NSError* error = nil;
    NSURLResponse* response = nil;
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];
    
    NSData* data = [NSURLConnection sendSynchronousRequest:request returningResponse:&response error:&error];
    
    if (error) {
        NSLog(@"Error performing request %@", requestUrl);
        }
    return;
    
}

@end
