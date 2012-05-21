#import "StoreApi.h"
#import "Order.h"



@implementation StoreApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

@synthesize queue = _queue;

- (id) init {
    [super init];
    _queue = [[NSOperationQueue alloc] init];
    return self;
}

-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(Order*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString:orderId];
    if(orderId == nil) {
        // error
    }
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];

    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
        ^(NSURLResponse *response, NSData *data, NSError *error) {
        if (error) {
            completionBlock(nil, error);return;
        }
        
        
        NSString* jsonString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];    
        id results = [jsonString objectFromJSONString];
    
        completionBlock( [[Order alloc]initWithValues: results], nil);}];
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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order/{orderId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"orderId", @"}"]] withString:orderId];
    if(orderId == nil) {
        // error
    }
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];

    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
        ^(NSURLResponse *response, NSData *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }
        
        
        completionBlock(nil);}];
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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/store.{format}/order?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(body == nil) {
        // error
    }
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    
    NSURL* URL = [NSURL URLWithString:requestUrl];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];

    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
        ^(NSURLResponse *response, NSData *data, NSError *error) {
        if (error) {
            completionBlock(error);return;
        }
        
        
        completionBlock(nil);}];
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
