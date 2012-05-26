#import "ApiInvoker.h"

@implementation ApiInvoker

@synthesize queue = _queue;
@synthesize defaultHeaders = _defaultHeaders;

- (id) init {
    [super init];
    _queue = [[NSOperationQueue alloc] init];
    _defaultHeaders = [[NSMutableDictionary alloc] init];
    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_defaultHeaders setValue:value forKey:key];
}

-(NSString*) escapeString:(NSString*) string{
    return string;
}

-(id) invokeWithCompletionBlock:(NSString*) path
                         method:(NSString*) method
                    queryParams:(NSDictionary*) queryParams
                           body:(NSDictionary*) body
                   headerParams:(NSDictionary*) headerParams
              completionHandler:(void (^)(NSDictionary*, NSError *))completionBlock
{
    NSMutableString * requestUrl = [NSMutableString stringWithFormat:@"%@", path];
    NSString * separator = nil;
    int counter = 0;
    if(queryParams != nil){
        for(NSString * key in [queryParams keyEnumerator]){
            if(counter == 0) separator = @"?";
            else separator = @"&";
            [requestUrl appendFormat:[NSString stringWithFormat:@"%@%@=%@", separator,
                                      [self escapeString:key], [self escapeString:[queryParams valueForKey:key]]]];
            counter += 1;
        }
    }
    NSLog(@"request url: %@", requestUrl);

    NSURL* URL = [NSURL URLWithString:requestUrl];
    
    NSMutableURLRequest* request = [[[NSMutableURLRequest alloc] init] autorelease];
    [request setURL:URL];
    [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
    [request setTimeoutInterval:30];

    for(NSString * key in [_defaultHeaders keyEnumerator]){
        [request setValue:[_defaultHeaders valueForKey:key] forHTTPHeaderField:key];            
    }
    if(headerParams != nil){
        for(NSString * key in [headerParams keyEnumerator]){
            [request setValue:[headerParams valueForKey:key] forHTTPHeaderField:key];            
        }
    }
    [request setHTTPMethod:method];
    if(body != nil) {
        NSString * json = [body JSONString];
        NSData *data = [json dataUsingEncoding:NSUTF8StringEncoding];
        NSString *postLength = [NSString stringWithFormat:@"%d", [data length]];
        
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
        [request setHTTPBody:data];
        [request setCachePolicy:NSURLRequestReloadIgnoringLocalCacheData];
        
        NSLog(@"request: %@", request);
    }

    [NSURLConnection sendAsynchronousRequest:request queue:_queue completionHandler:
     ^(NSURLResponse *response, NSData *data, NSError *error) {
         if (error) {
             completionBlock(nil, error);
             return;
         }
         else {
             NSString* jsonString = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];    
             id results = [jsonString objectFromJSONString];
             completionBlock(results, nil);
         }
     }];
    return nil;
}
@end

