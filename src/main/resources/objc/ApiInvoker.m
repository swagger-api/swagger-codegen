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
    NSMutableString * requestUrl = [NSMutableString stringWithFormat:@"%@?", path];
    if(queryParams != nil){
        for(NSString * key in [queryParams keyEnumerator]){
            [requestUrl appendFormat:[NSString stringWithFormat:@"%@=%@",
                                      [self escapeString:key], [self escapeString:[queryParams valueForKey:key]]]];
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
    
    if([method isEqualToString:@"GET"]) {
        [request setHTTPMethod:method];

        NSLog(@"all headers: %@", _defaultHeaders);

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
    }
    return nil;
}
@end
