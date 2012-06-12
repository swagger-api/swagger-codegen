#import "ApiInvoker.h"

@implementation ApiInvoker

@synthesize queue = _queue;
@synthesize defaultHeaders = _defaultHeaders;

- (id) init {
    self = [super init];
    _queue = [[NSOperationQueue alloc] init];
    _defaultHeaders = [[NSMutableDictionary alloc] init];
    return self;
}

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key {
    [_defaultHeaders setValue:value forKey:key];
}

-(NSString*) escapeString:(NSString*) string{
    if(string == nil)
        return nil;
    if([string isKindOfClass:[NSString class]])
        return [string stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    else {
        return string;
    }
}

-(id) invokeWithCompletionBlock:(NSString*) path
                         method:(NSString*) method
                    queryParams:(NSDictionary*) queryParams
                           body:(id) body
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
    
    NSMutableURLRequest* request = [[NSMutableURLRequest alloc] init];
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
        NSError * error = [NSError new];
        NSData * data = nil;
        if([body isKindOfClass:[NSDictionary class]]){
            data = [NSJSONSerialization dataWithJSONObject:body 
                                                   options:kNilOptions error:&error];
        }
        else {
            data = [body dataUsingEncoding:NSUTF8StringEncoding];
        }
        NSString *postLength = [NSString stringWithFormat:@"%d", [data length]];
        [request setValue:postLength forHTTPHeaderField:@"Content-Length"];
        [request setHTTPBody:data];
        
        [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
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
             NSDictionary* results = [NSJSONSerialization JSONObjectWithData:data
                                                                     options:kNilOptions 
                                                                       error:&error];
             completionBlock(results, nil);
         }
     }];
    return nil;
}
@end