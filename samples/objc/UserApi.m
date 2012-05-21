#import "UserApi.h"
#import "User.h"



@implementation UserApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

@synthesize queue = _queue;

- (id) init {
    [super init];
    _queue = [[NSOperationQueue alloc] init];
    return self;
}

-(void) createUsersWithArrayInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/createWithArray?", basePath];

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


-(void) createUsersWithArrayInput :(NSArray*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/createWithArray?", basePath];

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
-(void) createUserWithCompletionBlock :(User*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}?", basePath];

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


-(void) createUser :(User*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}?", basePath];

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
-(void) createUsersWithListInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/createWithList?", basePath];

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


-(void) createUsersWithListInput :(NSArray*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/createWithList?", basePath];

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
-(void) updateUserWithCompletionBlock :(NSString*) username body:(User*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
        // error
    }
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


-(void) updateUser :(NSString*) username
    body:(User*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
        // error
    }
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
-(void) deleteUserWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
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


-(void) deleteUser :(NSString*) username
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
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
-(void) getUserByNameWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(User*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
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
    
        completionBlock( [[User alloc]initWithValues: results], nil);}];
}


-(User*) getUserByName :(NSString*) username
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/{username}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"username", @"}"]] withString:username];
    if(username == nil) {
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

    return [[User alloc]initWithValues: results];
    
}
-(void) loginUserWithCompletionBlock :(NSString*) username password:(NSString*) password 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/login?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(username == nil) {
        // error
    }
    if(password == nil) {
        // error
    }
    if(username != nil) [requestUrl appendString:[NSString stringWithFormat:@"username=%@", username]];
    if(password != nil) [requestUrl appendString:[NSString stringWithFormat:@"password=%@", password]];
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
    
        completionBlock( [[NSString alloc]initWithValues: results], nil);}];
}


-(NSString*) loginUser :(NSString*) username
    password:(NSString*) password
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/login?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(username == nil) {
        // error
    }
    if(password == nil) {
        // error
    }
    if(username != nil) [requestUrl appendString:[NSString stringWithFormat:@"username=%@", username]];
    if(password != nil) [requestUrl appendString:[NSString stringWithFormat:@"password=%@", password]];
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

    return [[NSString alloc]initWithValues: results];
    
}
-(void) logoutUserWithCompletionBlock :
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/logout?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
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


-(void) logoutUser {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/user.{format}/logout?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
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
