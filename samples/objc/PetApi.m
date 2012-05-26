#import "PetApi.h"
#import "Pet.h"



@implementation PetApi
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

-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(Pet*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/{petId}", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString: [_api escapeString:petId]];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(petId == nil) {
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
        
        completionBlock( [[Pet alloc]initWithValues: data], nil);
    }];
}


-(Pet*) getPetById :(NSString*) petId
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/{petId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString:petId];
    if(petId == nil) {
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

    return [[Pet alloc]initWithValues: results];
    
}
-(void) addPetWithCompletionBlock :(Pet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

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


-(void) addPet :(Pet*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}?", basePath];

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
-(void) updatePetWithCompletionBlock :(Pet*) body 
        completionHandler:(void (^)(NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}", basePath];

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
                             method: @"PUT" 
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


-(void) updatePet :(Pet*) body
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}?", basePath];

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
-(void) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByStatus", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(status != nil)
        [queryParams setValue:status forKey:@"status"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(status == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in data) {
                Pet* d = [[Pet alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        
    }];
}


-(NSArray*) findPetsByStatus :(NSString*) status
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByStatus?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(status == nil) {
        // error
    }
    if(status != nil) [requestUrl appendString:[NSString stringWithFormat:@"status=%@", status]];
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

    if([results isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[results count]];
        for (NSDictionary* dict in results) {
            Pet* d = [[Pet alloc]initWithValues: dict];
            [objs addObject:d];
        }
        return objs;
    }
    
}
-(void) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByTags", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    NSMutableDictionary* queryParams = [[NSMutableDictionary alloc] init];
    if(tags != nil)
        [queryParams setValue:tags forKey:@"tags"];
    NSMutableDictionary* headerParams = [[NSMutableDictionary alloc] init];
    NSDictionary* bodyDictionary = nil;
		if(tags == nil) {
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
        
        if([data isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[data count]];
            for (NSDictionary* dict in data) {
                Pet* d = [[Pet alloc]initWithValues: dict];
                [objs addObject:d];
            }
            completionBlock(objs, nil);
        }
        
    }];
}


-(NSArray*) findPetsByTags :(NSString*) tags
    {
    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByTags?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(tags == nil) {
        // error
    }
    if(tags != nil) [requestUrl appendString:[NSString stringWithFormat:@"tags=%@", tags]];
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

    if([results isKindOfClass:[NSArray class]]){
        NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[results count]];
        for (NSDictionary* dict in results) {
            Pet* d = [[Pet alloc]initWithValues: dict];
            [objs addObject:d];
        }
        return objs;
    }
    
}

@end
