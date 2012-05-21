#import "PetApi.h"
#import "Pet.h"



@implementation PetApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

@synthesize queue = _queue;

- (id) init {
    [super init];
    _queue = [[NSOperationQueue alloc] init];
    return self;
}

-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(Pet*, NSError *))completionBlock{

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/{petId}?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:[NSString stringWithFormat:@"%@%@%@", @"{", @"petId", @"}"]] withString:petId];
    if(petId == nil) {
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
    
        completionBlock( [[Pet alloc]initWithValues: results], nil);}];
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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}?", basePath];

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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}?", basePath];

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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByStatus?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(status == nil) {
        // error
    }
    if(status != nil) [requestUrl appendString:[NSString stringWithFormat:@"status=%@", status]];
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
    
        if([results isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[results count]];
            for (NSDictionary* dict in results) {
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

    NSMutableString* requestUrl = [NSMutableString stringWithFormat:@"%@/pet.{format}/findByTags?", basePath];

    // remove format in URL
    [requestUrl replaceCharactersInRange: [requestUrl rangeOfString:@".{format}"] withString:@".json"];
    if(tags == nil) {
        // error
    }
    if(tags != nil) [requestUrl appendString:[NSString stringWithFormat:@"tags=%@", tags]];
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
    
        if([results isKindOfClass:[NSArray class]]){
            NSMutableArray * objs = [[NSMutableArray alloc] initWithCapacity:[results count]];
            for (NSDictionary* dict in results) {
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
