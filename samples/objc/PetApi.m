#import "PetApi.h"
#import "Pet.h"



@implementation PetApi
static NSString * basePath = @"http://petstore.swagger.wordnik.com/api";

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
