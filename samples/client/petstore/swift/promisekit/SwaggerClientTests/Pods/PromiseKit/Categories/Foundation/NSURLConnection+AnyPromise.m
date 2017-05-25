#import "NSURLConnection+AnyPromise.h"
#import <OMGHTTPURLRQ/OMGHTTPURLRQ.h>  // github "mxcl/OMGHTTPURLRQ" ~> 3.2
#import <Foundation/NSOperation.h>
#import <dispatch/once.h>

typedef void (^PMKURLDataCompletionHandler)(NSData *, NSURLResponse *, NSError *);
PMKURLDataCompletionHandler PMKMakeURLDataHandler(NSURLRequest *, PMKResolver);
extern id PMKURLRequestFromURLFormat(NSError **err, id urlFormat, ...);


@implementation NSURLConnection (PromiseKit)

+ (AnyPromise *)GET:(id)urlFormat, ... {
    id err;
    id rq = PMKURLRequestFromURLFormat(&err, urlFormat);
    if (err) {
        return [AnyPromise promiseWithValue:err];
    } else {
        return [self promise:rq];
    }
}

+ (AnyPromise *)GET:(NSString *)url query:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ GET:url:params error:&err];
    if (err) return [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)POST:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ POST:url:params error:&err];
    if (err) return [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)POST:(NSString *)urlString JSON:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ POST:urlString JSON:params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)PUT:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ PUT:url:params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];

}

+ (AnyPromise *)DELETE:(NSString *)url formURLEncodedParameters:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ DELETE:url :params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)PATCH:(NSString *)url JSON:(NSDictionary *)params {
    id err;
    id rq = [OMGHTTPURLRQ PATCH:url JSON:params error:&err];
    if (err) [AnyPromise promiseWithValue:err];
    return [self promise:rq];
}

+ (AnyPromise *)promise:(NSURLRequest *)rq {
    static id q = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        q = [NSOperationQueue new];
    });

    return [AnyPromise promiseWithResolverBlock:^(PMKResolver resolve) {
        [NSURLConnection sendAsynchronousRequest:rq queue:q completionHandler:^(NSURLResponse *response, NSData *data, NSError *connectionError) {
            PMKMakeURLDataHandler(rq, resolve)(data, response, connectionError);
        }];
    }];
}

@end
