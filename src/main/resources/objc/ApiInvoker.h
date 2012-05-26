#import <Foundation/Foundation.h>

@interface ApiInvoker : NSObject {
    
@private
    NSOperationQueue *_queue;
    NSMutableDictionary * _defaultHeaders;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NSMutableDictionary * defaultHeaders;



-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(NSString*) escapeString:(NSString*) string;

-(id) invokeWithCompletionBlock:(NSString*) path
                         method:(NSString*) method
                    queryParams:(NSDictionary*) queryParams
                           body:(NSDictionary*)body
                   headerParams:(NSDictionary*) headerParams
              completionHandler:(void (^)(NSDictionary*, NSError *))completionBlock;
@end
