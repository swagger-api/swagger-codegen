#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKTerminateReply.h"
#import "NIKAdRequestReply.h"
#import "NIKAdResponseReply.h"
#import "NIKInitializeReply.h"


@interface NIKRestApi: NSObject {

@private
    NSOperationQueue *_queue;
    NIKApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) NIKApiInvoker* api;

-(void) addHeader:(NSString*)value forKey:(NSString*)key;

/**

 Initializes the XappSession. Allows ads to be served.
 
 @param  
 */
-(void) initializeWithCompletionBlock :(NIKInitializeCall*)  
        completionHandler: (void (^)(NIKInitializeReply* output, NSError* error))completionBlock;

/**

 Terminates a session.
 
 @param  
 */
-(void) terminateWithCompletionBlock :(NIKTerminateCall*)  
        completionHandler: (void (^)(NIKTerminateReply* output, NSError* error))completionBlock;

/**

 Provides information about an ad to be played.
 
 @param  
 */
-(void) adRequestWithCompletionBlock :(NIKAdRequestCall*)  
        completionHandler: (void (^)(NIKAdRequestReply* output, NSError* error))completionBlock;

/**

 Receives information about how the listener interacted with the ad.
 
 @param  
 */
-(void) adResponseWithCompletionBlock :(NIKAdResponseCall*)  
        completionHandler: (void (^)(NIKAdResponseReply* output, NSError* error))completionBlock;

@end
