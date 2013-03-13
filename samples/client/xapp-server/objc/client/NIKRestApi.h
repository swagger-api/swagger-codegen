#import <Foundation/Foundation.h>
#import "NIKApiInvoker.h"
#import "NIKTerminateReply.h"
#import "NIKAdResponseCall.h"
#import "NIKInitializeCall.h"
#import "NIKTerminateCall.h"
#import "NIKAdRequestReply.h"
#import "NIKAdResponseReply.h"
#import "NIKInitializeReply.h"
#import "NIKAdRequestCall.h"


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
 
 @param body 
 */
-(void) initializeWithCompletionBlock :(NIKInitializeCall*) body 
        completionHandler: (void (^)(NIKInitializeReply* output, NSError* error))completionBlock;

/**

 Terminates a session.
 
 @param body 
 */
-(void) terminateWithCompletionBlock :(NIKTerminateCall*) body 
        completionHandler: (void (^)(NIKTerminateReply* output, NSError* error))completionBlock;

/**

 Provides information about an ad to be played.
 
 @param body 
 */
-(void) adRequestWithCompletionBlock :(NIKAdRequestCall*) body 
        completionHandler: (void (^)(NIKAdRequestReply* output, NSError* error))completionBlock;

/**

 Receives information about how the listener interacted with the ad.
 
 @param body 
 */
-(void) adResponseWithCompletionBlock :(NIKAdResponseCall*) body 
        completionHandler: (void (^)(NIKAdResponseReply* output, NSError* error))completionBlock;

@end
