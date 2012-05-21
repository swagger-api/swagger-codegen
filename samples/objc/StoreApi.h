#import <Foundation/Foundation.h>
#import "ApiInvoker.h"
#import "Order.h"


@interface StoreApi: NSObject {

@private
    NSOperationQueue *_queue;
    ApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) ApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(Order*) getOrderById :(NSString*) orderId ;
-(void) getOrderByIdWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(Order*, NSError *))completionBlock;
-(void) deleteOrder :(NSString*) orderId ;
-(void) deleteOrderWithCompletionBlock :(NSString*) orderId 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) placeOrder :(Order*) body ;
-(void) placeOrderWithCompletionBlock :(Order*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
@end
