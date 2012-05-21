#import <Foundation/Foundation.h>
#import "Order.h"


@interface StoreApi: NSObject {

@private
    NSOperationQueue *_queue;
}
@property(nonatomic, readonly) NSOperationQueue* queue;

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
