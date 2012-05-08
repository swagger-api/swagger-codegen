#import <Foundation/Foundation.h>
#import "Order.h"


@interface StoreApi: NSObject

-(Order*) getOrderById :(NSString*) orderId ;

-(void) deleteOrder :(NSString*) orderId ;

-(void) placeOrder :(Order*) body ;

@end
