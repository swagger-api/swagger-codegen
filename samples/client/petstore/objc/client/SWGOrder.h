#import <Foundation/Foundation.h>
#import "SWGObject.h"


@interface SWGOrder : SWGObject


@property(nonatomic) NSNumber* _id;  

@property(nonatomic) NSNumber* petId;  

@property(nonatomic) NSNumber* quantity;  

@property(nonatomic) NSDate* shipDate;  
/* Order Status  */
@property(nonatomic) NSString* status;  

@property(nonatomic) NSNumber* complete;  

@end
