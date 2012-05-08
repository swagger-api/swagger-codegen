#import <Foundation/Foundation.h>
#import "Date.h"

@interface Order : NSObject {
@private
    NSNumber* __id;
    NSNumber* _petId;
    NSString* _status;
    NSNumber* _quantity;
    Date* _shipDate;
    }


@property(nonatomic, readonly) NSNumber* _id;
@property(nonatomic, readonly) NSNumber* petId;
@property(nonatomic, readonly) NSString* status;
@property(nonatomic, readonly) NSNumber* quantity;
@property(nonatomic, readonly) Date* shipDate;
- (id) _id: (NSNumber*) _id
     petId: (NSNumber*) petId
     status: (NSString*) status
     quantity: (NSNumber*) quantity
     shipDate: (Date*) shipDate;

- (id) initWithValues: (NSDictionary*)dict;


@end

