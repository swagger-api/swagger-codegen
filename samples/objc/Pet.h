#import <Foundation/Foundation.h>
#import "Category.h"
#import "Tag.h"

@interface Pet : NSObject {
@private
    NSNumber* __id;
    NSArray* _tags;
    Category* _category;
    NSString* _status;
    NSString* _name;
    NSArray* _photoUrls;
    }


@property(nonatomic, readonly) NSNumber* _id;
@property(nonatomic, readonly) NSArray* tags;
@property(nonatomic, readonly) Category* category;
@property(nonatomic, readonly) NSString* status;
@property(nonatomic, readonly) NSString* name;
@property(nonatomic, readonly) NSArray* photoUrls;
- (id) _id: (NSNumber*) _id
     tags: (NSArray*) tags
     category: (Category*) category
     status: (NSString*) status
     name: (NSString*) name
     photoUrls: (NSArray*) photoUrls;

- (id) initWithValues: (NSDictionary*)dict;


@end

