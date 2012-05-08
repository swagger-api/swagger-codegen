#import <Foundation/Foundation.h>

@interface Tag : NSObject {
@private
    NSNumber* __id;
    NSString* _name;
    }


@property(nonatomic, readonly) NSNumber* _id;
@property(nonatomic, readonly) NSString* name;
- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;


@end

