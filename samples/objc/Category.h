#import <Foundation/Foundation.h>

@interface Category : NSObject {
@private
    NSNumber* __id;
    NSString* _name;
    }


@property(nonatomic, readonly) NSNumber* _id;
@property(nonatomic, readonly) NSString* name;
- (id) _id: (NSNumber*) _id
     name: (NSString*) name;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

