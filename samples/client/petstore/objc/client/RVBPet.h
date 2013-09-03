#import <Foundation/Foundation.h>
#import "NIKSwaggerObject.h"
#import "RVBRVBCategory*.h"
#import "RVBRVBTag*.h"
#import "RVBCategory.h"
#import "RVBTag.h"

@interface RVBPet : NIKSwaggerObject

@property(nonatomic) NSString* name;
@property(nonatomic) NSNumber* _id;
@property(nonatomic) NSArray* tags;
@property(nonatomic) NSString* status;
@property(nonatomic) RVBCategory* category;
@property(nonatomic) NSArray* photoUrls;
- (id) name: (NSString*) name
     _id: (NSNumber*) _id
     tags: (NSArray*) tags
     status: (NSString*) status
     category: (RVBCategory*) category
     photoUrls: (NSArray*) photoUrls;

- (id) initWithValues: (NSDictionary*)dict;
- (NSDictionary*) asDictionary;


@end

