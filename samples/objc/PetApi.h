#import <Foundation/Foundation.h>
#import "ApiInvoker.h"
#import "Pet.h"


@interface PetApi: NSObject {

@private
    NSOperationQueue *_queue;
    ApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) ApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(Pet*) getPetById :(NSString*) petId ;
-(void) getPetByIdWithCompletionBlock :(NSString*) petId 
        completionHandler:(void (^)(Pet*, NSError *))completionBlock;
-(void) addPet :(Pet*) body ;
-(void) addPetWithCompletionBlock :(Pet*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) updatePet :(Pet*) body ;
-(void) updatePetWithCompletionBlock :(Pet*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(NSArray*) findPetsByStatus :(NSString*) status ;
-(void) findPetsByStatusWithCompletionBlock :(NSString*) status 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
-(NSArray*) findPetsByTags :(NSString*) tags ;
-(void) findPetsByTagsWithCompletionBlock :(NSString*) tags 
        completionHandler:(void (^)(NSArray*, NSError *))completionBlock;
@end
