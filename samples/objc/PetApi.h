#import <Foundation/Foundation.h>
#import "Pet.h"


@interface PetApi: NSObject {

@private
    NSOperationQueue *_queue;
}
@property(nonatomic, readonly) NSOperationQueue* queue;

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
