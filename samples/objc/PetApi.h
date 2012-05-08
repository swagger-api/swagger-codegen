#import <Foundation/Foundation.h>
#import "Pet.h"


@interface PetApi: NSObject

-(Pet*) getPetById :(NSString*) petId ;

-(void) addPet :(Pet*) body ;

-(void) updatePet :(Pet*) body ;

-(NSArray*) findPetsByStatus :(NSString*) status ;

-(NSArray*) findPetsByTags :(NSString*) tags ;

@end
