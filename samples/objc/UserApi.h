#import <Foundation/Foundation.h>
#import "User.h"


@interface UserApi: NSObject

-(void) createUser :(User*) body ;

-(void) createUsersWithArrayInput :(NSArray*) body ;

-(void) createUsersWithListInput :(NSArray*) body ;

-(void) updateUser :(NSString*) username body:(User*) body ;

-(void) deleteUser :(NSString*) username ;

-(User*) getUserByName :(NSString*) username ;

-(NSString*) loginUser :(NSString*) username password:(NSString*) password ;

-(void) logoutUser ;

@end
