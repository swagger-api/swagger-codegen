#import <Foundation/Foundation.h>
#import "ApiInvoker.h"
#import "User.h"


@interface UserApi: NSObject {

@private
    NSOperationQueue *_queue;
    ApiInvoker * _api;
}
@property(nonatomic, readonly) NSOperationQueue* queue;
@property(nonatomic, readonly) ApiInvoker* api;

-(void) addHeader:(NSString*) value
           forKey:(NSString*)key;

-(void) createUsersWithArrayInput :(NSArray*) body ;
-(void) createUsersWithArrayInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) createUser :(User*) body ;
-(void) createUserWithCompletionBlock :(User*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) createUsersWithListInput :(NSArray*) body ;
-(void) createUsersWithListInputWithCompletionBlock :(NSArray*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) updateUser :(NSString*) username body:(User*) body ;
-(void) updateUserWithCompletionBlock :(NSString*) username body:(User*) body 
        completionHandler:(void (^)(NSError *))completionBlock;
-(void) deleteUser :(NSString*) username ;
-(void) deleteUserWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(NSError *))completionBlock;
-(User*) getUserByName :(NSString*) username ;
-(void) getUserByNameWithCompletionBlock :(NSString*) username 
        completionHandler:(void (^)(User*, NSError *))completionBlock;
-(NSString*) loginUser :(NSString*) username password:(NSString*) password ;
-(void) loginUserWithCompletionBlock :(NSString*) username password:(NSString*) password 
        completionHandler:(void (^)(NSString*, NSError *))completionBlock;
-(void) logoutUser ;
-(void) logoutUserWithCompletionBlock :
        completionHandler:(void (^)(NSError *))completionBlock;
@end
