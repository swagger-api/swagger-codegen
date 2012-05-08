#import <Foundation/Foundation.h>

@interface Date : NSObject{
@private
    NSDate *_date;
}
@property(nonatomic, readonly) NSDate* date;

- (id) initWithValues: (NSNumber*)input;
@end

