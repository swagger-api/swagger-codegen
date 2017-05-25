@import Foundation.NSError;
@import Foundation.NSPointerArray;

#if TARGET_OS_IPHONE
    #define NSPointerArrayMake(N) ({ \
        NSPointerArray *aa = [NSPointerArray strongObjectsPointerArray]; \
        aa.count = N; \
        aa; \
    })
#else
    static inline NSPointerArray * __nonnull NSPointerArrayMake(NSUInteger count) {
      #pragma clang diagnostic push
      #pragma clang diagnostic ignored "-Wdeprecated-declarations"
        NSPointerArray *aa = [[NSPointerArray class] respondsToSelector:@selector(strongObjectsPointerArray)]
            ? [NSPointerArray strongObjectsPointerArray]
            : [NSPointerArray pointerArrayWithStrongObjects];
      #pragma clang diagnostic pop
        aa.count = count;
        return aa;
    }
#endif

#define IsError(o) [o isKindOfClass:[NSError class]]
#define IsPromise(o) [o isKindOfClass:[AnyPromise class]]

#import "AnyPromise.h"

@interface AnyPromise (Swift)
- (void)pipe:(void (^ __nonnull)(id __nonnull))body;
- (AnyPromise * __nonnull)initWithBridge:(void (^ __nonnull)(PMKResolver __nonnull))resolver;
@end

extern NSError * __nullable PMKProcessUnhandledException(id __nonnull thrown);

@interface NSError (PMKUnhandledErrorHandler)
- (void)pmk_consume;
@end
