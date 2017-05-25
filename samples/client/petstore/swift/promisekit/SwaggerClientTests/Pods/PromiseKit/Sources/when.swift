import Foundation.NSProgress

private func _when<T>(promises: [Promise<T>]) -> Promise<Void> {
    let (rootPromise, fulfill, reject) = Promise<Void>.pendingPromise()
#if !PMKDisableProgress
    let progress = NSProgress(totalUnitCount: Int64(promises.count))
    progress.cancellable = false
    progress.pausable = false
#else
    var progress: (completedUnitCount: Int, totalUnitCount: Int) = (0, 0)
#endif
    var countdown = promises.count
    if countdown == 0 {
        fulfill()
        return rootPromise
    }
    let barrier = dispatch_queue_create("org.promisekit.barrier.when", DISPATCH_QUEUE_CONCURRENT)

    for (index, promise) in promises.enumerate() {
        promise.pipe { resolution in
            dispatch_barrier_sync(barrier) {
                switch resolution {
                case .Rejected(let error, let token):
                    token.consumed = true   // all errors are consumed by the parent Error.When
                    if rootPromise.pending {
                        progress.completedUnitCount = progress.totalUnitCount
                        reject(Error.When(index, error))
                    }
                case .Fulfilled:
                    guard rootPromise.pending else { return }
                    progress.completedUnitCount += 1
                    countdown -= 1
                    if countdown == 0 {
                        fulfill()
                    }
                }
            }
        }
    }

    return rootPromise
}

/**
 Wait for all promises in a set to resolve.

 For example:

     when(promise1, promise2).then { results in
         //…
     }.error { error in
         switch error {
         case Error.When(let index, NSURLError.NoConnection):
             //…
         case Error.When(let index, CLError.NotAuthorized):
             //…
         }
     }

 - Warning: If *any* of the provided promises reject, the returned promise is immediately rejected with that promise’s rejection. The error’s `userInfo` object is supplemented with `PMKFailingPromiseIndexKey`.
 - Warning: In the event of rejection the other promises will continue to resolve and, as per any other promise, will either fulfill or reject. This is the right pattern for `getter` style asynchronous tasks, but often for `setter` tasks (eg. storing data on a server), you most likely will need to wait on all tasks and then act based on which have succeeded and which have failed, in such situations use `join`.
 - Parameter promises: The promises upon which to wait before the returned promise resolves.
 - Returns: A new promise that resolves when all the provided promises fulfill or one of the provided promises rejects.
 - SeeAlso: `join()`
*/
public func when<T>(promises: [Promise<T>]) -> Promise<[T]> {
    return _when(promises).then(on: zalgo) { promises.map{ $0.value! } }
}

public func when<T>(promises: Promise<T>...) -> Promise<[T]> {
    return when(promises)
}

public func when(promises: Promise<Void>...) -> Promise<Void> {
    return _when(promises)
}

public func when(promises: [Promise<Void>]) -> Promise<Void> {
    return _when(promises)
}

public func when<U, V>(pu: Promise<U>, _ pv: Promise<V>) -> Promise<(U, V)> {
    return _when([pu.asVoid(), pv.asVoid()]).then(on: zalgo) { (pu.value!, pv.value!) }
}

public func when<U, V, X>(pu: Promise<U>, _ pv: Promise<V>, _ px: Promise<X>) -> Promise<(U, V, X)> {
    return _when([pu.asVoid(), pv.asVoid(), px.asVoid()]).then(on: zalgo) { (pu.value!, pv.value!, px.value!) }
}

/**
 Generate promises at a limited rate and wait for all to resolve.

 For example:
 
     func downloadFile(url: NSURL) -> Promise<NSData> {
         // ...
     }
 
     let urls: [NSURL] = /*…*/
     let urlGenerator = urls.generate()

     let generator = AnyGenerator<Promise<NSData>> {
         guard url = urlGenerator.next() else {
             return nil
         }

         return downloadFile(url)
     }

     when(generator, concurrently: 3).then { datum: [NSData] -> Void in
         // ...
     }

 - Warning: Refer to the warnings on `when(fulfilled:)`
 - Parameter promiseGenerator: Generator of promises.
 - Returns: A new promise that resolves when all the provided promises fulfill or one of the provided promises rejects.
 - SeeAlso: `join()`
 */

public func when<T, PromiseGenerator: GeneratorType where PromiseGenerator.Element == Promise<T> >(promiseGenerator: PromiseGenerator, concurrently: Int = 1) -> Promise<[T]> {

    guard concurrently > 0 else {
        return Promise(error: Error.WhenConcurrentlyZero)
    }

    var generator = promiseGenerator
    var root = Promise<[T]>.pendingPromise()
    var pendingPromises = 0
    var promises: [Promise<T>] = []

    let barrier = dispatch_queue_create("org.promisekit.barrier.when", DISPATCH_QUEUE_CONCURRENT)

    func dequeue() {
        var shouldDequeue = false
        dispatch_sync(barrier) {
            shouldDequeue = pendingPromises < concurrently
        }
        guard shouldDequeue else { return }

        var index: Int!
        var promise: Promise<T>!

        dispatch_barrier_sync(barrier) {
            guard let next = generator.next() else { return }

            promise = next
            index = promises.count

            pendingPromises += 1
            promises.append(next)
        }

        func testDone() {
            dispatch_sync(barrier) {
                if pendingPromises == 0 {
                    root.fulfill(promises.flatMap{ $0.value })
                }
            }
        }

        guard promise != nil else {
            return testDone()
        }

        promise.pipe { resolution in
            dispatch_barrier_sync(barrier) {
                pendingPromises -= 1
            }

            switch resolution {
            case .Fulfilled:
                dequeue()
                testDone()
            case .Rejected(let error, let token):
                token.consumed = true
                root.reject(Error.When(index, error))
            }
        }

        dequeue()
    }
        
    dequeue()

    return root.promise
}
