import Foundation.NSError
import UIKit
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import this `UIViewController` category:

    use_frameworks!
    pod "PromiseKit/UIKit"

 Or `UIKit` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension UIViewController {

    public enum PMKError: ErrorType {
        case NavigationControllerEmpty
        case NoImageFound
        case NotPromisable
        case NotGenericallyPromisable
        case NilPromisable
    }

    public enum FulfillmentType {
        case OnceDisappeared
        case BeforeDismissal
    }

    @available(*, deprecated=3.4, renamed="promiseViewController(_:animate:fulfills:completion:)")
    public func promiseViewController<T>(vc: UIViewController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<T> {
        return promiseViewController(vc, animate: [.Appear, .Disappear], completion: completion)
    }

    public func promiseViewController<T>(vc: UIViewController, animate animationOptions: PMKAnimationOptions = [.Appear, .Disappear], fulfills fulfillmentType: FulfillmentType = .OnceDisappeared, completion: (() -> Void)? = nil) -> Promise<T> {

        let pvc: UIViewController

        switch vc {
        case let nc as UINavigationController:
            guard let vc = nc.viewControllers.first else { return Promise(error: PMKError.NavigationControllerEmpty) }
            pvc = vc
        default:
            pvc = vc
        }

        let promise: Promise<T>

        if !pvc.conformsToProtocol(Promisable) {
            promise = Promise(error: PMKError.NotPromisable)
        } else if let p = pvc.valueForKeyPath("promise") as? Promise<T> {
            promise = p
        } else if let _: AnyObject = pvc.valueForKeyPath("promise") {
            promise = Promise(error: PMKError.NotGenericallyPromisable)
        } else {
            promise = Promise(error: PMKError.NilPromisable)
        }

        if !promise.pending {
            return promise
        }

        presentViewController(vc, animated: animationOptions.contains(.Appear), completion: completion)

        let (wrappingPromise, fulfill, reject) = Promise<T>.pendingPromise()

        switch fulfillmentType {
        case .OnceDisappeared:
            promise.then { result in
                vc.presentingViewController?.dismissViewControllerAnimated(animationOptions.contains(.Disappear), completion: { fulfill(result) })
            }
            .error(policy: .AllErrors) { error in
                vc.presentingViewController?.dismissViewControllerAnimated(animationOptions.contains(.Disappear), completion: { reject(error) })
            }
        case .BeforeDismissal:
            promise.then { result -> Void in
                fulfill(result)
                vc.presentingViewController?.dismissViewControllerAnimated(animationOptions.contains(.Disappear), completion: nil)
            }
            .error(policy: .AllErrors) { error in
                reject(error)
                vc.presentingViewController?.dismissViewControllerAnimated(animationOptions.contains(.Disappear), completion: nil)
            }
        }

        return wrappingPromise
    }

    public func promiseViewController(vc: UIImagePickerController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<UIImage> {
        let proxy = UIImagePickerControllerProxy()
        vc.delegate = proxy
        vc.mediaTypes = ["public.image"]  // this promise can only resolve with a UIImage
        presentViewController(vc, animated: animated, completion: completion)
        return proxy.promise.then(on: zalgo) { info -> UIImage in
            if let img = info[UIImagePickerControllerEditedImage] as? UIImage {
                return img
            }
            if let img = info[UIImagePickerControllerOriginalImage] as? UIImage {
                return img
            }
            throw PMKError.NoImageFound
        }.always {
            vc.presentingViewController?.dismissViewControllerAnimated(animated, completion: nil)
        }
    }

    public func promiseViewController(vc: UIImagePickerController, animated: Bool = true, completion: (() -> Void)? = nil) -> Promise<[String: AnyObject]> {
        let proxy = UIImagePickerControllerProxy()
        vc.delegate = proxy
        presentViewController(vc, animated: animated, completion: completion)
        return proxy.promise.always {
            vc.presentingViewController?.dismissViewControllerAnimated(animated, completion: nil)
        }
    }
}

@objc(Promisable) public protocol Promisable {
    /**
     Provide a promise for promiseViewController here.

     The resulting property must be annotated with @objc.

     Obviously return a Promise<T>. There is an issue with generics and Swift and
     protocols currently so we couldn't specify that.
    */
    var promise: AnyObject! { get }
}


// internal scope because used by ALAssetsLibrary extension
@objc class UIImagePickerControllerProxy: NSObject, UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    let (promise, fulfill, reject) = Promise<[String : AnyObject]>.pendingPromise()
    var retainCycle: AnyObject?

    required override init() {
        super.init()
        retainCycle = self
    }

    func imagePickerController(picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : AnyObject]) {
        fulfill(info)
        retainCycle = nil
    }

    func imagePickerControllerDidCancel(picker: UIImagePickerController) {
        reject(UIImagePickerController.Error.Cancelled)
        retainCycle = nil
    }
}


extension UIImagePickerController {
    public enum Error: CancellableErrorType {
        case Cancelled

        public var cancelled: Bool {
            switch self {
                case .Cancelled: return true
            }
        }
    }
}
