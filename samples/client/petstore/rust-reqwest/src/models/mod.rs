mod amount;
pub use self::amount::Amount;
mod api_response;
pub use self::api_response::ApiResponse;
mod category;
pub use self::category::Category;
mod currency;
pub use self::currency::Currency;
mod order;
pub use self::order::Order;
mod pet;
pub use self::pet::Pet;
mod tag;
pub use self::tag::Tag;
mod user;
pub use self::user::User;

// TODO(farcaller): sort out files
pub struct File;
