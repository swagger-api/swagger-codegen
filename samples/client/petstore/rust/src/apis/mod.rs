use hyper;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Hyper(hyper::Error),
    Serde(serde_json::Error),
}

impl From<hyper::Error> for Error {
    fn from(e: hyper::Error) -> Self {
        return Error::Hyper(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        return Error::Serde(e)
    }
}

use super::models::*;

mod pet_api;
pub use self::pet_api::PetApi;
pub use self::pet_api::PetApiImpl;
mod store_api;
pub use self::store_api::StoreApi;
pub use self::store_api::StoreApiImpl;
mod user_api;
pub use self::user_api::UserApi;
pub use self::user_api::UserApiImpl;

pub mod configuration;
pub mod client;
