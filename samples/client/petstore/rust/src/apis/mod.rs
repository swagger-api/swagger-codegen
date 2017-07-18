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

use super::models;

mod pet_api;
///Add a new pet to the store
pub use self::pet_api::AddPet;
///Deletes a pet
pub use self::pet_api::DeletePet;
///Finds Pets by status
pub use self::pet_api::FindPetsByStatus;
///Finds Pets by tags
pub use self::pet_api::FindPetsByTags;
///Find pet by ID
pub use self::pet_api::GetPetById;
///Update an existing pet
pub use self::pet_api::UpdatePet;
///Updates a pet in the store with form data
pub use self::pet_api::UpdatePetWithForm;
///uploads an image
pub use self::pet_api::UploadFile;

pub use self::pet_api::PetApi;

mod store_api;
///Delete purchase order by ID
pub use self::store_api::DeleteOrder;
///Returns pet inventories by status
pub use self::store_api::GetInventory;
///Find purchase order by ID
pub use self::store_api::GetOrderById;
///Place an order for a pet
pub use self::store_api::PlaceOrder;

pub use self::store_api::StoreApi;

mod user_api;
///Create user
pub use self::user_api::CreateUser;
///Creates list of users with given input array
pub use self::user_api::CreateUsersWithArrayInput;
///Creates list of users with given input array
pub use self::user_api::CreateUsersWithListInput;
///Delete user
pub use self::user_api::DeleteUser;
///Get user by user name
pub use self::user_api::GetUserByName;
///Logs user into the system
pub use self::user_api::LoginUser;
///Logs out current logged in user session
pub use self::user_api::LogoutUser;
///Updated user
pub use self::user_api::UpdateUser;

pub use self::user_api::UserApi;


pub mod configuration;
pub mod client;
