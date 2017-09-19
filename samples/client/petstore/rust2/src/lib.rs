#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate serde_xml_rs;
extern crate futures;
extern crate chrono;
extern crate multipart;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

// Logically this should be in the client and server modules, but rust doesn't allow `macro_use` from a module.
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate hyper;

extern crate swagger;

use futures::Stream;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

pub use futures::Future;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

pub use swagger::{ApiError, Context, XSpanId};


#[derive(Debug, PartialEq)]
pub enum GetXmlFeaturesResponse {
    Success ( models::XmlObject ) ,
}

#[derive(Debug, PartialEq)]
pub enum PostXmlFeaturesResponse {
    Success ,
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterBooleanSerializeResponse {
    OutputBoolean ( models::OuterBoolean ) ,
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterCompositeSerializeResponse {
    OutputComposite ( models::OuterComposite ) ,
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterNumberSerializeResponse {
    OutputNumber ( models::OuterNumber ) ,
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterStringSerializeResponse {
    OutputString ( models::OuterString ) ,
}

#[derive(Debug, PartialEq)]
pub enum TestClientModelResponse {
    SuccessfulOperation ( models::Client ) ,
}

#[derive(Debug, PartialEq)]
pub enum TestEndpointParametersResponse {
    InvalidUsernameSupplied ,
    UserNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum TestEnumParametersResponse {
    InvalidRequest ,
    NotFound ,
}

#[derive(Debug, PartialEq)]
pub enum TestJsonFormDataResponse {
    SuccessfulOperation ,
}

#[derive(Debug, PartialEq)]
pub enum TestClassnameResponse {
    SuccessfulOperation ( models::Client ) ,
}

#[derive(Debug, PartialEq)]
pub enum AddPetResponse {
    InvalidInput ,
}

#[derive(Debug, PartialEq)]
pub enum DeletePetResponse {
    InvalidPetValue ,
}

#[derive(Debug, PartialEq)]
pub enum FindPetsByStatusResponse {
    SuccessfulOperation ( Vec<models::Pet> ) ,
    InvalidStatusValue ,
}

#[derive(Debug, PartialEq)]
pub enum FindPetsByTagsResponse {
    SuccessfulOperation ( Vec<models::Pet> ) ,
    InvalidTagValue ,
}

#[derive(Debug, PartialEq)]
pub enum GetPetByIdResponse {
    SuccessfulOperation ( models::Pet ) ,
    InvalidIDSupplied ,
    PetNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum UpdatePetResponse {
    InvalidIDSupplied ,
    PetNotFound ,
    ValidationException ,
}

#[derive(Debug, PartialEq)]
pub enum UpdatePetWithFormResponse {
    InvalidInput ,
}

#[derive(Debug, PartialEq)]
pub enum UploadFileResponse {
    SuccessfulOperation ( models::ApiResponse ) ,
}

#[derive(Debug, PartialEq)]
pub enum DeleteOrderResponse {
    InvalidIDSupplied ,
    OrderNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum GetInventoryResponse {
    SuccessfulOperation ( HashMap<String, i32> ) ,
}

#[derive(Debug, PartialEq)]
pub enum GetOrderByIdResponse {
    SuccessfulOperation ( models::Order ) ,
    InvalidIDSupplied ,
    OrderNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum PlaceOrderResponse {
    SuccessfulOperation ( models::Order ) ,
    InvalidOrder ,
}

#[derive(Debug, PartialEq)]
pub enum CreateUserResponse {
    SuccessfulOperation ,
}

#[derive(Debug, PartialEq)]
pub enum CreateUsersWithArrayInputResponse {
    SuccessfulOperation ,
}

#[derive(Debug, PartialEq)]
pub enum CreateUsersWithListInputResponse {
    SuccessfulOperation ,
}

#[derive(Debug, PartialEq)]
pub enum DeleteUserResponse {
    InvalidUsernameSupplied ,
    UserNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum GetUserByNameResponse {
    SuccessfulOperation ( models::User ) ,
    InvalidUsernameSupplied ,
    UserNotFound ,
}

#[derive(Debug, PartialEq)]
pub enum LoginUserResponse {
    SuccessfulOperation { body: String, x_rate_limit: i32, x_expires_after: chrono::DateTime<chrono::Utc> } ,
    InvalidUsername ,
}

#[derive(Debug, PartialEq)]
pub enum LogoutUserResponse {
    SuccessfulOperation ,
}

#[derive(Debug, PartialEq)]
pub enum UpdateUserResponse {
    InvalidUserSupplied ,
    UserNotFound ,
}


pub trait Api {

    /// Get some XML
    fn get_xml_features(&self, context: &Context) -> Box<Future<Item=GetXmlFeaturesResponse, Error=ApiError> + Send>;

    /// Post some xml
    fn post_xml_features(&self, xml_object: models::XmlObject, context: &Context) -> Box<Future<Item=PostXmlFeaturesResponse, Error=ApiError> + Send>;


    fn fake_outer_boolean_serialize(&self, body: Option<models::OuterBoolean>, context: &Context) -> Box<Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError> + Send>;


    fn fake_outer_composite_serialize(&self, body: Option<models::OuterComposite>, context: &Context) -> Box<Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError> + Send>;


    fn fake_outer_number_serialize(&self, body: Option<models::OuterNumber>, context: &Context) -> Box<Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError> + Send>;


    fn fake_outer_string_serialize(&self, body: Option<models::OuterString>, context: &Context) -> Box<Future<Item=FakeOuterStringSerializeResponse, Error=ApiError> + Send>;

    /// To test \"client\" model
    fn test_client_model(&self, body: models::Client, context: &Context) -> Box<Future<Item=TestClientModelResponse, Error=ApiError> + Send>;

    /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    fn test_endpoint_parameters(&self, number: f64, double: f64, pattern_without_delimiter: String, byte: swagger::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<swagger::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>, context: &Context) -> Box<Future<Item=TestEndpointParametersResponse, Error=ApiError> + Send>;

    /// To test enum parameters
    fn test_enum_parameters(&self, enum_form_string_array: Option<&Vec<String>>, enum_form_string: Option<String>, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, context: &Context) -> Box<Future<Item=TestEnumParametersResponse, Error=ApiError> + Send>;

    /// test json serialization of form data
    fn test_json_form_data(&self, param: String, param2: String, context: &Context) -> Box<Future<Item=TestJsonFormDataResponse, Error=ApiError> + Send>;

    /// To test class name in snake case
    fn test_classname(&self, body: models::Client, context: &Context) -> Box<Future<Item=TestClassnameResponse, Error=ApiError> + Send>;

    /// Add a new pet to the store
    fn add_pet(&self, body: models::Pet, context: &Context) -> Box<Future<Item=AddPetResponse, Error=ApiError> + Send>;

    /// Deletes a pet
    fn delete_pet(&self, pet_id: i64, api_key: Option<String>, context: &Context) -> Box<Future<Item=DeletePetResponse, Error=ApiError> + Send>;

    /// Finds Pets by status
    fn find_pets_by_status(&self, status: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByStatusResponse, Error=ApiError> + Send>;

    /// Finds Pets by tags
    fn find_pets_by_tags(&self, tags: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByTagsResponse, Error=ApiError> + Send>;

    /// Find pet by ID
    fn get_pet_by_id(&self, pet_id: i64, context: &Context) -> Box<Future<Item=GetPetByIdResponse, Error=ApiError> + Send>;

    /// Update an existing pet
    fn update_pet(&self, body: models::Pet, context: &Context) -> Box<Future<Item=UpdatePetResponse, Error=ApiError> + Send>;

    /// Updates a pet in the store with form data
    fn update_pet_with_form(&self, pet_id: i64, name: Option<String>, status: Option<String>, context: &Context) -> Box<Future<Item=UpdatePetWithFormResponse, Error=ApiError> + Send>;

    /// uploads an image
    fn upload_file(&self, pet_id: i64, additional_metadata: Option<String>, file: Box<Future<Item=Option<Box<Stream<Item=Vec<u8>, Error=Error> + Send>>, Error=Error> + Send>, context: &Context) -> Box<Future<Item=UploadFileResponse, Error=ApiError> + Send>;

    /// Delete purchase order by ID
    fn delete_order(&self, order_id: String, context: &Context) -> Box<Future<Item=DeleteOrderResponse, Error=ApiError> + Send>;

    /// Returns pet inventories by status
    fn get_inventory(&self, context: &Context) -> Box<Future<Item=GetInventoryResponse, Error=ApiError> + Send>;

    /// Find purchase order by ID
    fn get_order_by_id(&self, order_id: i64, context: &Context) -> Box<Future<Item=GetOrderByIdResponse, Error=ApiError> + Send>;

    /// Place an order for a pet
    fn place_order(&self, body: models::Order, context: &Context) -> Box<Future<Item=PlaceOrderResponse, Error=ApiError> + Send>;

    /// Create user
    fn create_user(&self, body: models::User, context: &Context) -> Box<Future<Item=CreateUserResponse, Error=ApiError> + Send>;

    /// Creates list of users with given input array
    fn create_users_with_array_input(&self, body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError> + Send>;

    /// Creates list of users with given input array
    fn create_users_with_list_input(&self, body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithListInputResponse, Error=ApiError> + Send>;

    /// Delete user
    fn delete_user(&self, username: String, context: &Context) -> Box<Future<Item=DeleteUserResponse, Error=ApiError> + Send>;

    /// Get user by user name
    fn get_user_by_name(&self, username: String, context: &Context) -> Box<Future<Item=GetUserByNameResponse, Error=ApiError> + Send>;

    /// Logs user into the system
    fn login_user(&self, username: String, password: String, context: &Context) -> Box<Future<Item=LoginUserResponse, Error=ApiError> + Send>;

    /// Logs out current logged in user session
    fn logout_user(&self, context: &Context) -> Box<Future<Item=LogoutUserResponse, Error=ApiError> + Send>;

    /// Updated user
    fn update_user(&self, username: String, body: models::User, context: &Context) -> Box<Future<Item=UpdateUserResponse, Error=ApiError> + Send>;

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use self::client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::router;

pub mod models;
