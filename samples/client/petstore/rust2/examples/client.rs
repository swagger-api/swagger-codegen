#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate petstore_api;
#[allow(unused_extern_crates)]
extern crate futures;
#[allow(unused_extern_crates)]
extern crate swagger;
#[allow(unused_extern_crates)]
extern crate uuid;
extern crate clap;

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use petstore_api::{Api,
                      ApiError,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
                      TestClientModelResponse,
                      TestEndpointParametersResponse,
                      TestEnumParametersResponse,
                      TestJsonFormDataResponse,
                      TestClassnameResponse,
                      AddPetResponse,
                      DeletePetResponse,
                      FindPetsByStatusResponse,
                      FindPetsByTagsResponse,
                      GetPetByIdResponse,
                      UpdatePetResponse,
                      UpdatePetWithFormResponse,
                      UploadFileResponse,
                      DeleteOrderResponse,
                      GetInventoryResponse,
                      GetOrderByIdResponse,
                      PlaceOrderResponse,
                      CreateUserResponse,
                      CreateUsersWithArrayInputResponse,
                      CreateUsersWithListInputResponse,
                      DeleteUserResponse,
                      GetUserByNameResponse,
                      LoginUserResponse,
                      LogoutUserResponse,
                      UpdateUserResponse
                     };
use clap::{App, Arg};

fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[
    "FakeOuterBooleanSerialize",
    "FakeOuterCompositeSerialize",
    "FakeOuterNumberSerialize",
    "FakeOuterStringSerialize",
    "TestEndpointParameters",
    "TestEnumParameters",
    "TestJsonFormData",
    "DeletePet",
    "FindPetsByStatus",
    "FindPetsByTags",
    "GetPetById",
    "UpdatePetWithForm",
    "UploadFile",
    "DeleteOrder",
    "GetInventory",
    "GetOrderById",
    "CreateUsersWithArrayInput",
    "CreateUsersWithListInput",
    "DeleteUser",
    "GetUserByName",
    "LoginUser",
    "LogoutUser",
])
            .required(true)
            .index(1))
        .arg(Arg::with_name("https")
            .long("https")
            .help("Whether to use HTTPS or not"))
        .get_matches();

    let client = if matches.is_present("https") {
        // Using Simple HTTPS
        petstore_api::Client::try_new_https("https://localhost:8080", "examples/ca.pem").expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        petstore_api::Client::try_new_http("http://localhost:8080").expect("Failed to create HTTP client")
    };

    match matches.value_of("operation") {

        Some("FakeOuterBooleanSerialize") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.fake_outer_boolean_serialize(None, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("FakeOuterCompositeSerialize") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.fake_outer_composite_serialize(None, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("FakeOuterNumberSerialize") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.fake_outer_number_serialize(None, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("FakeOuterStringSerialize") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.fake_outer_string_serialize(None, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        // Disabled because there's no example.
        // Some("TestClientModel") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.test_client_model(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        Some("TestEndpointParameters") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.test_endpoint_parameters(3.4, 1.2, "pattern_without_delimiter_example".to_string(), swagger::ByteArray("byte_example".to_string().into_bytes()), Some(56), Some(56), Some(789), Some(3.4), Some("string_example".to_string()), Some(swagger::ByteArray(Vec::from("B"))), None, None, Some("password_example".to_string()), Some("callback_example".to_string()), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("TestEnumParameters") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.test_enum_parameters(Some(&Vec::new()), Some("enum_form_string_example".to_string()), Some(&Vec::new()), Some("enum_header_string_example".to_string()), Some(&Vec::new()), Some("enum_query_string_example".to_string()), Some(56), Some(1.2), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("TestJsonFormData") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.test_json_form_data("param_example".to_string(), "param2_example".to_string(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        // Disabled because there's no example.
        // Some("TestClassname") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.test_classname(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        // Disabled because there's no example.
        // Some("AddPet") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.add_pet(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        Some("DeletePet") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.delete_pet(789, Some("api_key_example".to_string()), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("FindPetsByStatus") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.find_pets_by_status(&Vec::new(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("FindPetsByTags") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.find_pets_by_tags(&Vec::new(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("GetPetById") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.get_pet_by_id(789, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        // Disabled because there's no example.
        // Some("UpdatePet") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.update_pet(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        Some("UpdatePetWithForm") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.update_pet_with_form(789, Some("name_example".to_string()), Some("status_example".to_string()), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("UploadFile") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.upload_file(789, Some("additional_metadata_example".to_string()), future::ok(Some(stream::once(Ok(b"hello".to_vec())).boxed())).boxed(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("DeleteOrder") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.delete_order("order_id_example".to_string(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("GetInventory") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.get_inventory(&context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("GetOrderById") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.get_order_by_id(789, &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        // Disabled because there's no example.
        // Some("PlaceOrder") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.place_order(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        // Disabled because there's no example.
        // Some("CreateUser") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.create_user(???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        Some("CreateUsersWithArrayInput") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.create_users_with_array_input(&Vec::new(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("CreateUsersWithListInput") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.create_users_with_list_input(&Vec::new(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("DeleteUser") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.delete_user("username_example".to_string(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("GetUserByName") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.get_user_by_name("username_example".to_string(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("LoginUser") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.login_user("username_example".to_string(), "password_example".to_string(), &context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        Some("LogoutUser") => {
            // Using a non-default `Context` is not required; this is just an example!
            let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
            let result = client.logout_user(&context).wait();
            println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
         },

        // Disabled because there's no example.
        // Some("UpdateUser") => {
        //     // Using a non-default `Context` is not required; this is just an example!
        //     let context = petstore_api::Context::new_with_span_id(self::uuid::Uuid::new_v4().to_string());
        //     let result = client.update_user("username_example".to_string(), ???, &context).wait();
        //     println!("{:?} (X-Span-ID: {:?})", result, context.x_span_id.unwrap_or(String::from("<none>")).clone());
        //  },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}

