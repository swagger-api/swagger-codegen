#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate petstore_api;
#[allow(unused_extern_crates)]
extern crate futures;
#[allow(unused_extern_crates)]
extern crate swagger;
#[allow(unused_extern_crates)]
extern crate uuid;
extern crate clap;
extern crate tokio_core;

use swagger::{Context, XSpanIdString, Has, AuthData};

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
use tokio_core::reactor;
#[allow(unused_imports)]
use petstore_api::{ApiNoContext, ContextWrapperExt,
                      ApiError,
                      TestSpecialTagsResponse,
                      GetXmlFeaturesResponse,
                      PostPlainTextResponse,
                      PostUrlEncodedFormResponse,
                      PostXmlFeaturesResponse,
                      PutPlainTextResponse,
                      TestBodyWithQueryParamsResponse,
                      UuidHeaderResponse,
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
    "GetXmlFeatures",
    "PostPlainText",
    "PostUrlEncodedForm",
    "PutPlainText",
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
        .arg(Arg::with_name("host")
            .long("host")
            .takes_value(true)
            .default_value("petstore.swagger.io")
            .help("Hostname to contact"))
        .arg(Arg::with_name("port")
            .long("port")
            .takes_value(true)
            .default_value("80")
            .help("Port to contact"))
        .get_matches();

    let mut core = reactor::Core::new().unwrap();
    let is_https = matches.is_present("https");
    let base_url = format!("{}://{}:{}",
                           if is_https { "https" } else { "http" },
                           matches.value_of("host").unwrap(),
                           matches.value_of("port").unwrap());
    let client = if matches.is_present("https") {
        // Using Simple HTTPS
        petstore_api::Client::try_new_https(core.handle(), &base_url, "examples/ca.pem")
            .expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        petstore_api::Client::try_new_http(core.handle(), &base_url)
            .expect("Failed to create HTTP client")
    };

    // Using a non-default `Context` is not required; this is just an example!
    let context: Context<XSpanIdString, ()> = Context::construct(XSpanIdString(self::uuid::Uuid::new_v4().to_string()), ());
    let context: Context<Option<AuthData>, Context<XSpanIdString, ()>> = Context::construct(None, context);
    let client = client.with_context(context);

    match matches.value_of("operation") {

        // Disabled because there's no example.
        // Some("TestSpecialTags") => {
        //     let result = core.run(client.test_special_tags(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("GetXmlFeatures") => {
            let result = core.run(client.get_xml_features());
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("PostPlainText") => {
            let result = core.run(client.post_plain_text("message_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("PostUrlEncodedForm") => {
            let result = core.run(client.post_url_encoded_form("param1_example".to_string(), "param2_example".to_string(), Some("param3_example".to_string())));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("PostXmlFeatures") => {
        //     let result = core.run(client.post_xml_features(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("PutPlainText") => {
            let result = core.run(client.put_plain_text(Some("message_example".to_string())));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("TestBodyWithQueryParams") => {
        //     let result = core.run(client.test_body_with_query_params(???, "query_example".to_string()));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        // Disabled because there's no example.
        // Some("UuidHeader") => {
        //     let result = core.run(client.uuid_header(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("FakeOuterBooleanSerialize") => {
            let result = core.run(client.fake_outer_boolean_serialize(None));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("FakeOuterCompositeSerialize") => {
            let result = core.run(client.fake_outer_composite_serialize(None));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("FakeOuterNumberSerialize") => {
            let result = core.run(client.fake_outer_number_serialize(None));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("FakeOuterStringSerialize") => {
            let result = core.run(client.fake_outer_string_serialize(None));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("TestClientModel") => {
        //     let result = core.run(client.test_client_model(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("TestEndpointParameters") => {
            let result = core.run(client.test_endpoint_parameters(8.14, 1.2, "pattern_without_delimiter_example".to_string(), swagger::ByteArray(Vec::from("B")), Some(56), Some(56), Some(789), Some(3.4), Some("string_example".to_string()), Some(swagger::ByteArray(Vec::from("B"))), None, None, Some("password_example".to_string()), Some("callback_example".to_string())));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("TestEnumParameters") => {
            let result = core.run(client.test_enum_parameters(Some(&Vec::new()), Some("enum_form_string_example".to_string()), Some(&Vec::new()), Some("enum_header_string_example".to_string()), Some(&Vec::new()), Some("enum_query_string_example".to_string()), Some(56), Some(1.2)));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("TestJsonFormData") => {
            let result = core.run(client.test_json_form_data("param_example".to_string(), "param2_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("TestClassname") => {
        //     let result = core.run(client.test_classname(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        // Disabled because there's no example.
        // Some("AddPet") => {
        //     let result = core.run(client.add_pet(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("DeletePet") => {
            let result = core.run(client.delete_pet(789, Some("api_key_example".to_string())));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("FindPetsByStatus") => {
            let result = core.run(client.find_pets_by_status(&Vec::new()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("FindPetsByTags") => {
            let result = core.run(client.find_pets_by_tags(&Vec::new()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("GetPetById") => {
            let result = core.run(client.get_pet_by_id(789));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("UpdatePet") => {
        //     let result = core.run(client.update_pet(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("UpdatePetWithForm") => {
            let result = core.run(client.update_pet_with_form(789, Some("name_example".to_string()), Some("status_example".to_string())));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("UploadFile") => {
            let result = core.run(client.upload_file(789, Some("additional_metadata_example".to_string()), Box::new(future::ok(Some(Box::new(stream::once(Ok(b"hello".to_vec()))) as Box<Stream<Item=_, Error=_> + Send>))) as Box<Future<Item=_, Error=_> + Send>));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("DeleteOrder") => {
            let result = core.run(client.delete_order("order_id_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("GetInventory") => {
            let result = core.run(client.get_inventory());
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("GetOrderById") => {
            let result = core.run(client.get_order_by_id(789));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("PlaceOrder") => {
        //     let result = core.run(client.place_order(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        // Disabled because there's no example.
        // Some("CreateUser") => {
        //     let result = core.run(client.create_user(???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        Some("CreateUsersWithArrayInput") => {
            let result = core.run(client.create_users_with_array_input(&Vec::new()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("CreateUsersWithListInput") => {
            let result = core.run(client.create_users_with_list_input(&Vec::new()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("DeleteUser") => {
            let result = core.run(client.delete_user("username_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("GetUserByName") => {
            let result = core.run(client.get_user_by_name("username_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("LoginUser") => {
            let result = core.run(client.login_user("username_example".to_string(), "password_example".to_string()));
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        Some("LogoutUser") => {
            let result = core.run(client.logout_user());
            println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
         },

        // Disabled because there's no example.
        // Some("UpdateUser") => {
        //     let result = core.run(client.update_user("username_example".to_string(), ???));
        //     println!("{:?} (X-Span-ID: {:?})", result, Has::<XSpanIdString>::get(client.context()).clone());
        //  },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}

