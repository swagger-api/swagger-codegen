extern crate petstore_client;
extern crate hyper;
extern crate tokio_core;
extern crate futures;

use hyper::Client;
use hyper::client::HttpConnector;
use tokio_core::reactor::Core;
use futures::{Future};

fn main() {
    let mut core = Core::new().expect("failed to init core");
    let handle = core.handle();

    let http_connector = HttpConnector::new(4, &handle);

    let client = Client::configure().connector(http_connector).build(&handle);
    let api = "http://localhost:8080/v2";

    let mut new_pet = petstore_client::models::Pet::new("barker".to_owned(), vec![]);
    new_pet.set_id(1337);

    let work = petstore_client::apis::add_pet(api, &client, &new_pet)
    .and_then(|_| {
        petstore_client::apis::update_pet_with_form(
            api,
            &client,
            1337,
            "barko",
            "escaped")
    })
    .and_then(|_| {
        petstore_client::apis::get_pet_by_id(
            api,
            &client,
            1337)
    })
    .and_then(|pet| {
        println!("pet: {:?}", pet);
        futures::future::ok(())
    });

    core.run(work).expect("failed to run core");
}
