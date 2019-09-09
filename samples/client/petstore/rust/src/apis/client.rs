use hyper;
use super::configuration::Configuration;

pub struct APIClient<'a, C: hyper::client::connect::Connect> {
  #[allow(dead_code)]
  configuration: &'a Configuration<C>,
  pet_api: Box<dyn (::apis::PetApi) + 'a>,
  store_api: Box<dyn (::apis::StoreApi) + 'a>,
  user_api: Box<dyn (::apis::UserApi) + 'a>,
}

impl<'a, C: hyper::client::connect::Connect> APIClient<'a, C> where C: 'static {
  pub fn new(configuration: &'a Configuration<C>) -> APIClient<C> where C: 'static {
    APIClient {
      configuration: configuration,
      pet_api: Box::new(::apis::PetApiClient::new(configuration)),
      store_api: Box::new(::apis::StoreApiClient::new(configuration)),
      user_api: Box::new(::apis::UserApiClient::new(configuration)),
    }
  }

  pub fn pet_api(&self) -> &dyn (::apis::PetApi) {
    self.pet_api.as_ref()
  }

  pub fn store_api(&self) -> &dyn (::apis::StoreApi) {
    self.store_api.as_ref()
  }

  pub fn user_api(&self) -> &dyn (::apis::UserApi) {
    self.user_api.as_ref()
  }


}
