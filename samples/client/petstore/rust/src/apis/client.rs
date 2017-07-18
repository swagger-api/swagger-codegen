use std::rc::Rc;

use hyper;
use super::configuration::Configuration;
use super::pet_api;

pub struct APIClient<C: hyper::client::Connect> {
  configuration: Rc<Configuration<C>>,
  pet_api: Box<pet_api::PetApi>,
  store_api: Box<store_api::StoreApi>,
  user_api: Box<user_api::UserApi>,
}

impl<C: hyper::client::Connect> APIClient<C> {
  pub fn new(configuration: Configuration<C>) -> APIClient<C> {
    let rc = Rc::new(configuration);
    
    APIClient {
      configuration: rc.clone(),
      pet_api: Box::new(pet_api::PetApiImpl::new(rc.clone())),
      store_api: Box::new(store_api::StoreApiImpl::new(rc.clone())),
      user_api: Box::new(user_api::UserApiImpl::new(rc.clone())),
    }
  }

  pub fn pet_api(&self) -> &pet_api::PetApi{
    self.pet_api.as_ref()
  }

  pub fn store_api(&self) -> &store_api::StoreApi{
    self.store_api.as_ref()
  }

  pub fn user_api(&self) -> &user_api::UserApi{
    self.user_api.as_ref()
  }


}
