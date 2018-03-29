use std::io;
use std::marker::PhantomData;
use std::default::Default;
use hyper;
use hyper::{Request, Response, Error, StatusCode};
use server::url::form_urlencoded;
use swagger::auth::{Authorization, AuthData, Scopes};
use swagger::context::ExtendsWith;
use Api;

pub struct NewService<T, C, D>
    where
        T: hyper::server::NewService<Request=(Request, D), Response=Response, Error=Error>,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    inner: T,
    marker1: PhantomData<C>,
    marker2: PhantomData<D>,
}

impl<T, C, D> NewService<T, C, D>
    where
        T: hyper::server::NewService<Request=(Request, D), Response=Response, Error=Error> + 'static,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    pub fn new(inner: T) -> NewService<T, C, D> {
        NewService{inner, marker1: PhantomData, marker2: PhantomData}
    }
}

impl<T, C, D> hyper::server::NewService for NewService<T, C, D>
    where
        T: hyper::server::NewService<Request=(Request, D), Response=Response, Error=Error> + 'static,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Instance = Service<T::Instance, C, D>;

    fn new_service(&self) -> Result<Self::Instance, io::Error> {
        self.inner.new_service().map(|s| Service::new(s))
    }
}

/// Middleware to extract authentication data from request
pub struct Service<T, C, D>
    where
        T: hyper::server::Service<Request=(Request, D), Response=Response, Error=Error>,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    inner: T,
    marker1: PhantomData<C>,
    marker2: PhantomData<D>,
}

impl<T, C, D> Service<T, C, D>
    where
        T: hyper::server::Service<Request=(Request, D), Response=Response, Error=Error>,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    pub fn new(inner: T) -> Service<T, C, D> {
        Service{inner, marker1: PhantomData, marker2: PhantomData}
    }
}

impl<T, C, D> hyper::server::Service for Service<T, C, D>
    where
        T: hyper::server::Service<Request=(Request, D), Response=Response, Error=Error>,
        C: Default,
        D: ExtendsWith<Inner=C, Ext=Option<AuthData>>,
{
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = T::Future;

    fn call(&self, req: Self::Request) -> Self::Future {
        {
            header! { (ApiKey1, "api_key") => [String] }
            if let Some(header) = req.headers().get::<ApiKey1>().cloned() {
                let auth_data = AuthData::ApiKey(header.0);
                let context = D::new(C::default(), Some(auth_data));
                return self.inner.call((req, context));
            }
        }
        {
            let key = form_urlencoded::parse(req.query().unwrap_or_default().as_bytes())
                .filter(|e| e.0 == "api_key_query")
                .map(|e| e.1.clone().into_owned())
                .nth(0);
            if let Some(key) = key {
                let auth_data = AuthData::ApiKey(key);
                let context = D::new(C::default(), Some(auth_data));
                return self.inner.call((req, context));
            }
        }
        {
            use hyper::header::{Authorization, Basic, Bearer};
            use std::ops::Deref;
            if let Some(basic) = req.headers().get::<Authorization<Basic>>().cloned() {
                let auth_data = AuthData::Basic(basic.deref().clone());
                let context = D::new(C::default(), Some(auth_data));
                return self.inner.call((req, context));
            }
        }
        {
            use hyper::header::{Authorization, Basic, Bearer};
            use std::ops::Deref;
            if let Some(bearer) = req.headers().get::<Authorization<Bearer>>().cloned() {
                let auth_data = AuthData::Bearer(bearer.deref().clone());
                let context = D::new(C::default(), Some(auth_data));
                return self.inner.call((req, context));
            }
        }

        let context = D::new(C::default(), None);
        return self.inner.call((req, context));
    }
}
