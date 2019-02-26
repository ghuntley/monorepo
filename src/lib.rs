//! # crimp
//!
//! This library provides a simplified API over the [cURL Rust
//! bindings][] that resemble that of higher-level libraries such as
//! [reqwest][]. All calls are synchronous.
//!
//! `crimp` is intended to be used in situations where HTTP client
//! functionality is desired without adding a significant number of
//! dependencies or sacrificing too much usability.
//!
//! Using `crimp` to make HTTP requests is done using a simple
//! builder-pattern style API. For example, to make a `GET`-request
//! and print the result to `stdout`:
//!
//! ```rust
//! use crimp::{Method, Request};
//!
//! let response = Request::new(Method::Get, "http://httpbin.org/get")
//!     .user_agent("crimp test suite").unwrap()
//!     .send().unwrap()
//!     .as_string().unwrap();
//!
//! println!("Status: {}\nBody: {}", response.status, response.body);
//! # assert_eq!(response.status, 200);
//! ```
//!
//! If a feature from the underlying cURL library is missing, the
//! `Request::raw` method can be used as an escape hatch to deal with
//! the handle directly. Should you find yourself doing this, please
//! [file an issue][].
//!
//! `crimp` does not currently expose functionality for re-using a
//! cURL Easy handle, meaning that keep-alive of HTTP connections and
//! the like is not supported.
//!
//! ## Cargo features
//!
//! `crimp` has several optional features, all of which are enabled by
//! default:
//!
//! * `json`: Adds `Request::json` and `Response::as_json` methods
//!   which can be used for convenient serialisation of
//!   request/response bodies using `serde_json`. This feature adds a
//!   dependency on the `serde` and `serde_json` crates.
//!
//! * `basic_auth`: Adds a `Request::basic_auth` utility method to set
//!   a basic authentication header on the request. This feature adds
//!   a dependency on the `base64` crate.
//!
//! [cURL Rust bindings]: https://docs.rs/curl
//! [reqwest]: https://docs.rs/reqwest
//! [file an issue]: https://github.com/tazjin/crimp/issues

extern crate curl;

#[cfg(feature = "json")] extern crate serde;
#[cfg(feature = "json")] extern crate serde_json;
#[cfg(feature = "basic_auth")] extern crate base64;

use curl::easy::{Easy, Form, List, ReadError};
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::string::{FromUtf8Error, ToString};

#[cfg(feature = "json")] use serde::Serialize;
#[cfg(feature = "json")] use serde::de::DeserializeOwned;

#[cfg(test)]
mod tests;

/// HTTP method to use for the request.
pub enum Method {
    Get, Post, Put, Patch, Delete
}

/// Certificate types for client-certificate key pairs.
pub enum CertType {
    P12, PEM, DER
}

/// Builder structure for an HTTP request.
///
/// This is the primary API-type in `crimp`. After creating a new
/// request its parameters are modified using the various builder
/// methods until it is consumed by `send()`.
pub struct Request<'a> {
    url: &'a str,
    method: Method,
    handle: Easy,
    headers: List,
    body: Body<'a>,
}

enum Body<'a> {
    NoBody,
    Form(Form),

    Bytes {
        content_type: &'a str,
        data: &'a [u8],
    },

    #[cfg(feature = "json")]
    Json(Vec<u8>),
}

/// HTTP responses structure containing response data and headers.
///
/// By default the `send()` function of the `Request` structure will
/// return a `Response<Vec<u8>>`. Convenience helpers exist for
/// decoding a string via `Response::as_string` or to a
/// `serde`-compatible type with `Response::as_json` (if the
/// `json`-feature is enabled).
#[derive(Debug)]
pub struct Response<T> {
    /// HTTP status code of the response.
    pub status: u32,

    /// HTTP headers returned from the remote.
    pub headers: HashMap<String, String>,

    /// Body data from the HTTP response.
    pub body: T,
}

impl <'a> Request<'a> {
    /// Initiate an HTTP request with the given method and URL.
    pub fn new(method: Method, url: &'a str) -> Self {
        Request {
            url,
            method,
            handle: Easy::new(),
            headers: List::new(),
            body: Body::NoBody,
        }
    }

    /// Add an HTTP header to a request.
    pub fn header(mut self, k: &str, v: &str) -> Result<Self, curl::Error> {
        self.headers.append(&format!("{}: {}", k, v))?;
        Ok(self)
    }

    /// Set the `User-Agent` for this request. By default this will be
    /// set to cURL's standard user agent.
    pub fn user_agent(mut self, agent: &str) -> Result<Self, curl::Error> {
        self.handle.useragent(agent)?;
        Ok(self)
    }

    /// Set the `Authorization` header to a `Bearer` value with the
    /// supplied token.
    pub fn bearer_auth(mut self, token: &str) -> Result<Self, curl::Error> {
        self.headers.append(&format!("Authorization: Bearer {}", token))?;
        Ok(self)
    }

    /// Configure a TLS client certificate on the request.
    ///
    /// Depending on whether the certificate file contains the private
    /// key or not, calling `tls_client_key` may be required in
    /// addition.
    ///
    /// Consult the documentation for the `ssl_cert` and `ssl_key`
    /// functions in `curl::easy::Easy2` for details on supported
    /// formats and defaults.
    pub fn tls_client_cert<P: AsRef<Path>>(mut self, cert_type: CertType, cert: P)
                                           -> Result<Self, curl::Error> {
        self.handle.ssl_cert(cert)?;
        self.handle.ssl_cert_type(match cert_type {
            CertType::P12 => "P12",
            CertType::PEM => "PEM",
            CertType::DER => "DER",
        })?;

        Ok(self)
    }

    /// Configure a TLS client certificate key on the request.
    ///
    /// Note that this does **not** need to be called again for
    /// PKCS12-encoded key pairs which are set via `tls_client_cert`.
    ///
    /// Currently only PEM-encoded key files are supported.
    pub fn tls_client_key<P: AsRef<Path>>(mut self, key: P) -> Result<Self, curl::Error> {
        self.handle.ssl_key(key)?;
        Ok(self)
    }

    /// Configure an encryption password for a TLS client certificate
    /// key on the request.
    ///
    /// This is required in case of an encrypted private key that
    /// should be used.
    pub fn tls_key_password(mut self, password: &str) -> Result<Self, curl::Error> {
        self.handle.key_password(password)?;
        Ok(self)
    }

    #[cfg(feature = "basic_auth")]
    /// Set the `Authorization` header to a basic authentication value
    /// from the supplied username and password.
    pub fn basic_auth(mut self, username: &str, password: &str) -> Result<Self, curl::Error> {
        let auth = base64::encode(format!("{}:{}", username, password).as_bytes());
        self.headers.append(&format!("Authorization: Basic {}", auth))?;
        Ok(self)
    }

    /// Add a byte-array body to a request using the specified
    /// `Content-Type`.
    pub fn body(mut self, content_type: &'a str, data: &'a [u8]) -> Self {
        self.body = Body::Bytes { data, content_type };
        self
    }

    /// Add a form-encoded body to a request using the `curl::Form`
    /// type.
    ///
    /// ```rust
    /// # extern crate curl;
    /// # extern crate serde_json;
    /// # use crimp::*;
    /// # use serde_json::{Value, json};
    /// use curl::easy::Form;
    ///
    /// let mut form = Form::new();
    /// form.part("some-name")
    ///     .contents("some-data".as_bytes())
    ///     .add().unwrap();
    ///
    /// let response = Request::new(Method::Post, "https://httpbin.org/post")
    ///     .user_agent("crimp test suite").unwrap()
    ///     .form(form)
    ///     .send().unwrap();
    /// #
    /// # assert_eq!(200, response.status, "form POST should succeed");
    /// # assert_eq!(
    /// #     response.as_json::<Value>().unwrap().body.get("form").unwrap(),
    /// #     &json!({"some-name": "some-data"}),
    /// #     "posted form data should match",
    /// # );
    /// ```
    ///
    /// See the documentation of `curl::easy::Form` for details on how
    /// to construct a form body.
    pub fn form(mut self, form: Form) -> Self {
        self.body = Body::Form(form);
        self
    }

    /// Add a JSON-encoded body from a serializable type.
    #[cfg(feature = "json")]
    pub fn json<T: Serialize>(mut self, body: &T) -> Result<Self, serde_json::Error> {
        let json = serde_json::to_vec(body)?;
        self.body = Body::Json(json);
        Ok(self)
    }

    /// Send the HTTP request and return a response structure
    /// containing the raw body.
    pub fn send(mut self) -> Result<Response<Vec<u8>>, curl::Error> {
        // Configure request basics:
        self.handle.url(self.url)?;

        match self.method {
            Method::Get    => self.handle.get(true)?,
            Method::Post   => self.handle.post(true)?,
            Method::Put    => self.handle.put(true)?,
            Method::Patch  => self.handle.custom_request("PATCH")?,
            Method::Delete => self.handle.custom_request("DELETE")?,
        }

        // Create structures in which to store the response data:
        let mut headers = HashMap::new();
        let mut body = vec![];

        // Submit a form value to cURL if it is set and proceed
        // pretending that there is no body, as the handling of this
        // type of body happens under-the-hood.
        if let Body::Form(form) = self.body {
            self.handle.httppost(form)?;
            self.body = Body::NoBody;
        }

        // Optionally set content type if a body payload is configured
        // and configure the expected body size (or form payload).
         match self.body {
            Body::Bytes { content_type, data } => {
                self.handle.post_field_size(data.len() as u64)?;
                self.headers.append(&format!("Content-Type: {}", content_type))?;
            },

            #[cfg(feature = "json")]
            Body::Json(ref data) => {
                self.handle.post_field_size(data.len() as u64)?;
                self.headers.append("Content-Type: application/json")?;
            },

             // Do not set content-type header at all if there is no
             // body, or if the form handler was invoked above.
             _ => (),
        };

        // Configure headers on the request:
        self.handle.http_headers(self.headers)?;

        {
            // Take a scoped transfer from the Easy handle. This makes it
            // possible to write data into the above local buffers without
            // fighting the borrow-checker:
            let mut transfer = self.handle.transfer();

            // Write the payload if it exists:
            match self.body {
                Body::Bytes { data, .. } => transfer.read_function(move |mut into| {
                    into.write_all(data)
                        .map(|_| data.len())
                        .map_err(|_| ReadError::Abort)
                })?,

                #[cfg(feature = "json")]
                Body::Json(json) => transfer.read_function(move |mut into| {
                    into.write_all(&json)
                        .map(|_| json.len())
                        .map_err(|_| ReadError::Abort)
                })?,

                // Do nothing if there is no body or if the body is a
                // form.
                _ => (),
            };

            // Read one header per invocation. Request processing is
            // terminated if any header is malformed:
            transfer.header_function(|header| {
                // Headers are expected to be valid UTF-8 data. If they
                // are not, the conversion is lossy.
                //
                // Technically it is legal for HTTP requests to use
                // different encodings, but we don't interface with such
                // services for hygienic reasons.
                let header = String::from_utf8_lossy(header);
                let split = header.splitn(2, ':').collect::<Vec<_>>();

                // "Malformed" headers are skipped. In most cases this
                // will only be the HTTP version statement.
                if split.len() != 2 {
                    return true;
                }

                headers.insert(
                    split[0].trim().to_string(), split[1].trim().to_string()
                );
                true
            })?;

            // Read the body to the allocated buffer.
            transfer.write_function(|data| {
                let len = data.len();
                body.write_all(data)
                    .map(|_| len)
                    .map_err(|err| panic!("{:?}", err))
            })?;

            transfer.perform()?;
        }

        Ok(Response {
            status: self.handle.response_code()?,
            headers,
            body
        })
    }
}

impl <T> Response<T> {
    /// Check whether the status code of this HTTP response is a
    /// success (i.e. in the 200-299 range).
    pub fn is_success(&self) -> bool {
        self.status >= 200 && self.status < 300
    }
}

impl Response<Vec<u8>> {
    /// Attempt to parse the HTTP response body as a UTF-8 encoded
    /// string.
    pub fn as_string(self) -> Result<Response<String>, FromUtf8Error> {
        let body = String::from_utf8(self.body)?;

        Ok(Response {
            body,
            status: self.status,
            headers: self.headers,
        })
    }

    /// Attempt to deserialize the HTTP response body from JSON.
    #[cfg(feature = "json")]
    pub fn as_json<T: DeserializeOwned>(self) -> Result<Response<T>, serde_json::Error> {
        let deserialized = serde_json::from_slice(&self.body)?;

        Ok(Response {
            body: deserialized,
            status: self.status,
            headers: self.headers,
        })
    }
}
