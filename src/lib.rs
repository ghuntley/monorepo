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
//! builder-pattern style API:
//!
//! ```rust
//! use crimp::{Method, Request};
//!
//! let response = Request::new(Method::Get, "http://httpbin.org/get")
//!     .user_agent("crimp test suite").unwrap()
//!     .send().unwrap();
//!
//! assert_eq!(response.status, 200);
//! ```
//!
//! The `json` feature (enabled by default) adds support for
//! automatically serialising and deserialising request/response
//! bodies using `serde_json`.
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
//! [cURL Rust bindings]: https://docs.rs/curl
//! [reqwest]: https://docs.rs/reqwest
//! [file an issue]: https://github.com/tazjin/crimp/issues

extern crate curl;

#[cfg(feature = "json")] extern crate serde;
#[cfg(feature = "json")] extern crate serde_json;

use curl::easy::{Easy, List, ReadError};
use std::collections::HashMap;
use std::io::Write;
use std::string::{FromUtf8Error, ToString};

#[cfg(feature = "json")] use serde::Serialize;
#[cfg(feature = "json")] use serde::de::DeserializeOwned;

#[cfg(test)]
mod tests;

/// HTTP method to use for the request.
pub enum Method {
    Get, Post, Put, Patch, Delete
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
    #[cfg(feature = "json")]
    Json(Vec<u8>),
    Bytes {
        content_type: &'a str,
        data: &'a [u8],
    }
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
    pub fn user_agent<'b: 'a>(mut self, agent: &str) -> Result<Self, curl::Error> {
        self.handle.useragent(agent)?;
        Ok(self)
    }

    /// Add a byte-array body to a request using the specified
    /// `Content-Type`.
    pub fn body(mut self, content_type: &'a str, data: &'a [u8]) -> Self {
        self.body = Body::Bytes { data, content_type };
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

        // Optionally set content type if a body payload is configured
        // and configure the expected body size.
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

            Body::NoBody => (),
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

                // Do nothing if there is no body ...
                Body::NoBody => (),
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
