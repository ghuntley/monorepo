//! # crimp
//!
//! This library provides a simplified API over the [cURL Rust
//! bindings][] that resemble that of higher-level libraries such as
//! [reqwest][].
//!
//! `crimp` is intended to be used in situations where HTTP client
//! functionality is desired without adding a significant number of
//! dependencies or sacrificing too much usability.
//!
//! [cURL Rust bindings]: https://docs.rs/curl
//! [reqwest]: https://docs.rs/reqwest

extern crate curl;

#[cfg(feature = "json")] extern crate serde;
#[cfg(feature = "json")] extern crate serde_json;

use curl::easy::{Easy, List, ReadError};
#[cfg(feature = "json")] use serde::Serialize;
#[cfg(feature = "json")] use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::io::Write;
use std::string::{FromUtf8Error, ToString};

#[cfg(test)]
mod tests;

type CurlResult<T> = Result<T, curl::Error>;

/// HTTP method to use for the request.
pub enum Method {
    Get, Post, Put, Patch, Delete
}

pub struct Request<'a> {
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

#[derive(Debug)]
pub struct CurlResponse<T> {
    pub status: u32,
    pub headers: HashMap<String, String>,
    pub body: T,
}

impl <'a> Request<'a> {
    /// Initiate an HTTP request with the given method and URL.
    pub fn new(method: Method, url: &str) -> CurlResult<Self> {
        let mut handle = Easy::new();
        handle.url(url)?;

        match method {
            Method::Get    => handle.get(true)?,
            Method::Post   => handle.post(true)?,
            Method::Put    => handle.put(true)?,
            Method::Patch  => handle.custom_request("PATCH")?,
            Method::Delete => handle.custom_request("DELETE")?,
        }

        Ok(Request {
            handle,
            headers: List::new(),
            body: Body::NoBody,
        })
    }

    /// Add a header to a request.
    pub fn header(&mut self, k: &str, v: &str) -> CurlResult<&mut Self> {
        self.headers.append(&format!("{}: {}", k, v))?;
        Ok(self)
    }

    /// Set the User-Agent for this request.
    pub fn user_agent(&mut self, agent: &str) -> CurlResult<&mut Self> {
        self.handle.useragent(agent)?;
        Ok(self)
    }

    /// Add a byte-array body to a request using the specified
    /// Content-Type.
    pub fn body(&'a mut self, content_type: &'a str, data: &'a [u8]) -> &mut Self {
        self.body = Body::Bytes { data, content_type };
        self
    }

    /// Add a JSON-encoded body from a serializable type.
    #[cfg(feature = "json")]
    pub fn json<T: Serialize>(&'a mut self, body: &T)
                              -> Result<&mut Self, serde_json::Error> {
        let json = serde_json::to_vec(body)?;
        self.body = Body::Json(json);
        Ok(self)
    }

    /// Send the HTTP request and return a response structure
    /// containing the raw body.
    pub fn send(mut self) -> CurlResult<CurlResponse<Vec<u8>>> {
        // Create structures in which to store the response data:
        let mut headers = HashMap::new();
        let mut body = vec![];

        // Optionally set content type if a body payload is
        // configured.
        match self.body {
            Body::Bytes { content_type, .. } => self.header("Content-Type", content_type),
            Body::NoBody => Ok(&mut self),

            #[cfg(feature = "json")]
            Body::Json(..) => self.header("Content-Type", "application/json"),
        }?;

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
                Body::NoBody => {},
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

        Ok(CurlResponse {
            status: self.handle.response_code()?,
            headers,
            body
        })
    }
}

impl CurlResponse<Vec<u8>> {
    /// Attempt to parse the HTTP response body as a UTF-8 encoded
    /// string.
    pub fn as_string(self) -> Result<CurlResponse<String>, FromUtf8Error> {
        let body = String::from_utf8(self.body)?;

        Ok(CurlResponse {
            body,
            status: self.status,
            headers: self.headers,
        })
    }

    /// Attempt to deserialize the HTTP response body from JSON.
    #[cfg(feature = "json")]
    pub fn as_json<T: DeserializeOwned>(self) -> Result<CurlResponse<T>, serde_json::Error> {
        let deserialized = serde_json::from_slice(&self.body)?;

        Ok(CurlResponse {
            body: deserialized,
            status: self.status,
            headers: self.headers,
        })
    }
}
