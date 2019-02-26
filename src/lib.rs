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

use curl::easy::{Easy, List, ReadError};
use std::collections::HashMap;
use std::io::Write;
use std::string::{FromUtf8Error, ToString};

type CurlResult<T> = Result<T, curl::Error>;

/// HTTP method to use for the request.
pub enum Method {
    Get, Post, Put, Patch, Delete
}


pub struct Request<'a> {
    handle: Easy,
    headers: List,
    body: Option<&'a [u8]>,
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
            body: None,
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
    pub fn body(&'a mut self, content_type: &str, body: &'a [u8])
                    -> CurlResult<&mut Self> {
        self.header("Content-Type", content_type)?;
        self.body = Some(body);

        Ok(self)
    }

    /// Send the HTTP request and return a response structure
    /// containing the raw body.
    pub fn send(mut self) -> CurlResult<CurlResponse<Vec<u8>>> {
        // Create structures in which to store the response data:
        let mut headers = HashMap::new();
        let mut body = vec![];

        {
            // Take a scoped transfer from the Easy handle. This makes it
            // possible to write data into the above local buffers without
            // fighting the borrow-checker:
            let mut transfer = self.handle.transfer();

            // Write the payload if it exists:
            if let Some(body) = self.body {
                transfer.read_function(move |mut into| {
                    into.write_all(body)
                        .map(|_| body.len())
                        .map_err(|_| ReadError::Abort)
                })?;
            }

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
}
