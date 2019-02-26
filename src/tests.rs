use super::*;
use serde_json::{Value, json};

// These tests check whether the correct HTTP method is used in the
// requests. httpbin will return 405-statuses for incorrect methods.

#[test]
fn test_http_get() {
    let resp = Request::new(Method::Get, "https://httpbin.org/get")
        .send().expect("failed to send request");

    assert!(resp.is_success(), "request should have succeeded");
}

#[test]
fn test_http_delete() {
    let resp = Request::new(Method::Delete, "https://httpbin.org/delete")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

#[test]
fn test_http_put() {
    let resp = Request::new(Method::Put, "https://httpbin.org/put")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

#[test]
fn test_http_patch() {
    let resp = Request::new(Method::Patch, "https://httpbin.org/patch")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

// These tests perform various requests with different body payloads
// and verify that those were received correctly by the remote side.

#[test]
fn test_http_post() {
    let body = "test body";
    let response = Request::new(Method::Post, "https://httpbin.org/post")
        .user_agent("crimp test suite").expect("failed to set user-agent")
        .body("text/plain", &body.as_bytes())
        .send().expect("failed to send request")
        .as_json::<Value>().expect("failed to deserialize response");

    let data = response.body;

    assert_eq!(200, response.status, "response status should be 200 OK");

    assert_eq!(data.get("data").unwrap(), &json!("test body"),
               "test body should have been POSTed");

    assert_eq!(
        data.get("headers").unwrap().get("Content-Type").unwrap(),
        &json!("text/plain"),
        "Content-Type should be `text/plain`",
    );
}

#[cfg(feature = "json")] #[test]
fn test_http_post_json() {
    let body = json!({
        "purpose": "testing!"
    });

    let response = Request::new(Method::Post, "https://httpbin.org/post")
        .user_agent("crimp test suite").expect("failed to set user-agent")
        .json(&body).expect("request serialization failed")
        .send().expect("failed to send request")
        .as_json::<Value>().expect("failed to deserialize response");


    let data = response.body;

    assert_eq!(200, response.status, "response status should be 200 OK");

    assert_eq!(data.get("json").unwrap(), &body,
               "test body should have been POSTed");

    assert_eq!(
        data.get("headers").unwrap().get("Content-Type").unwrap(),
        &json!("application/json"),
        "Content-Type should be `application/json`",
    );
}

// Tests for different authentication methods that are supported
// out-of-the-box:

#[test]
fn test_bearer_auth() {
    let response = Request::new(Method::Get, "https://httpbin.org/bearer")
        .bearer_auth("some-token").expect("failed to set auth header")
        .send().expect("failed to send request");

    assert!(response.is_success(), "authorized request should succeed");
}

#[cfg(feature = "basic_auth")] #[test]
fn test_basic_auth() {
    let request = Request::new(
        Method::Get, "https://httpbin.org/basic-auth/alan_watts/oneness"
    );

    let response = request
        .basic_auth("alan_watts", "oneness").expect("failed to set auth header")
        .send().expect("failed to send request");

    assert!(response.is_success(), "authorized request should succeed");
}
