use super::*;
use serde_json::{Value, json};

// These tests check whether the correct HTTP method is used in the
// requests. httpbin will return 405-statuses for incorrect methods.

#[test]
fn test_http_get() {
    let resp = Request::get("https://httpbin.org/get")
        .send().expect("failed to send request");

    assert!(resp.is_success(), "request should have succeeded");
}

#[test]
fn test_http_delete() {
    let resp = Request::delete("https://httpbin.org/delete")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

#[test]
fn test_http_put() {
    let resp = Request::put("https://httpbin.org/put")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

#[test]
fn test_http_patch() {
    let resp = Request::patch("https://httpbin.org/patch")
        .send().expect("failed to send request");

    assert_eq!(200, resp.status, "response status should be 200 OK");
}

// These tests perform various requests with different body payloads
// and verify that those were received correctly by the remote side.

#[test]
fn test_http_post() {
    let body = "test body";
    let response = Request::post("https://httpbin.org/post")
        .user_agent("crimp test suite").expect("failed to set user-agent")
        .timeout(Duration::from_secs(5)).expect("failed to set request timeout")
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

    let response = Request::post("https://httpbin.org/post")
        .user_agent("crimp test suite").expect("failed to set user-agent")
        .timeout(Duration::from_secs(5)).expect("failed to set request timeout")
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
    let response = Request::get("https://httpbin.org/bearer")
        .bearer_auth("some-token").expect("failed to set auth header")
        .send().expect("failed to send request");

    assert!(response.is_success(), "authorized request should succeed");
}

#[test]
fn test_basic_auth() {
    let request = Request::get("https://httpbin.org/basic-auth/alan_watts/oneness");

    let response = request
        .basic_auth("alan_watts", "oneness").expect("failed to set auth header")
        .send().expect("failed to send request");

    assert!(response.is_success(), "authorized request should succeed");
}

#[test]
fn test_large_body() {
    // By default cURL buffers seem to be 2^16 bytes in size. The test
    // size is therefore 2^16+1.
    const BODY_SIZE: usize = 65537;

    let resp = Request::post("http://127.0.0.1:4662/post")
        .body("application/octet-stream", &[0; BODY_SIZE])
        .send().expect("sending request")
        .as_json::<Value>().expect("JSON deserialisation");

    // httpbin returns the uploaded data as a string in the `data`
    // field.
    let data = resp.body.get("data").unwrap().as_str().unwrap();

    assert_eq!(BODY_SIZE, data.len(), "uploaded data length should be correct");
}

// Tests for various other features.

#[test]
fn test_error_for_status() {
    let response = Request::get("https://httpbin.org/patch")
        .send().expect("failed to send request")
        .error_for_status(|resp| format!("Response error code: {}", resp.status));

    assert_eq!(Err("Response error code: 405".into()), response,
               "returned error should be converted into Result::Err");
}
