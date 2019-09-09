#[test]
fn user_empty() {
    let json = r#"{}"#;
    let amount: petstore_client::models::User = serde_json::from_str(json).unwrap();
}

#[test]
fn user_with_id() {
    let json = r#"{"id": 12}"#;
    let amount: petstore_client::models::User = serde_json::from_str(json).unwrap();
}

#[test]
fn amount_from_str() {
    let json = r#"{"value": 36.14, "currency": "HRK" }"#;
    let amount: petstore_client::models::Amount = serde_json::from_str(json).unwrap();
}