#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;

use std::collections::HashMap;
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AdditionalPropertiesClass {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "map_property")]
    pub map_property: Option<HashMap<String, String>>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "map_of_map_property")]
    pub map_of_map_property: Option<HashMap<String, HashMap<String, String>>>,


}

impl AdditionalPropertiesClass {
    pub fn new() -> AdditionalPropertiesClass {
        AdditionalPropertiesClass {
            map_property: None,
            map_of_map_property: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Animal {
    pub class_name: String,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "color")]
    pub color: Option<String>,


}

impl Animal {
    pub fn new(class_name: String, ) -> Animal {
        Animal {
            class_name: class_name,
            color: Some("red".to_string()),
        }
    }
}

pub type AnimalFarm = Vec<Animal>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ApiResponse {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "code")]
    pub code: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "type")]
    pub _type: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "message")]
    pub message: Option<String>,


}

impl ApiResponse {
    pub fn new() -> ApiResponse {
        ApiResponse {
            code: None,
            _type: None,
            message: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayOfArrayOfNumberOnly {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "ArrayArrayNumber")]
    pub array_array_number: Option<Vec<Vec<f64>>>,


}

impl ArrayOfArrayOfNumberOnly {
    pub fn new() -> ArrayOfArrayOfNumberOnly {
        ArrayOfArrayOfNumberOnly {
            array_array_number: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayOfNumberOnly {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "ArrayNumber")]
    pub array_number: Option<Vec<f64>>,


}

impl ArrayOfNumberOnly {
    pub fn new() -> ArrayOfNumberOnly {
        ArrayOfNumberOnly {
            array_number: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArrayTest {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "array_of_string")]
    pub array_of_string: Option<Vec<String>>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "array_array_of_integer")]
    pub array_array_of_integer: Option<Vec<Vec<i64>>>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "array_array_of_model")]
    pub array_array_of_model: Option<Vec<Vec<models::ReadOnlyFirst>>>,


}

impl ArrayTest {
    pub fn new() -> ArrayTest {
        ArrayTest {
            array_of_string: None,
            array_array_of_integer: None,
            array_array_of_model: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Capitalization {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "smallCamel")]
    pub small_camel: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "CapitalCamel")]
    pub capital_camel: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "small_Snake")]
    pub small_snake: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "Capital_Snake")]
    pub capital_snake: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "SCA_ETH_Flow_Points")]
    pub sca_eth_flow_points: Option<String>,

    /// Name of the pet 
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "ATT_NAME")]
    pub att_name: Option<String>,


}

impl Capitalization {
    pub fn new() -> Capitalization {
        Capitalization {
            small_camel: None,
            capital_camel: None,
            small_snake: None,
            capital_snake: None,
            sca_eth_flow_points: None,
            att_name: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Category {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "id")]
    pub id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "name")]
    pub name: Option<String>,


}

impl Category {
    pub fn new() -> Category {
        Category {
            id: None,
            name: None,
        }
    }
}

/// Model for testing model with \"_class\" property
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassModel {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "_class")]
    pub _class: Option<String>,


}

impl ClassModel {
    pub fn new() -> ClassModel {
        ClassModel {
            _class: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Client {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "client")]
    pub client: Option<String>,


}

impl Client {
    pub fn new() -> Client {
        Client {
            client: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumArrays {
    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "just_symbol")]
    pub just_symbol: Option<String>,

    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "array_enum")]
    pub array_enum: Option<Vec<String>>,


}

impl EnumArrays {
    pub fn new() -> EnumArrays {
        EnumArrays {
            just_symbol: None,
            array_enum: None,
        }
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum EnumClass { 
    #[serde(rename = "_abc")]
    _ABC,
    #[serde(rename = "-efg")]
    _EFG,
    #[serde(rename = "(xyz)")]
    _XYZ_,
}

impl ::std::fmt::Display for EnumClass {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            EnumClass::_ABC => write!(f, "{}", "_abc"),
            EnumClass::_EFG => write!(f, "{}", "-efg"),
            EnumClass::_XYZ_ => write!(f, "{}", "(xyz)"),
        }
    }
}

impl ::std::str::FromStr for EnumClass {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "_abc" => Ok(EnumClass::_ABC),
            "-efg" => Ok(EnumClass::_EFG),
            "(xyz)" => Ok(EnumClass::_XYZ_),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumTest {
    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "enum_string")]
    pub enum_string: Option<String>,

    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "enum_integer")]
    pub enum_integer: Option<i32>,

    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "enum_number")]
    pub enum_number: Option<f64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "outerEnum")]
    pub outer_enum: Option<models::OuterEnum>,


}

impl EnumTest {
    pub fn new() -> EnumTest {
        EnumTest {
            enum_string: None,
            enum_integer: None,
            enum_number: None,
            outer_enum: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FormatTest {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "integer")]
    pub integer: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "int32")]
    pub int32: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "int64")]
    pub int64: Option<i64>,

    pub number: f64,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "float")]
    pub float: Option<f32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "double")]
    pub double: Option<f64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "string")]
    pub string: Option<String>,

    pub byte: swagger::ByteArray,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "binary")]
    pub binary: Option<swagger::ByteArray>,

    pub date: chrono::DateTime<chrono::Utc>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "dateTime")]
    pub date_time: Option<chrono::DateTime<chrono::Utc>>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "uuid")]
    pub uuid: Option<uuid::Uuid>,

    pub password: String,


}

impl FormatTest {
    pub fn new(number: f64, byte: swagger::ByteArray, date: chrono::DateTime<chrono::Utc>, password: String, ) -> FormatTest {
        FormatTest {
            integer: None,
            int32: None,
            int64: None,
            number: number,
            float: None,
            double: None,
            string: None,
            byte: byte,
            binary: None,
            date: date,
            date_time: None,
            uuid: None,
            password: password,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HasOnlyReadOnly {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "bar")]
    pub bar: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "foo")]
    pub foo: Option<String>,


}

impl HasOnlyReadOnly {
    pub fn new() -> HasOnlyReadOnly {
        HasOnlyReadOnly {
            bar: None,
            foo: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct List {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "123-list")]
    pub _123_list: Option<String>,


}

impl List {
    pub fn new() -> List {
        List {
            _123_list: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MapTest {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "map_map_of_string")]
    pub map_map_of_string: Option<HashMap<String, HashMap<String, String>>>,

    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "map_of_enum_string")]
    pub map_of_enum_string: Option<HashMap<String, String>>,


}

impl MapTest {
    pub fn new() -> MapTest {
        MapTest {
            map_map_of_string: None,
            map_of_enum_string: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MixedPropertiesAndAdditionalPropertiesClass {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "uuid")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "dateTime")]
    pub date_time: Option<chrono::DateTime<chrono::Utc>>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "map")]
    pub map: Option<HashMap<String, models::Animal>>,


}

impl MixedPropertiesAndAdditionalPropertiesClass {
    pub fn new() -> MixedPropertiesAndAdditionalPropertiesClass {
        MixedPropertiesAndAdditionalPropertiesClass {
            uuid: None,
            date_time: None,
            map: None,
        }
    }
}

/// Model for testing model name starting with number
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Model200Response {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "name")]
    pub name: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "class")]
    pub class: Option<String>,


}

impl Model200Response {
    pub fn new() -> Model200Response {
        Model200Response {
            name: None,
            class: None,
        }
    }
}

/// Model for testing reserved words
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModelReturn {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "return")]
    pub _return: Option<i32>,


}

impl ModelReturn {
    pub fn new() -> ModelReturn {
        ModelReturn {
            _return: None,
        }
    }
}

/// Model for testing model name same as property name
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Name {
    pub name: i32,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "snake_case")]
    pub snake_case: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "property")]
    pub property: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "123Number")]
    pub _123_number: Option<i32>,


}

impl Name {
    pub fn new(name: i32, ) -> Name {
        Name {
            name: name,
            snake_case: None,
            property: None,
            _123_number: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NumberOnly {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "JustNumber")]
    pub just_number: Option<f64>,


}

impl NumberOnly {
    pub fn new() -> NumberOnly {
        NumberOnly {
            just_number: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Order {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "id")]
    pub id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "petId")]
    pub pet_id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "quantity")]
    pub quantity: Option<i32>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "shipDate")]
    pub ship_date: Option<chrono::DateTime<chrono::Utc>>,

    /// Order Status
    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "status")]
    pub status: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "complete")]
    pub complete: Option<bool>,


}

impl Order {
    pub fn new() -> Order {
        Order {
            id: None,
            pet_id: None,
            quantity: None,
            ship_date: None,
            status: None,
            complete: Some(false),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct OuterBoolean(bool);

impl From<bool> for OuterBoolean {
    fn from(x: bool) -> Self {
        OuterBoolean(x)
    }
}

impl ::std::ops::Deref for OuterBoolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterBoolean {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OuterComposite {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "my_number")]
    pub my_number: Option<models::OuterNumber>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "my_string")]
    pub my_string: Option<models::OuterString>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "my_boolean")]
    pub my_boolean: Option<models::OuterBoolean>,


}

impl OuterComposite {
    pub fn new() -> OuterComposite {
        OuterComposite {
            my_number: None,
            my_string: None,
            my_boolean: None,
        }
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum OuterEnum { 
    #[serde(rename = "placed")]
    PLACED,
    #[serde(rename = "approved")]
    APPROVED,
    #[serde(rename = "delivered")]
    DELIVERED,
}

impl ::std::fmt::Display for OuterEnum {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            OuterEnum::PLACED => write!(f, "{}", "placed"),
            OuterEnum::APPROVED => write!(f, "{}", "approved"),
            OuterEnum::DELIVERED => write!(f, "{}", "delivered"),
        }
    }
}

impl ::std::str::FromStr for OuterEnum {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "placed" => Ok(OuterEnum::PLACED),
            "approved" => Ok(OuterEnum::APPROVED),
            "delivered" => Ok(OuterEnum::DELIVERED),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct OuterNumber(f64);

impl From<f64> for OuterNumber {
    fn from(x: f64) -> Self {
        OuterNumber(x)
    }
}

impl ::std::ops::Deref for OuterNumber {
    type Target = f64;
    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterNumber {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct OuterString(String);

impl From<String> for OuterString {
    fn from(x: String) -> Self {
        OuterString(x)
    }
}

impl ::std::ops::Deref for OuterString {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Pet {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "id")]
    pub id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "category")]
    pub category: Option<models::Category>,

    pub name: String,

    pub photo_urls: Vec<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "tags")]
    pub tags: Option<Vec<models::Tag>>,

    /// pet status in the store
    // Note: inline enums are not fully supported by swagger-codegen
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "status")]
    pub status: Option<String>,


}

impl Pet {
    pub fn new(name: String, photo_urls: Vec<String>, ) -> Pet {
        Pet {
            id: None,
            category: None,
            name: name,
            photo_urls: photo_urls,
            tags: None,
            status: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReadOnlyFirst {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "bar")]
    pub bar: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "baz")]
    pub baz: Option<String>,


}

impl ReadOnlyFirst {
    pub fn new() -> ReadOnlyFirst {
        ReadOnlyFirst {
            bar: None,
            baz: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpecialModelName {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "$special[property.name]")]
    pub special_property_name: Option<i64>,


}

impl SpecialModelName {
    pub fn new() -> SpecialModelName {
        SpecialModelName {
            special_property_name: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Tag {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "id")]
    pub id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "name")]
    pub name: Option<String>,


}

impl Tag {
    pub fn new() -> Tag {
        Tag {
            id: None,
            name: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct User {
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "id")]
    pub id: Option<i64>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "username")]
    pub username: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "firstName")]
    pub first_name: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "lastName")]
    pub last_name: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "email")]
    pub email: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "password")]
    pub password: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "phone")]
    pub phone: Option<String>,

    /// User Status
    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "userStatus")]
    pub user_status: Option<i32>,


}

impl User {
    pub fn new() -> User {
        User {
            id: None,
            username: None,
            first_name: None,
            last_name: None,
            email: None,
            password: None,
            phone: None,
            user_status: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Cat {
    pub class_name: String,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "color")]
    pub color: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "declawed")]
    pub declawed: Option<bool>,


}

impl Cat {
    pub fn new(class_name: String, ) -> Cat {
        Cat {
            class_name: class_name,
            color: Some("red".to_string()),
            declawed: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Dog {
    pub class_name: String,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "color")]
    pub color: Option<String>,

    #[serde(skip_serializing_if="Option::is_none")]
    #[serde(rename = "breed")]
    pub breed: Option<String>,


}

impl Dog {
    pub fn new(class_name: String, ) -> Dog {
        Dog {
            class_name: class_name,
            color: Some("red".to_string()),
            breed: None,
        }
    }
}

