package com.wordnik.client.model {

[XmlRootNode(name="AuthenticationToken")]
    public class AuthenticationToken {
    [XmlElement(name="token")]
        public var token: String = null;

    [XmlElement(name="userId")]
        public var userId: Number = 0.0;

    [XmlElement(name="userSignature")]
        public var userSignature: String = null;

    public function toString(): String {
            var str: String = "AuthenticationToken: ";
            str += " (token: " + token + ")";
            str += " (userId: " + userId + ")";
            str += " (userSignature: " + userSignature + ")";
            return str;
        }


}
}

