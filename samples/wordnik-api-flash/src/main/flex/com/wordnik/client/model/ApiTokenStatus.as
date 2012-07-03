package com.wordnik.client.model {

[XmlRootNode(name="ApiTokenStatus")]
    public class ApiTokenStatus {
    [XmlElement(name="valid")]
        public var valid: Boolean = false;

    [XmlElement(name="token")]
        public var token: String = null;

    [XmlElement(name="resetsInMillis")]
        public var resetsInMillis: Number = 0.0;

    [XmlElement(name="remainingCalls")]
        public var remainingCalls: Number = 0.0;

    [XmlElement(name="expiresInMillis")]
        public var expiresInMillis: Number = 0.0;

    [XmlElement(name="totalRequests")]
        public var totalRequests: Number = 0.0;

    public function toString(): String {
            var str: String = "ApiTokenStatus: ";
            str += " (valid: " + valid + ")";
            str += " (token: " + token + ")";
            str += " (resetsInMillis: " + resetsInMillis + ")";
            str += " (remainingCalls: " + remainingCalls + ")";
            str += " (expiresInMillis: " + expiresInMillis + ")";
            str += " (totalRequests: " + totalRequests + ")";
            return str;
        }


}
}

