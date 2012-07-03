package com.wordnik.client.model {

[XmlRootNode(name="User")]
    public class User {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="username")]
        public var username: String = null;

    [XmlElement(name="status")]
        public var status: Number = 0.0;

    [XmlElement(name="email")]
        public var email: String = null;

    [XmlElement(name="faceBookId")]
        public var faceBookId: String = null;

    [XmlElement(name="userName")]
        public var userName: String = null;

    [XmlElement(name="displayName")]
        public var displayName: String = null;

    [XmlElement(name="password")]
        public var password: String = null;

    public function toString(): String {
            var str: String = "User: ";
            str += " (id: " + id + ")";
            str += " (username: " + username + ")";
            str += " (status: " + status + ")";
            str += " (email: " + email + ")";
            str += " (faceBookId: " + faceBookId + ")";
            str += " (userName: " + userName + ")";
            str += " (displayName: " + displayName + ")";
            str += " (password: " + password + ")";
            return str;
        }


}
}

