package com.wordnik.client.model {

[XmlRootNode(name="WordList")]
    public class WordList {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="updatedAt")]
        public var updatedAt: Date = null;

    [XmlElement(name="username")]
        public var username: String = null;

    [XmlElement(name="permalink")]
        public var permalink: String = null;

    [XmlElement(name="description")]
        public var description: String = null;

    [XmlElement(name="createdAt")]
        public var createdAt: Date = null;

    [XmlElement(name="lastActivityAt")]
        public var lastActivityAt: Date = null;

    [XmlElement(name="name")]
        public var name: String = null;

    [XmlElement(name="userId")]
        public var userId: Number = 0.0;

    [XmlElement(name="numberWordsInList")]
        public var numberWordsInList: Number = 0.0;

    [XmlElement(name="type")]
        public var type: String = null;

    public function toString(): String {
            var str: String = "WordList: ";
            str += " (id: " + id + ")";
            str += " (updatedAt: " + updatedAt + ")";
            str += " (username: " + username + ")";
            str += " (permalink: " + permalink + ")";
            str += " (description: " + description + ")";
            str += " (createdAt: " + createdAt + ")";
            str += " (lastActivityAt: " + lastActivityAt + ")";
            str += " (name: " + name + ")";
            str += " (userId: " + userId + ")";
            str += " (numberWordsInList: " + numberWordsInList + ")";
            str += " (type: " + type + ")";
            return str;
        }


}
}

