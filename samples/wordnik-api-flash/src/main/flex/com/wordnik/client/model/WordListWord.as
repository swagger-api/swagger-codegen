package com.wordnik.client.model {

[XmlRootNode(name="WordListWord")]
    public class WordListWord {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="username")]
        public var username: String = null;

    [XmlElement(name="createdAt")]
        public var createdAt: Date = null;

    [XmlElement(name="userId")]
        public var userId: Number = 0.0;

    [XmlElement(name="numberCommentsOnWord")]
        public var numberCommentsOnWord: Number = 0.0;

    [XmlElement(name="word")]
        public var word: String = null;

    [XmlElement(name="numberLists")]
        public var numberLists: Number = 0.0;

    public function toString(): String {
            var str: String = "WordListWord: ";
            str += " (id: " + id + ")";
            str += " (username: " + username + ")";
            str += " (createdAt: " + createdAt + ")";
            str += " (userId: " + userId + ")";
            str += " (numberCommentsOnWord: " + numberCommentsOnWord + ")";
            str += " (word: " + word + ")";
            str += " (numberLists: " + numberLists + ")";
            return str;
        }


}
}

