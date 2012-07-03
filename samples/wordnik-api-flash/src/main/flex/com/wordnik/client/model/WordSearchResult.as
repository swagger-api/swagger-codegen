package com.wordnik.client.model {

[XmlRootNode(name="WordSearchResult")]
    public class WordSearchResult {
    [XmlElement(name="count")]
        public var count: Number = 0.0;

    [XmlElement(name="lexicality")]
        public var lexicality: Number = 0.0;

    [XmlElement(name="word")]
        public var word: String = null;

    public function toString(): String {
            var str: String = "WordSearchResult: ";
            str += " (count: " + count + ")";
            str += " (lexicality: " + lexicality + ")";
            str += " (word: " + word + ")";
            return str;
        }


}
}

