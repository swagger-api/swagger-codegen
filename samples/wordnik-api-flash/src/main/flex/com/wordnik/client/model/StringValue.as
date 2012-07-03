package com.wordnik.client.model {

[XmlRootNode(name="StringValue")]
    public class StringValue {
    [XmlElement(name="word")]
        public var word: String = null;

    public function toString(): String {
            var str: String = "StringValue: ";
            str += " (word: " + word + ")";
            return str;
        }


}
}

