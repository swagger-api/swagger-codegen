package com.wordnik.client.model {

[XmlRootNode(name="TextPron")]
    public class TextPron {
    [XmlElement(name="raw")]
        public var raw: String = null;

    [XmlElement(name="seq")]
        public var seq: Number = 0.0;

    [XmlElement(name="rawType")]
        public var rawType: String = null;

    public function toString(): String {
            var str: String = "TextPron: ";
            str += " (raw: " + raw + ")";
            str += " (seq: " + seq + ")";
            str += " (rawType: " + rawType + ")";
            return str;
        }


}
}

