package com.wordnik.client.model {

[XmlRootNode(name="Bigram")]
    public class Bigram {
    [XmlElement(name="count")]
        public var count: Number = 0.0;

    [XmlElement(name="gram2")]
        public var gram2: String = null;

    [XmlElement(name="gram1")]
        public var gram1: String = null;

    [XmlElement(name="wlmi")]
        public var wlmi: Number = 0.0;

    [XmlElement(name="mi")]
        public var mi: Number = 0.0;

    public function toString(): String {
            var str: String = "Bigram: ";
            str += " (count: " + count + ")";
            str += " (gram2: " + gram2 + ")";
            str += " (gram1: " + gram1 + ")";
            str += " (wlmi: " + wlmi + ")";
            str += " (mi: " + mi + ")";
            return str;
        }


}
}

