package com.wordnik.client.model {

[XmlRootNode(name="SimpleExample")]
    public class SimpleExample {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="title")]
        public var title: String = null;

    [XmlElement(name="url")]
        public var url: String = null;

    public function toString(): String {
            var str: String = "SimpleExample: ";
            str += " (id: " + id + ")";
            str += " (text: " + text + ")";
            str += " (title: " + title + ")";
            str += " (url: " + url + ")";
            return str;
        }


}
}

