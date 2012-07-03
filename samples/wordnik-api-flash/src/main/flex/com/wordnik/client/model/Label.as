package com.wordnik.client.model {

[XmlRootNode(name="Label")]
    public class Label {
    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="type")]
        public var type: String = null;

    public function toString(): String {
            var str: String = "Label: ";
            str += " (text: " + text + ")";
            str += " (type: " + type + ")";
            return str;
        }


}
}

