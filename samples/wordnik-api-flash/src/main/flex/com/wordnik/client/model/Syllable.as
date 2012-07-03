package com.wordnik.client.model {

[XmlRootNode(name="Syllable")]
    public class Syllable {
    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="seq")]
        public var seq: Number = 0.0;

    [XmlElement(name="type")]
        public var type: String = null;

    public function toString(): String {
            var str: String = "Syllable: ";
            str += " (text: " + text + ")";
            str += " (seq: " + seq + ")";
            str += " (type: " + type + ")";
            return str;
        }


}
}

