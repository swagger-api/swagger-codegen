package com.wordnik.client.model {

[XmlRootNode(name="SimpleDefinition")]
    public class SimpleDefinition {
    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="source")]
        public var source: String = null;

    [XmlElement(name="note")]
        public var note: String = null;

    [XmlElement(name="partOfSpeech")]
        public var partOfSpeech: String = null;

    public function toString(): String {
            var str: String = "SimpleDefinition: ";
            str += " (text: " + text + ")";
            str += " (source: " + source + ")";
            str += " (note: " + note + ")";
            str += " (partOfSpeech: " + partOfSpeech + ")";
            return str;
        }


}
}

