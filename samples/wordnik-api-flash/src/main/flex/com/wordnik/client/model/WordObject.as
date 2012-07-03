package com.wordnik.client.model {

[XmlRootNode(name="WordObject")]
    public class WordObject {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="originalWord")]
        public var originalWord: String = null;

    [XmlElement(name="word")]
        public var word: String = null;

    // This declaration below of _suggestions_obj_class is to force flash compiler to include this class
        private var _suggestions_obj_class: String = null;
        [XmlElementWrapper(name="suggestions")]
        [XmlElements(name="suggestion", type="String")]
        public var suggestions: Array = new Array();

    [XmlElement(name="canonicalForm")]
        public var canonicalForm: String = null;

    [XmlElement(name="vulgar")]
        public var vulgar: String = null;

    public function toString(): String {
            var str: String = "WordObject: ";
            str += " (id: " + id + ")";
            str += " (originalWord: " + originalWord + ")";
            str += " (word: " + word + ")";
            str += " (suggestions: " + suggestions + ")";
            str += " (canonicalForm: " + canonicalForm + ")";
            str += " (vulgar: " + vulgar + ")";
            return str;
        }


}
}

