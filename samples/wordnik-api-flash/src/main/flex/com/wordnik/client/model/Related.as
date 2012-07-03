package com.wordnik.client.model {

[XmlRootNode(name="Related")]
    public class Related {
    [XmlElement(name="label1")]
        public var label1: String = null;

    [XmlElement(name="label2")]
        public var label2: String = null;

    [XmlElement(name="relationshipType")]
        public var relationshipType: String = null;

    [XmlElement(name="label3")]
        public var label3: String = null;

    // This declaration below of _words_obj_class is to force flash compiler to include this class
        private var _words_obj_class: String = null;
        [XmlElementWrapper(name="words")]
        [XmlElements(name="word", type="String")]
        public var words: Array = new Array();

    [XmlElement(name="label4")]
        public var label4: String = null;

    [XmlElement(name="gram")]
        public var gram: String = null;

    public function toString(): String {
            var str: String = "Related: ";
            str += " (label1: " + label1 + ")";
            str += " (label2: " + label2 + ")";
            str += " (relationshipType: " + relationshipType + ")";
            str += " (label3: " + label3 + ")";
            str += " (words: " + words + ")";
            str += " (label4: " + label4 + ")";
            str += " (gram: " + gram + ")";
            return str;
        }


}
}

