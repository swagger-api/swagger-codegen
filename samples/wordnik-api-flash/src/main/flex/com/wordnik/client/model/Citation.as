package com.wordnik.client.model {

[XmlRootNode(name="Citation")]
    public class Citation {
    [XmlElement(name="cite")]
        public var cite: String = null;

    [XmlElement(name="source")]
        public var source: String = null;

    public function toString(): String {
            var str: String = "Citation: ";
            str += " (cite: " + cite + ")";
            str += " (source: " + source + ")";
            return str;
        }


}
}

