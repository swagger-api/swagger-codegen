package com.wordnik.client.model {

[XmlRootNode(name="Frequency")]
    public class Frequency {
    [XmlElement(name="count")]
        public var count: Number = 0.0;

    [XmlElement(name="year")]
        public var year: Number = 0.0;

    public function toString(): String {
            var str: String = "Frequency: ";
            str += " (count: " + count + ")";
            str += " (year: " + year + ")";
            return str;
        }


}
}

