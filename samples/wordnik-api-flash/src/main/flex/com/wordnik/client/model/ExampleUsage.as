package com.wordnik.client.model {

[XmlRootNode(name="ExampleUsage")]
    public class ExampleUsage {
    [XmlElement(name="text")]
        public var text: String = null;

    public function toString(): String {
            var str: String = "ExampleUsage: ";
            str += " (text: " + text + ")";
            return str;
        }


}
}

