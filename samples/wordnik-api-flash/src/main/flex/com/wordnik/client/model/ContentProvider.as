package com.wordnik.client.model {

[XmlRootNode(name="ContentProvider")]
    public class ContentProvider {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="name")]
        public var name: String = null;

    public function toString(): String {
            var str: String = "ContentProvider: ";
            str += " (id: " + id + ")";
            str += " (name: " + name + ")";
            return str;
        }


}
}

