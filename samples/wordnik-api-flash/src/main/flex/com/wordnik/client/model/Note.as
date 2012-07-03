package com.wordnik.client.model {

[XmlRootNode(name="Note")]
    public class Note {
    [XmlElement(name="noteType")]
        public var noteType: String = null;

    // This declaration below of _appliesTo_obj_class is to force flash compiler to include this class
        private var _appliesTo_obj_class: String = null;
        [XmlElementWrapper(name="appliesTo")]
        [XmlElements(name="appliesTo", type="String")]
        public var appliesTo: Array = new Array();

    [XmlElement(name="value")]
        public var value: String = null;

    [XmlElement(name="pos")]
        public var pos: Number = 0.0;

    public function toString(): String {
            var str: String = "Note: ";
            str += " (noteType: " + noteType + ")";
            str += " (appliesTo: " + appliesTo + ")";
            str += " (value: " + value + ")";
            str += " (pos: " + pos + ")";
            return str;
        }


}
}

