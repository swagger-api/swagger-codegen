package com.wordnik.client.model {

import com.wordnik.client.model.Facet;
import com.wordnik.client.model.Example;
[XmlRootNode(name="Wada")]
    public class Wada {
    [XmlElement(name="title")]
    public var title: String = null;

    // This declaration below of _examples_obj_class is to force flash compiler to include this class
        private var _examples_obj_class: Example = null;
        [XmlElementWrapper(name="examples")]
        [XmlElements(name="example", type="com.wordnik.client.model.Example")]
        public var examples: Array = new Array();

    public function toString(): String {
            var str: String = "Wada: ";
            str += " (title: " + title + ")";
            str += " (examples: " + examples + ")";
            return str;
        }


}
}

