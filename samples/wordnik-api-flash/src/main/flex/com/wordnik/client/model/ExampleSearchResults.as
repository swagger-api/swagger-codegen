package com.wordnik.client.model {

import com.wordnik.client.model.Facet;
import com.wordnik.client.model.Example;
[XmlRootNode(name="ExampleSearchResults")]
    public class ExampleSearchResults {
    // This declaration below of _facets_obj_class is to force flash compiler to include this class
        private var _facets_obj_class: com.wordnik.client.model.Facet = null;
        [XmlElementWrapper(name="facets")]
        [XmlElements(name="facet", type="com.wordnik.client.model.Facet")]
        public var facets: Array = new Array();

    // This declaration below of _examples_obj_class is to force flash compiler to include this class
        private var _examples_obj_class: com.wordnik.client.model.Example = null;
        [XmlElementWrapper(name="examples")]
        [XmlElements(name="example", type="com.wordnik.client.model.Example")]
        public var examples: Array = new Array();

    public function toString(): String {
            var str: String = "ExampleSearchResults: ";
            str += " (facets: " + facets + ")";
            str += " (examples: " + examples + ")";
            return str;
        }


}
}

