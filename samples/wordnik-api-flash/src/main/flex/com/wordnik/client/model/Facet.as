package com.wordnik.client.model {

import com.wordnik.client.model.FacetValue;
[XmlRootNode(name="Facet")]
    public class Facet {
    // This declaration below of _facetValues_obj_class is to force flash compiler to include this class
        private var _facetValues_obj_class: com.wordnik.client.model.FacetValue = null;
        [XmlElementWrapper(name="facetValues")]
        [XmlElements(name="facetValue", type="com.wordnik.client.model.FacetValue")]
        public var facetValues: Array = new Array();

    [XmlElement(name="name")]
        public var name: String = null;

    public function toString(): String {
            var str: String = "Facet: ";
            str += " (facetValues: " + facetValues + ")";
            str += " (name: " + name + ")";
            return str;
        }


}
}

