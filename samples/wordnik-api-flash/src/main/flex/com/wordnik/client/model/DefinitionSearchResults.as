package com.wordnik.client.model {

import com.wordnik.client.model.Definition;
[XmlRootNode(name="DefinitionSearchResults")]
    public class DefinitionSearchResults {
    // This declaration below of _results_obj_class is to force flash compiler to include this class
        private var _results_obj_class: com.wordnik.client.model.Definition = null;
        [XmlElementWrapper(name="results")]
        [XmlElements(name="result", type="com.wordnik.client.model.Definition")]
        public var results: Array = new Array();

    [XmlElement(name="totalResults")]
        public var totalResults: Number = 0.0;

    public function toString(): String {
            var str: String = "DefinitionSearchResults: ";
            str += " (results: " + results + ")";
            str += " (totalResults: " + totalResults + ")";
            return str;
        }


}
}

