package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.Facet;
import com.wordnik.client.model.Example;
public class ExampleSearchResultsList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _exampleSearchResults_obj_class: com.wordnik.client.model.ExampleSearchResults = null;
        [XmlElements(name="exampleSearchResults", type="com.wordnik.client.model.ExampleSearchResults")]
        public var exampleSearchResults: Array = new Array();

        public function getList(): Array{
            return exampleSearchResults;
        }

}
}

