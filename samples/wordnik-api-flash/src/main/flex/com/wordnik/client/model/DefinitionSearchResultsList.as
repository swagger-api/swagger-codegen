package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.Definition;
public class DefinitionSearchResultsList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _definitionSearchResults_obj_class: com.wordnik.client.model.DefinitionSearchResults = null;
        [XmlElements(name="definitionSearchResults", type="com.wordnik.client.model.DefinitionSearchResults")]
        public var definitionSearchResults: Array = new Array();

        public function getList(): Array{
            return definitionSearchResults;
        }

}
}

