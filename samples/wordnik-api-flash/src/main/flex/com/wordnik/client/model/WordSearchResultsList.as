package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.WordSearchResult;
public class WordSearchResultsList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordSearchResults_obj_class: com.wordnik.client.model.WordSearchResults = null;
        [XmlElements(name="wordSearchResults", type="com.wordnik.client.model.WordSearchResults")]
        public var wordSearchResults: Array = new Array();

        public function getList(): Array{
            return wordSearchResults;
        }

}
}

