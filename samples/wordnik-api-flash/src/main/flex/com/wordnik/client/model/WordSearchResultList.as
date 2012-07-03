package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class WordSearchResultList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordSearchResult_obj_class: com.wordnik.client.model.WordSearchResult = null;
        [XmlElements(name="wordSearchResult", type="com.wordnik.client.model.WordSearchResult")]
        public var wordSearchResult: Array = new Array();

        public function getList(): Array{
            return wordSearchResult;
        }

}
}

