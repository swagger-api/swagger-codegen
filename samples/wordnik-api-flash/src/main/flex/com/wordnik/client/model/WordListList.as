package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class WordListList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordList_obj_class: com.wordnik.client.model.WordList = null;
        [XmlElements(name="wordList", type="com.wordnik.client.model.WordList")]
        public var wordList: Array = new Array();

        public function getList(): Array{
            return wordList;
        }

}
}

