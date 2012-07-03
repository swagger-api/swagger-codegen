package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class WordListWordList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordListWord_obj_class: com.wordnik.client.model.WordListWord = null;
        [XmlElements(name="wordListWord", type="com.wordnik.client.model.WordListWord")]
        public var wordListWord: Array = new Array();

        public function getList(): Array{
            return wordListWord;
        }

}
}

