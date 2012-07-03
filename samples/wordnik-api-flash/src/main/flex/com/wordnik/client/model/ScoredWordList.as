package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class ScoredWordList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _scoredWord_obj_class: com.wordnik.client.model.ScoredWord = null;
        [XmlElements(name="scoredWord", type="com.wordnik.client.model.ScoredWord")]
        public var scoredWord: Array = new Array();

        public function getList(): Array{
            return scoredWord;
        }

}
}

