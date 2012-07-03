package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.ScoredWord;
public class SentenceList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _sentence_obj_class: com.wordnik.client.model.Sentence = null;
        [XmlElements(name="sentence", type="com.wordnik.client.model.Sentence")]
        public var sentence: Array = new Array();

        public function getList(): Array{
            return sentence;
        }

}
}

