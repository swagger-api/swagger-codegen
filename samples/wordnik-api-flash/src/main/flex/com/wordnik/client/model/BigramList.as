package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class BigramList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _bigram_obj_class: com.wordnik.client.model.Bigram = null;
        [XmlElements(name="bigram", type="com.wordnik.client.model.Bigram")]
        public var bigram: Array = new Array();

        public function getList(): Array{
            return bigram;
        }

}
}

