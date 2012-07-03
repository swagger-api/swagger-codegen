package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class SyllableList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _syllable_obj_class: com.wordnik.client.model.Syllable = null;
        [XmlElements(name="syllable", type="com.wordnik.client.model.Syllable")]
        public var syllable: Array = new Array();

        public function getList(): Array{
            return syllable;
        }

}
}

