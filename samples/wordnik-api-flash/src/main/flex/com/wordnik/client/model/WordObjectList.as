package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class WordObjectList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordObject_obj_class: com.wordnik.client.model.WordObject = null;
        [XmlElements(name="wordObject", type="com.wordnik.client.model.WordObject")]
        public var wordObject: Array = new Array();

        public function getList(): Array{
            return wordObject;
        }

}
}

