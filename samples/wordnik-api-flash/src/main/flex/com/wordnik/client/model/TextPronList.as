package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class TextPronList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _textPron_obj_class: com.wordnik.client.model.TextPron = null;
        [XmlElements(name="textPron", type="com.wordnik.client.model.TextPron")]
        public var textPron: Array = new Array();

        public function getList(): Array{
            return textPron;
        }

}
}

