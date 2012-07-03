package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class LabelList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _label_obj_class: com.wordnik.client.model.Label = null;
        [XmlElements(name="label", type="com.wordnik.client.model.Label")]
        public var label: Array = new Array();

        public function getList(): Array{
            return label;
        }

}
}

