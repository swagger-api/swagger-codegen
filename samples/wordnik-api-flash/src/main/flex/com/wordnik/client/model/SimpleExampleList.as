package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class SimpleExampleList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _simpleExample_obj_class: com.wordnik.client.model.SimpleExample = null;
        [XmlElements(name="simpleExample", type="com.wordnik.client.model.SimpleExample")]
        public var simpleExample: Array = new Array();

        public function getList(): Array{
            return simpleExample;
        }

}
}

