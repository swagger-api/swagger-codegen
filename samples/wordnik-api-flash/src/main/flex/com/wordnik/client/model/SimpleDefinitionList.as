package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class SimpleDefinitionList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _simpleDefinition_obj_class: com.wordnik.client.model.SimpleDefinition = null;
        [XmlElements(name="simpleDefinition", type="com.wordnik.client.model.SimpleDefinition")]
        public var simpleDefinition: Array = new Array();

        public function getList(): Array{
            return simpleDefinition;
        }

}
}

