package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class StringValueList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _stringValue_obj_class: com.wordnik.client.model.StringValue = null;
        [XmlElements(name="stringValue", type="com.wordnik.client.model.StringValue")]
        public var stringValue: Array = new Array();

        public function getList(): Array{
            return stringValue;
        }

}
}

