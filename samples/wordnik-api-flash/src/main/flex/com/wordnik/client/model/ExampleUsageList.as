package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class ExampleUsageList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _exampleUsage_obj_class: com.wordnik.client.model.ExampleUsage = null;
        [XmlElements(name="exampleUsage", type="com.wordnik.client.model.ExampleUsage")]
        public var exampleUsage: Array = new Array();

        public function getList(): Array{
            return exampleUsage;
        }

}
}

