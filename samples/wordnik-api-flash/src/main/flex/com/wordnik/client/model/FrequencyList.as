package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class FrequencyList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _frequency_obj_class: com.wordnik.client.model.Frequency = null;
        [XmlElements(name="frequency", type="com.wordnik.client.model.Frequency")]
        public var frequency: Array = new Array();

        public function getList(): Array{
            return frequency;
        }

}
}

