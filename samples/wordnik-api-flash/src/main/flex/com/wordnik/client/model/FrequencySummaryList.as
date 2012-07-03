package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.Frequency;
public class FrequencySummaryList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _frequencySummary_obj_class: com.wordnik.client.model.FrequencySummary = null;
        [XmlElements(name="frequencySummary", type="com.wordnik.client.model.FrequencySummary")]
        public var frequencySummary: Array = new Array();

        public function getList(): Array{
            return frequencySummary;
        }

}
}

