package com.wordnik.client.model {

import com.wordnik.client.model.Frequency;
[XmlRootNode(name="FrequencySummary")]
    public class FrequencySummary {
    [XmlElement(name="unknownYearCount")]
        public var unknownYearCount: Number = 0.0;

    [XmlElement(name="totalCount")]
        public var totalCount: Number = 0.0;

    [XmlElement(name="frequencyString")]
        public var frequencyString: String = null;

    [XmlElement(name="word")]
        public var word: String = null;

    // This declaration below of _frequency_obj_class is to force flash compiler to include this class
        private var _frequency_obj_class: com.wordnik.client.model.Frequency = null;
        [XmlElementWrapper(name="frequency")]
        [XmlElements(name="frequency", type="com.wordnik.client.model.Frequency")]
        public var frequency: Array = new Array();

    public function toString(): String {
            var str: String = "FrequencySummary: ";
            str += " (unknownYearCount: " + unknownYearCount + ")";
            str += " (totalCount: " + totalCount + ")";
            str += " (frequencyString: " + frequencyString + ")";
            str += " (word: " + word + ")";
            str += " (frequency: " + frequency + ")";
            return str;
        }


}
}

