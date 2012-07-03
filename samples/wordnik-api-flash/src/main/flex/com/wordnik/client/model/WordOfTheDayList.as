package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.SimpleDefinition;
import com.wordnik.client.model.SimpleExample;
import com.wordnik.client.model.ContentProvider;
public class WordOfTheDayList implements ListWrapper {
        // This declaration below of __obj_class is to force flash compiler to include this class
        private var _wordOfTheDay_obj_class: com.wordnik.client.model.WordOfTheDay = null;
        [XmlElements(name="wordOfTheDay", type="com.wordnik.client.model.WordOfTheDay")]
        public var wordOfTheDay: Array = new Array();

        public function getList(): Array{
            return wordOfTheDay;
        }

}
}

