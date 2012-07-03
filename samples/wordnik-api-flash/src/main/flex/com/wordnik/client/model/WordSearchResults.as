package com.wordnik.client.model {

import com.wordnik.client.model.WordSearchResult;
[XmlRootNode(name="WordSearchResults")]
    public class WordSearchResults {
    [XmlElement(name="totalResults")]
        public var totalResults: Number = 0.0;

    // This declaration below of _searchResults_obj_class is to force flash compiler to include this class
        private var _searchResults_obj_class: com.wordnik.client.model.WordSearchResult = null;
        [XmlElementWrapper(name="searchResults")]
        [XmlElements(name="searchResult", type="com.wordnik.client.model.WordSearchResult")]
        public var searchResults: Array = new Array();

    public function toString(): String {
            var str: String = "WordSearchResults: ";
            str += " (totalResults: " + totalResults + ")";
            str += " (searchResults: " + searchResults + ")";
            return str;
        }


}
}

