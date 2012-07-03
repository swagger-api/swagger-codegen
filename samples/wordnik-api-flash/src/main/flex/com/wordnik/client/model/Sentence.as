package com.wordnik.client.model {

import com.wordnik.client.model.ScoredWord;
[XmlRootNode(name="Sentence")]
    public class Sentence {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="hasScoredWords")]
        public var hasScoredWords: Boolean = false;

    // This declaration below of _scoredWords_obj_class is to force flash compiler to include this class
        private var _scoredWords_obj_class: com.wordnik.client.model.ScoredWord = null;
        [XmlElementWrapper(name="scoredWords")]
        [XmlElements(name="scoredWord", type="com.wordnik.client.model.ScoredWord")]
        public var scoredWords: Array = new Array();

    [XmlElement(name="display")]
        public var display: String = null;

    [XmlElement(name="rating")]
        public var rating: Number = 0.0;

    [XmlElement(name="documentMetadataId")]
        public var documentMetadataId: Number = 0.0;

    public function toString(): String {
            var str: String = "Sentence: ";
            str += " (id: " + id + ")";
            str += " (hasScoredWords: " + hasScoredWords + ")";
            str += " (scoredWords: " + scoredWords + ")";
            str += " (display: " + display + ")";
            str += " (rating: " + rating + ")";
            str += " (documentMetadataId: " + documentMetadataId + ")";
            return str;
        }


}
}

