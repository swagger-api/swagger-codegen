package com.wordnik.client.model {

[XmlRootNode(name="ScoredWord")]
    public class ScoredWord {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="position")]
        public var position: Number = 0.0;

    [XmlElement(name="lemma")]
        public var lemma: String = null;

    [XmlElement(name="docTermCount")]
        public var docTermCount: Number = 0.0;

    [XmlElement(name="wordType")]
        public var wordType: String = null;

    [XmlElement(name="score")]
        public var score: Number = 0.0;

    [XmlElement(name="word")]
        public var word: String = null;

    [XmlElement(name="sentenceId")]
        public var sentenceId: Number = 0.0;

    [XmlElement(name="stopword")]
        public var stopword: Boolean = false;

    [XmlElement(name="baseWordScore")]
        public var baseWordScore: Number = 0.0;

    [XmlElement(name="partOfSpeech")]
        public var partOfSpeech: String = null;

    public function toString(): String {
            var str: String = "ScoredWord: ";
            str += " (id: " + id + ")";
            str += " (position: " + position + ")";
            str += " (lemma: " + lemma + ")";
            str += " (docTermCount: " + docTermCount + ")";
            str += " (wordType: " + wordType + ")";
            str += " (score: " + score + ")";
            str += " (word: " + word + ")";
            str += " (sentenceId: " + sentenceId + ")";
            str += " (stopword: " + stopword + ")";
            str += " (baseWordScore: " + baseWordScore + ")";
            str += " (partOfSpeech: " + partOfSpeech + ")";
            return str;
        }


}
}

