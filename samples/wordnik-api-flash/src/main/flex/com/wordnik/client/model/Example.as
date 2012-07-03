package com.wordnik.client.model {

import com.wordnik.client.model.Sentence;
import com.wordnik.client.model.ScoredWord;
import com.wordnik.client.model.ContentProvider;
[XmlRootNode(name="Example")]
    public class Example {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="title")]
        public var title: String = null;

    [XmlElement(name="exampleId")]
        public var exampleId: Number = 0.0;

    [XmlElement(name="score")]
        public var score: ScoredWord = null;

    [XmlElement(name="sentence")]
        public var sentence: Sentence = null;

    [XmlElement(name="year")]
        public var year: Number = 0.0;

    [XmlElement(name="provider")]
        public var provider: ContentProvider = null;

    [XmlElement(name="word")]
        public var word: String = null;

    [XmlElement(name="rating")]
        public var rating: Number = 0.0;

    [XmlElement(name="url")]
        public var url: String = null;

    [XmlElement(name="documentId")]
        public var documentId: Number = 0.0;

    public function toString(): String {
            var str: String = "Example: ";
            str += " (id: " + id + ")";
            str += " (text: " + text + ")";
            str += " (title: " + title + ")";
            str += " (exampleId: " + exampleId + ")";
            str += " (score: " + score + ")";
            str += " (sentence: " + sentence + ")";
            str += " (year: " + year + ")";
            str += " (provider: " + provider + ")";
            str += " (word: " + word + ")";
            str += " (rating: " + rating + ")";
            str += " (url: " + url + ")";
            str += " (documentId: " + documentId + ")";
            return str;
        }


}
}

