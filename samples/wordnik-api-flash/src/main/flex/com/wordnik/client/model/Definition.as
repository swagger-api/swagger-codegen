package com.wordnik.client.model {

import com.wordnik.client.model.ExampleUsage;
import com.wordnik.client.model.Note;
import com.wordnik.client.model.Citation;
import com.wordnik.client.model.TextPron;
import com.wordnik.client.model.Label;
import com.wordnik.client.model.Related;
[XmlRootNode(name="Definition")]
    public class Definition {
    [XmlElement(name="extendedText")]
        public var extendedText: String = null;

    [XmlElement(name="text")]
        public var text: String = null;

    [XmlElement(name="sourceDictionary")]
        public var sourceDictionary: String = null;

    // This declaration below of _citations_obj_class is to force flash compiler to include this class
        private var _citations_obj_class: com.wordnik.client.model.Citation = null;
        [XmlElementWrapper(name="citations")]
        [XmlElements(name="citation", type="com.wordnik.client.model.Citation")]
        public var citations: Array = new Array();

    // This declaration below of _labels_obj_class is to force flash compiler to include this class
        private var _labels_obj_class: com.wordnik.client.model.Label = null;
        [XmlElementWrapper(name="labels")]
        [XmlElements(name="label", type="com.wordnik.client.model.Label")]
        public var labels: Array = new Array();

    [XmlElement(name="score")]
        public var score: Number = 0.0;

    // This declaration below of _exampleUses_obj_class is to force flash compiler to include this class
        private var _exampleUses_obj_class: com.wordnik.client.model.ExampleUsage = null;
        [XmlElementWrapper(name="exampleUses")]
        [XmlElements(name="exampleUse", type="com.wordnik.client.model.ExampleUsage")]
        public var exampleUses: Array = new Array();

    [XmlElement(name="attributionUrl")]
        public var attributionUrl: String = null;

    [XmlElement(name="seqString")]
        public var seqString: String = null;

    [XmlElement(name="attributionText")]
        public var attributionText: String = null;

    // This declaration below of _relatedWords_obj_class is to force flash compiler to include this class
        private var _relatedWords_obj_class: com.wordnik.client.model.Related = null;
        [XmlElementWrapper(name="relatedWords")]
        [XmlElements(name="relatedWord", type="com.wordnik.client.model.Related")]
        public var relatedWords: Array = new Array();

    [XmlElement(name="sequence")]
        public var sequence: String = null;

    [XmlElement(name="word")]
        public var word: String = null;

    // This declaration below of _textProns_obj_class is to force flash compiler to include this class
        private var _textProns_obj_class: com.wordnik.client.model.TextPron = null;
        [XmlElementWrapper(name="textProns")]
        [XmlElements(name="textPron", type="com.wordnik.client.model.TextPron")]
        public var textProns: Array = new Array();

    // This declaration below of _notes_obj_class is to force flash compiler to include this class
        private var _notes_obj_class: com.wordnik.client.model.Note = null;
        [XmlElementWrapper(name="notes")]
        [XmlElements(name="note", type="com.wordnik.client.model.Note")]
        public var notes: Array = new Array();

    [XmlElement(name="partOfSpeech")]
        public var partOfSpeech: String = null;

    public function toString(): String {
            var str: String = "Definition: ";
            str += " (extendedText: " + extendedText + ")";
            str += " (text: " + text + ")";
            str += " (sourceDictionary: " + sourceDictionary + ")";
            str += " (citations: " + citations + ")";
            str += " (labels: " + labels + ")";
            str += " (score: " + score + ")";
            str += " (exampleUses: " + exampleUses + ")";
            str += " (attributionUrl: " + attributionUrl + ")";
            str += " (seqString: " + seqString + ")";
            str += " (attributionText: " + attributionText + ")";
            str += " (relatedWords: " + relatedWords + ")";
            str += " (sequence: " + sequence + ")";
            str += " (word: " + word + ")";
            str += " (textProns: " + textProns + ")";
            str += " (notes: " + notes + ")";
            str += " (partOfSpeech: " + partOfSpeech + ")";
            return str;
        }


}
}

