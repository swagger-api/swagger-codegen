package com.wordnik.client.model {

import com.wordnik.client.model.SimpleDefinition;
import com.wordnik.client.model.SimpleExample;
import com.wordnik.client.model.ContentProvider;
[XmlRootNode(name="WordOfTheDay")]
    public class WordOfTheDay {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="parentId")]
        public var parentId: String = null;

    [XmlElement(name="category")]
        public var category: String = null;

    [XmlElement(name="createdBy")]
        public var createdBy: String = null;

    [XmlElement(name="createdAt")]
        public var createdAt: Date = null;

    [XmlElement(name="contentProvider")]
        public var contentProvider: ContentProvider = null;

    [XmlElement(name="word")]
        public var word: String = null;

    [XmlElement(name="htmlExtra")]
        public var htmlExtra: String = null;

    // This declaration below of _definitions_obj_class is to force flash compiler to include this class
        private var _definitions_obj_class: com.wordnik.client.model.SimpleDefinition = null;
        [XmlElementWrapper(name="definitions")]
        [XmlElements(name="definition", type="com.wordnik.client.model.SimpleDefinition")]
        public var definitions: Array = new Array();

    // This declaration below of _examples_obj_class is to force flash compiler to include this class
        private var _examples_obj_class: com.wordnik.client.model.SimpleExample = null;
        [XmlElementWrapper(name="examples")]
        [XmlElements(name="example", type="com.wordnik.client.model.SimpleExample")]
        public var examples: Array = new Array();

    [XmlElement(name="publishDate")]
        public var publishDate: Date = null;

    [XmlElement(name="note")]
        public var note: String = null;

    public function toString(): String {
            var str: String = "WordOfTheDay: ";
            str += " (id: " + id + ")";
            str += " (parentId: " + parentId + ")";
            str += " (category: " + category + ")";
            str += " (createdBy: " + createdBy + ")";
            str += " (createdAt: " + createdAt + ")";
            str += " (contentProvider: " + contentProvider + ")";
            str += " (word: " + word + ")";
            str += " (htmlExtra: " + htmlExtra + ")";
            str += " (definitions: " + definitions + ")";
            str += " (examples: " + examples + ")";
            str += " (publishDate: " + publishDate + ")";
            str += " (note: " + note + ")";
            return str;
        }


}
}

