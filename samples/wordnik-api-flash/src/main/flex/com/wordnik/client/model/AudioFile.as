package com.wordnik.client.model {

[XmlRootNode(name="AudioFile")]
    public class AudioFile {
    [XmlElement(name="attributionUrl")]
        public var attributionUrl: String = null;

    [XmlElement(name="commentCount")]
        public var commentCount: Number = 0.0;

    [XmlElement(name="voteCount")]
        public var voteCount: Number = 0.0;

    [XmlElement(name="fileUrl")]
        public var fileUrl: String = null;

    [XmlElement(name="audioType")]
        public var audioType: String = null;

    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="duration")]
        public var duration: Number = 0.0;

    [XmlElement(name="attributionText")]
        public var attributionText: String = null;

    [XmlElement(name="createdBy")]
        public var createdBy: String = null;

    [XmlElement(name="description")]
        public var description: String = null;

    [XmlElement(name="createdAt")]
        public var createdAt: Date = null;

    [XmlElement(name="voteWeightedAverage")]
        public var voteWeightedAverage: Number = 0.0;

    [XmlElement(name="voteAverage")]
        public var voteAverage: Number = 0.0;

    [XmlElement(name="word")]
        public var word: String = null;

    public function toString(): String {
            var str: String = "AudioFile: ";
            str += " (attributionUrl: " + attributionUrl + ")";
            str += " (commentCount: " + commentCount + ")";
            str += " (voteCount: " + voteCount + ")";
            str += " (fileUrl: " + fileUrl + ")";
            str += " (audioType: " + audioType + ")";
            str += " (id: " + id + ")";
            str += " (duration: " + duration + ")";
            str += " (attributionText: " + attributionText + ")";
            str += " (createdBy: " + createdBy + ")";
            str += " (description: " + description + ")";
            str += " (createdAt: " + createdAt + ")";
            str += " (voteWeightedAverage: " + voteWeightedAverage + ")";
            str += " (voteAverage: " + voteAverage + ")";
            str += " (word: " + word + ")";
            return str;
        }


}
}

