class ScoredWord
  attr_accessor :id, :position, :lemma, :doc_term_count, :word_type, :score, :word, :sentence_id, :stopword, :base_word_score, :part_of_speech

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :position => :position, :lemma => :lemma, :doc_term_count => :docTermCount, :word_type => :wordType, :score => :score, :word => :word, :sentence_id => :sentenceId, :stopword => :stopword, :base_word_score => :baseWordScore, :part_of_speech => :partOfSpeech

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      ScoredWord.attribute_map.each_pair do |internal, external|
        if attributes.has_key? external
          attributes[internal] = attributes.delete(external)
        end
      end

      # Assign attributes
      attributes.each do |name, value|
        send("#{name}=", value) if self.respond_to?(name.to_sym)
      end
    end
  end

  def to_body
    body = {}
    ScoredWord.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
