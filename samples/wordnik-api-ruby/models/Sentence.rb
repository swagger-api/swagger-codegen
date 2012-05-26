class Sentence
  attr_accessor :id, :has_scored_words, :scored_words, :display, :rating, :document_metadata_id

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :has_scored_words => :hasScoredWords, :scored_words => :scoredWords, :display => :display, :rating => :rating, :document_metadata_id => :documentMetadataId

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      Sentence.attribute_map.each_pair do |internal, external|
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
    Sentence.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
