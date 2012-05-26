class Definition
  attr_accessor :text, :source, :note, :part_of_speech, :part_of_speech_obj

  # :internal => :external
  def self.attribute_map
  {
      :text => :text, :source => :source, :note => :note, :part_of_speech => :partOfSpeech, :part_of_speech_obj => :partOfSpeechObj

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      Definition.attribute_map.each_pair do |internal, external|
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
    Definition.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
