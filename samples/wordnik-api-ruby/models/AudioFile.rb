class AudioFile
  attr_accessor :attribution_url, :comment_count, :vote_count, :file_url, :audio_type, :id, :duration, :attribution_text, :created_by, :description, :created_at, :vote_weighted_average, :vote_average, :word

  # :internal => :external
  def self.attribute_map
  {
      :attribution_url => :attributionUrl, :comment_count => :commentCount, :vote_count => :voteCount, :file_url => :fileUrl, :audio_type => :audioType, :id => :id, :duration => :duration, :attribution_text => :attributionText, :created_by => :createdBy, :description => :description, :created_at => :createdAt, :vote_weighted_average => :voteWeightedAverage, :vote_average => :voteAverage, :word => :word

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      AudioFile.attribute_map.each_pair do |internal, external|
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
    AudioFile.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
