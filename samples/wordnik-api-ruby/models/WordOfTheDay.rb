class WordOfTheDay
  attr_accessor :id, :parent_id, :category, :created_by, :created_at, :content_provider, :word, :html_extra, :definitions, :examples, :publish_date, :note

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :parent_id => :parentId, :category => :category, :created_by => :createdBy, :created_at => :createdAt, :content_provider => :contentProvider, :word => :word, :html_extra => :htmlExtra, :definitions => :definitions, :examples => :examples, :publish_date => :publishDate, :note => :note

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      WordOfTheDay.attribute_map.each_pair do |internal, external|
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
    WordOfTheDay.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
