class Pet
  attr_accessor :id, :tags, :category, :status, :name, :photo_urls

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :tags => :tags, :category => :category, :status => :status, :name => :name, :photo_urls => :photoUrls

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      Pet.attribute_map.each_pair do |internal, external|
        if attributes.has_key? external.to_s
          attributes[internal.to_s] = attributes.delete(external.to_s)
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
    Pet.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

