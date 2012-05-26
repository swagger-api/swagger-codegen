class ExampleSearchResults
  attr_accessor :facets, :examples

  # :internal => :external
  def self.attribute_map
  {
      :facets => :facets, :examples => :examples

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      ExampleSearchResults.attribute_map.each_pair do |internal, external|
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
    ExampleSearchResults.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
