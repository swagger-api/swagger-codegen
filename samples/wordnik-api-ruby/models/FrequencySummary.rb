class FrequencySummary
  attr_accessor :unknown_year_count, :total_count, :frequency_string, :word, :frequency

  # :internal => :external
  def self.attribute_map
  {
      :unknown_year_count => :unknownYearCount, :total_count => :totalCount, :frequency_string => :frequencyString, :word => :word, :frequency => :frequency

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      FrequencySummary.attribute_map.each_pair do |internal, external|
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
    FrequencySummary.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
