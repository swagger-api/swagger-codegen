class Order
  attr_accessor :id, :pet_id, :status, :quantity, :ship_date

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :pet_id => :petId, :status => :status, :quantity => :quantity, :ship_date => :shipDate

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      Order.attribute_map.each_pair do |internal, external|
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
    Order.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

