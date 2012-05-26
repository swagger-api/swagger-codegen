class ApiTokenStatus
  attr_accessor :valid, :token, :resets_in_millis, :remaining_calls, :expires_in_millis, :total_requests

  # :internal => :external
  def self.attribute_map
  {
      :valid => :valid, :token => :token, :resets_in_millis => :resetsInMillis, :remaining_calls => :remainingCalls, :expires_in_millis => :expiresInMillis, :total_requests => :totalRequests

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      ApiTokenStatus.attribute_map.each_pair do |internal, external|
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
    ApiTokenStatus.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
