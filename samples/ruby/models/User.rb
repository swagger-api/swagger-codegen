class User
  attr_accessor :id, :last_name, :username, :phone, :email, :user_status, :first_name, :password

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :last_name => :lastName, :username => :username, :phone => :phone, :email => :email, :user_status => :userStatus, :first_name => :firstName, :password => :password

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      User.attribute_map.each_pair do |internal, external|
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
    User.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end

