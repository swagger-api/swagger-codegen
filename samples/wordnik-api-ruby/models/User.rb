class User
  attr_accessor :id, :username, :status, :email, :face_book_id, :user_name, :display_name, :password

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :username => :username, :status => :status, :email => :email, :face_book_id => :faceBookId, :user_name => :userName, :display_name => :displayName, :password => :password

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
