class WordList
  attr_accessor :id, :updated_at, :username, :permalink, :description, :created_at, :last_activity_at, :name, :user_id, :number_words_in_list, :type

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :updated_at => :updatedAt, :username => :username, :permalink => :permalink, :description => :description, :created_at => :createdAt, :last_activity_at => :lastActivityAt, :name => :name, :user_id => :userId, :number_words_in_list => :numberWordsInList, :type => :type

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      WordList.attribute_map.each_pair do |internal, external|
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
    WordList.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
