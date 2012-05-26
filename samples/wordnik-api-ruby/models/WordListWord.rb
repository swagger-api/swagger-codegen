class WordListWord
  attr_accessor :id, :username, :created_at, :user_id, :number_comments_on_word, :word, :number_lists

  # :internal => :external
  def self.attribute_map
  {
      :id => :id, :username => :username, :created_at => :createdAt, :user_id => :userId, :number_comments_on_word => :numberCommentsOnWord, :word => :word, :number_lists => :numberLists

  }
  end

  def initialize(attributes = {})
    # Morph attribute keys into undescored rubyish style
    if attributes.to_s != ""
      WordListWord.attribute_map.each_pair do |internal, external|
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
    WordListWord.attribute_map.each_pair do |key,value|
      body[value] = self.send(key) unless self.send(key).nil?
    end
    body
  end
end
