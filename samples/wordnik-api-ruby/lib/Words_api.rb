require "uri"

class Words_api
  basePath = "http://api.wordnik.com/v4"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.search_words (query,include_part_of_speech,exclude_part_of_speech,max_corpus_count,max_dictionary_count,max_length,case_sensitive= "true",min_corpus_count= "5",min_dictionary_count= "1",min_length= "1",skip= "0",limit= "10",opts={})
    query_param_keys = [
      :query, :case_sensitive, :include_part_of_speech, :exclude_part_of_speech, :min_corpus_count, :max_corpus_count, :min_dictionary_count, :max_dictionary_count, :min_length, :max_length, :skip, :limit]
    
    # verify existence of params
    raise "query is required" if query.nil?
    # set default values and merge with input
    options = {
      :query=>query,
    :include_part_of_speech=>include_part_of_speech,
    :exclude_part_of_speech=>exclude_part_of_speech,
    :max_corpus_count=>max_corpus_count,
    :max_dictionary_count=>max_dictionary_count,
    :max_length=>max_length,
    :case_sensitive=>case_sensitive,
    :min_corpus_count=>min_corpus_count,
    :min_dictionary_count=>min_dictionary_count,
    :min_length=>min_length,
    :skip=>skip,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/words.{format}/search".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|WordFrequency.new(response)}

  end


  def self.get_word_of_the_day (date,category,creator,opts={})
    # set default values and merge with input
    options = {
      :date=>date,
    :category=>category,
    :creator=>creator,
    }.merge(opts)

    #resource path
    path = "/words.{format}/wordOfTheDay".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    WordOfTheDay.new(response)

  end


  def self.search_words_new (query,include_part_of_speech,exclude_part_of_speech,max_corpus_count,max_dictionary_count,max_length,case_sensitive= "true",min_corpus_count= "5",min_dictionary_count= "1",min_length= "1",skip= "0",limit= "10",opts={})
    query_param_keys = [
      :case_sensitive, :include_part_of_speech, :exclude_part_of_speech, :min_corpus_count, :max_corpus_count, :min_dictionary_count, :max_dictionary_count, :min_length, :max_length, :skip, :limit]
    
    # verify existence of params
    raise "query is required" if query.nil?
    # set default values and merge with input
    options = {
      :query=>query,
    :include_part_of_speech=>include_part_of_speech,
    :exclude_part_of_speech=>exclude_part_of_speech,
    :max_corpus_count=>max_corpus_count,
    :max_dictionary_count=>max_dictionary_count,
    :max_length=>max_length,
    :case_sensitive=>case_sensitive,
    :min_corpus_count=>min_corpus_count,
    :min_dictionary_count=>min_dictionary_count,
    :min_length=>min_length,
    :skip=>skip,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/words.{format}/search/{query}".sub('{format}','json').sub('{' + 'query' + '}', escapeString(query))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    WordSearchResults.new(response)

  end


  def self.get_random_words (has_dictionary_def,include_part_of_speech,exclude_part_of_speech,min_corpus_count,max_corpus_count,min_dictionary_count,max_dictionary_count,min_length,max_length,sort_by,sort_order,limit,opts={})
    # set default values and merge with input
    options = {
      :has_dictionary_def=>has_dictionary_def,
    :include_part_of_speech=>include_part_of_speech,
    :exclude_part_of_speech=>exclude_part_of_speech,
    :min_corpus_count=>min_corpus_count,
    :max_corpus_count=>max_corpus_count,
    :min_dictionary_count=>min_dictionary_count,
    :max_dictionary_count=>max_dictionary_count,
    :min_length=>min_length,
    :max_length=>max_length,
    :sort_by=>sort_by,
    :sort_order=>sort_order,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/words.{format}/randomWords".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|WordObject.new(response)}

  end


  def self.get_random_word (include_part_of_speech,exclude_part_of_speech,min_corpus_count,max_corpus_count,min_dictionary_count,max_dictionary_count,min_length,max_length,has_dictionary_def= "true",opts={})
    # set default values and merge with input
    options = {
      :include_part_of_speech=>include_part_of_speech,
    :exclude_part_of_speech=>exclude_part_of_speech,
    :min_corpus_count=>min_corpus_count,
    :max_corpus_count=>max_corpus_count,
    :min_dictionary_count=>min_dictionary_count,
    :max_dictionary_count=>max_dictionary_count,
    :min_length=>min_length,
    :max_length=>max_length,
    :has_dictionary_def=>has_dictionary_def,
    }.merge(opts)

    #resource path
    path = "/words.{format}/randomWord".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    WordObject.new(response)

  end


  end
