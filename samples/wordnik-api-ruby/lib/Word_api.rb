require "uri"

class Word_api
  basePath = "http://api.wordnik.com/v4"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.get_examples (word,include_duplicates,content_provider,use_canonical,skip,limit,opts={})
    query_param_keys = [
      :include_duplicates, :content_provider, :use_canonical, :skip, :limit]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :include_duplicates=>include_duplicates,
    :content_provider=>content_provider,
    :use_canonical=>use_canonical,
    :skip=>skip,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/examples".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    ExampleSearchResults.new(response)

  end


  def self.get_word (word,include_suggestions,use_canonical= "false",opts={})
    query_param_keys = [
      :use_canonical, :include_suggestions]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :include_suggestions=>include_suggestions,
    :use_canonical=>use_canonical,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    WordObject.new(response)

  end


  def self.get_definitions (word,limit,part_of_speech,source_dictionaries,include_related= "false",use_canonical= "false",include_tags= "false",opts={})
    query_param_keys = [
      :limit, :part_of_speech, :include_related, :source_dictionaries, :use_canonical, :include_tags]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :limit=>limit,
    :part_of_speech=>part_of_speech,
    :source_dictionaries=>source_dictionaries,
    :include_related=>include_related,
    :use_canonical=>use_canonical,
    :include_tags=>include_tags,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/definitions".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Definition.new(response)}

  end


  def self.get_top_example (word,content_provider,use_canonical,opts={})
    query_param_keys = [
      :content_provider, :use_canonical]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :content_provider=>content_provider,
    :use_canonical=>use_canonical,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/topExample".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    Example.new(response)

  end


  def self.get_text_pronunciations (word,use_canonical,source_dictionary,type_format,limit,opts={})
    query_param_keys = [
      :use_canonical, :source_dictionary, :type_format, :limit]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :use_canonical=>use_canonical,
    :source_dictionary=>source_dictionary,
    :type_format=>type_format,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/pronunciations".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|TextPron.new(response)}

  end


  def self.get_hyphenation (word,use_canonical,source_dictionary,limit,opts={})
    query_param_keys = [
      :use_canonical, :source_dictionary, :limit]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :use_canonical=>use_canonical,
    :source_dictionary=>source_dictionary,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/hyphenation".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Syllable.new(response)}

  end


  def self.get_word_frequency (word,use_canonical,start_year,end_year,opts={})
    query_param_keys = [
      :use_canonical, :start_year, :end_year]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :use_canonical=>use_canonical,
    :start_year=>start_year,
    :end_year=>end_year,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/frequency".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    FrequencySummary.new(response)

  end


  def self.get_phrases (word,limit,wlmi,use_canonical,opts={})
    query_param_keys = [
      :limit, :wlmi, :use_canonical]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :limit=>limit,
    :wlmi=>wlmi,
    :use_canonical=>use_canonical,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/phrases".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Bigram.new(response)}

  end


  def self.get_related (word,part_of_speech,source_dictionary,limit,use_canonical,type,opts={})
    query_param_keys = [
      :part_of_speech, :source_dictionary, :limit, :use_canonical, :type]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :part_of_speech=>part_of_speech,
    :source_dictionary=>source_dictionary,
    :limit=>limit,
    :use_canonical=>use_canonical,
    :type=>type,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/related".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Related.new(response)}

  end


  def self.get_audio (word,use_canonical,limit,opts={})
    query_param_keys = [
      :use_canonical, :limit]
    
    # verify existence of params
    raise "word is required" if word.nil?
    # set default values and merge with input
    options = {
      :word=>word,
    :use_canonical=>use_canonical,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/word.{format}/{word}/audio".sub('{format}','json').sub('{' + 'word' + '}', escapeString(word))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|AudioFile.new(response)}

  end


  end
