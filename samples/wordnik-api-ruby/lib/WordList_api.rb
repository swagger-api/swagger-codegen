require "uri"

class WordList_api
  basePath = "http://api.wordnik.com/v4"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.update_word_list (word_list_id,body,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :body=>body,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    Swagger::Request.new(:PUT, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    

  end


  def self.delete_word_list (word_list_id,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make
    

  end


  def self.get_word_list_by_id (word_list_id,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    WordList.new(response)

  end


  def self.add_words_to_word_list (word_list_id,body,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :body=>body,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}/words".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    

  end


  def self.get_word_list_words (word_list_id,skip,limit,auth_token,sort_by= "createDate",sort_order= "desc",opts={})
    query_param_keys = [
      :sort_by, :sort_order, :skip, :limit]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :skip=>skip,
    :limit=>limit,
    :auth_token=>auth_token,
    :sort_by=>sort_by,
    :sort_order=>sort_order,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}/words".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|WordListWord.new(response)}

  end


  def self.delete_words_from_word_list (word_list_id,body,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "word_list_id is required" if word_list_id.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :word_list_id=>word_list_id,
    :body=>body,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/wordList.{format}/{wordListId}/deleteWords".sub('{format}','json').sub('{' + 'wordListId' + '}', escapeString(word_list_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      auth_token: auth_token,
    }
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    

  end


  end
