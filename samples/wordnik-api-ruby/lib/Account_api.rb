require "uri"

class Account_api
  basePath = "http://api.wordnik.com/v4"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.authenticate (username,password,opts={})
    query_param_keys = [
      :password]
    
    # verify existence of params
    raise "username is required" if username.nil?
    raise "password is required" if password.nil?
    # set default values and merge with input
    options = {
      :username=>username,
    :password=>password,
    }.merge(opts)

    #resource path
    path = "/account.{format}/authenticate/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    AuthenticationToken.new(response)

  end


  def self.authenticate_post (username,body,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "username is required" if username.nil?
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
      :username=>username,
    :body=>body,
    }.merge(opts)

    #resource path
    path = "/account.{format}/authenticate/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make.body
    AuthenticationToken.new(response)

  end


  def self.get_word_lists_for_current_user (api_key,auth_token,skip,limit,opts={})
    query_param_keys = [
      :skip, :limit]
    
    # verify existence of params
    raise "api_key is required" if api_key.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :api_key=>api_key,
    :auth_token=>auth_token,
    :skip=>skip,
    :limit=>limit,
    }.merge(opts)

    #resource path
    path = "/account.{format}/wordLists".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      api_key: api_key,
    }
    headers = {
      auth_token: auth_token,
    }
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|WordList.new(response)}

  end


  def self.get_api_token_status (api_key,opts={})
    # set default values and merge with input
    options = {
      :api_key=>api_key,
    }.merge(opts)

    #resource path
    path = "/account.{format}/apiTokenStatus".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      api_key: api_key,
    }
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    ApiTokenStatus.new(response)

  end


  def self.get_logged_in_user (api_key,auth_token,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "api_key is required" if api_key.nil?
    raise "auth_token is required" if auth_token.nil?
    # set default values and merge with input
    options = {
      :api_key=>api_key,
    :auth_token=>auth_token,
    }.merge(opts)

    #resource path
    path = "/account.{format}/user".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    headers = {
      api_key: api_key,
    }
    headers = {
      auth_token: auth_token,
    }
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    User.new(response)

  end


  end
