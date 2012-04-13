require "uri"

class User_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.create_user (body,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
      :body=>body,
    }.merge(opts)

    #resource path
    path = "/user.{format}".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    

  end


  def self.update_user (username,body,opts={})
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
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:PUT, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    

  end


  def self.delete_user (username,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "username is required" if username.nil?
    # set default values and merge with input
    options = {
      :username=>username,
    }.merge(opts)

    #resource path
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make
    

  end


  def self.get_user_by_name (username,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "username is required" if username.nil?
    # set default values and merge with input
    options = {
      :username=>username,
    }.merge(opts)

    #resource path
    path = "/user.{format}/{username}".sub('{format}','json').sub('{' + 'username' + '}', escapeString(username))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    User.new(response)

  end


  def self.login_user (username,password,opts={})
    query_param_keys = [
      :username, :password]
    
    # verify existence of params
    raise "username is required" if username.nil?
    raise "password is required" if password.nil?
    # set default values and merge with input
    options = {
      :username=>username,
    :password=>password,
    }.merge(opts)

    #resource path
    path = "/user.{format}/login".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    String.new(response)

  end


  def self.logout_user (opts={})
    # set default values and merge with input
    options = {
      }.merge(opts)

    #resource path
    path = "/user.{format}/logout".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make
    

  end


  end
