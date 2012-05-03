require "uri"

class Pet_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.get_pet_by_id (pet_id,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "pet_id is required" if pet_id.nil?
    # set default values and merge with input
    options = {
      :pet_id=>pet_id,
    }.merge(opts)

    #resource path
    path = "/pet.{format}/{petId}".sub('{format}','json').sub('{' + 'petId' + '}', escapeString(pet_id))
    
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    Pet.new(response)
  end

  def self.add_pet (body,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
      :body=>body,
    }.merge(opts)

    #resource path
    path = "/pet.{format}".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    
  end

  def self.update_pet (body,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
      :body=>body,
    }.merge(opts)

    #resource path
    path = "/pet.{format}".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:PUT, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make
    
  end

  def self.find_pets_by_status (status= "available",opts={})
    query_param_keys = [
      :status]
    
    # verify existence of params
    raise "status is required" if status.nil?
    # set default values and merge with input
    options = {
      :status=>status,
    }.merge(opts)

    #resource path
    path = "/pet.{format}/findByStatus".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Pet.new(response)}
  end

  def self.find_pets_by_tags (tags,opts={})
    query_param_keys = [
      :tags]
    
    # verify existence of params
    raise "tags is required" if tags.nil?
    # set default values and merge with input
    options = {
      :tags=>tags,
    }.merge(opts)

    #resource path
    path = "/pet.{format}/findByTags".sub('{format}','json')
    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    response.map {|response|Pet.new(response)}
  end

  end

