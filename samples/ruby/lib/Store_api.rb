require "uri"

class Store_api
  basePath = "http://petstore.swagger.wordnik.com/api"
  # apiInvoker = APIInvoker

  def self.escapeString(string)
    URI.encode(string.to_s)
  end

  def self.get_order_by_id (order_id,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "order_id is required" if order_id.nil?
    # set default values and merge with input
    options = {
      :order_id=>order_id,
    }.merge(opts)

    #resource path
    path = "/store.{format}/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:GET, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make.body
    Order.new(response)

  end


  def self.delete_order (order_id,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "order_id is required" if order_id.nil?
    # set default values and merge with input
    options = {
      :order_id=>order_id,
    }.merge(opts)

    #resource path
    path = "/store.{format}/order/{orderId}".sub('{format}','json').sub('{' + 'orderId' + '}', escapeString(order_id))
    

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    Swagger::Request.new(:DELETE, path, {:params=>queryopts,:headers=>nil, :body=>nil}).make
    

  end


  def self.place_order (body,opts={})
    query_param_keys = [
      ]
    
    # verify existence of params
    raise "body is required" if body.nil?
    # set default values and merge with input
    options = {
      :body=>body,
    }.merge(opts)

    #resource path
    path = "/store.{format}/order".sub('{format}','json')

    
    # pull querystring keys from options
    queryopts = options.select do |key,value|
      query_param_keys.include? key
    end
    
    response = Swagger::Request.new(:POST, path, {:params=>queryopts,:headers=>nil, :body=>body.to_body}).make.body
    Order.new(response)

  end


  end
