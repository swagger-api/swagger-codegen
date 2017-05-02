defmodule SwaggerPetstore.Api.Catalog do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Catalog.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io/v2"
  plug Tesla.Middleware.JSON

  @doc """
  Definition 1

  
  """
  def definition1() do
    method = [method: :get]
    url = [url: "/api/fakedef1"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Definition 2

  
  """
  def definition2() do
    method = [method: :get]
    url = [url: "/api/fakedef2"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Definition 3

  
  """
  def definition3() do
    method = [method: :get]
    url = [url: "/api/fakedef3"]
    query_params = []
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end
end
