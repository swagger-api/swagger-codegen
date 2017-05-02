defmodule SwaggerPetstore.Api.Fake do
  @moduledoc """
  Documentation for SwaggerPetstore.Api.Fake.
  """

  use Tesla

  plug Tesla.Middleware.BaseUrl, "http://petstore.swagger.io:80/v2"
  plug Tesla.Middleware.JSON

  @doc """
  Definition 1

  
  """
  def definition1() do
    method = [method: :get]
    url = [url: "/fakedef1"]
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
    url = [url: "/fakedef2"]
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
    url = [url: "/fakedef3"]
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
  

  submitting JSON
  """
  def post_json(items) do
    method = [method: :get]
    url = [url: "/fakeArrays"]
    query_params = [query: [{:"items", items}]]
    header_params = []
    body_params = []
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  

  submitting JSON
  """
  def post_json_0(items) do
    method = [method: :post]
    url = [url: "/fakeArrays"]
    query_params = []
    header_params = []
    body_params = [body: items]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  

  submitting JSON
  """
  def post_json_1(body) do
    method = [method: :post]
    url = [url: "/fakedef1"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  To test \&quot;client\&quot; model

  To test \&quot;client\&quot; model
  """
  def test_client_model(body) do
    method = [method: :patch]
    url = [url: "/fake"]
    query_params = []
    header_params = []
    body_params = [body: body]
    form_params = []
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

  Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  """
  def test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback) do
    method = [method: :post]
    url = [url: "/fake"]
    query_params = []
    header_params = []
    body_params = []
    form_params = [body: Enum.map_join([{:"integer", integer}, {:"int32", int32}, {:"int64", int64}, {:"number", number}, {:"float", float}, {:"double", double}, {:"string", string}, {:"pattern_without_delimiter", pattern_without_delimiter}, {:"byte", byte}, {:"binary", binary}, {:"date", date}, {:"dateTime", date_time}, {:"password", password}, {:"callback", callback}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end

  @doc """
  To test enum parameters

  To test enum parameters
  """
  def test_enum_parameters(enum_form_string_array, enum_form_string, enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double) do
    method = [method: :get]
    url = [url: "/fake"]
    query_params = [query: [{:"enum_query_string_array", enum_query_string_array}, {:"enum_query_string", enum_query_string}, {:"enum_query_integer", enum_query_integer}]]
    header_params = [header: [{:"enum_header_string_array", enum_header_string_array}, {:"enum_header_string", enum_header_string}]]
    body_params = []
    form_params = [body: Enum.map_join([{:"enum_form_string_array", enum_form_string_array}, {:"enum_form_string", enum_form_string}, {:"enum_query_double", enum_query_double}], "&", &("#{elem(&1, 0)}=#{elem(&1, 1)}"))]
    params = query_params ++ header_params ++ body_params ++ form_params
    opts = []
    options = method ++ url ++ params ++ opts

    request(options)
  end
end
