using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using Newtonsoft.Json;

namespace io.swagger.client 
{
public class ApiInvoker
    {
        private static readonly ApiInvoker Instance = new ApiInvoker();
        private readonly Dictionary<String, String> _defaultHeaderMap = new Dictionary<String, String>();

        public static ApiInvoker GetInstance()
        {
            return Instance;
        }

        public void AddDefaultHeader(string key, string value)
        {
            _defaultHeaderMap.Add(key, value);
        }

        public string EscapeString(string str)
        {
            return str;
        }

        public static object Deserialize(string json, Type type)
        {
            try
            {
                return JsonConvert.DeserializeObject(json, type);
            }
            catch (IOException e)
            {
                throw new ApiException(500, e.Message);
            }
        }

        public static string Serialize(object obj)
        {
            try
            {
                return obj != null ? JsonConvert.SerializeObject(obj) : null;
            }
            catch (Exception e)
            {
                throw new ApiException(500, e.Message);
            }
        }

        public string InvokeApi(string host, string path, string method, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams)
        {
            return InvokeApiInternal(host, path, method, false, queryParams, body, headerParams, formParams) as string;
        }

        public byte[] InvokeBinaryApi(string host, string path, string method, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams)
        {
            return InvokeApiInternal(host, path, method, true, queryParams, body, headerParams, formParams) as byte[];
        }

        private object InvokeApiInternal(string host, string path, string method, bool binaryResponse, Dictionary<String, String> queryParams, object body, Dictionary<String, String> headerParams, Dictionary<String, object> formParams)
        {
            var querystring = CreateQueryString(queryParams);

            host = host.EndsWith("/") ? host.Substring(0, host.Length - 1) : host;

            var client = WebRequest.Create(host + path + querystring);
            client.Method = method;

            byte[] formData = null;
            if (formParams.Count > 0)
            {
                var formDataBoundary = String.Format("----------{0:N}", Guid.NewGuid());
                client.ContentType = "multipart/form-data; boundary=" + formDataBoundary;
                formData = GetMultipartFormData(formParams, formDataBoundary);
                client.ContentLength = formData.Length;
            }
            else
            {
                client.ContentType = "application/json";
            }

            foreach (var headerParamsItem in headerParams)
            {
                client.Headers.Add(headerParamsItem.Key, headerParamsItem.Value);
            }
            foreach (var defaultHeaderMapItem in _defaultHeaderMap.Where(defaultHeaderMapItem => !headerParams.ContainsKey(defaultHeaderMapItem.Key)))
            {
                client.Headers.Add(defaultHeaderMapItem.Key, defaultHeaderMapItem.Value);
            }

            switch (method)
            {
                case "GET":
                    break;
                case "POST":
                case "PUT":
                case "DELETE":
                    using (var requestStream = client.GetRequestStream())
                    {
                        if (formData != null)
                        {
                            requestStream.Write(formData, 0, formData.Length);
                        }

                        var swRequestWriter = new StreamWriter(requestStream);
                        swRequestWriter.Write(Serialize(body));
                        swRequestWriter.Close();
                    }
                    break;
                default:
                    throw new ApiException(500, "unknown method type " + method);
            }

            try
            {
                var webResponse = (HttpWebResponse)client.GetResponse();
                if (webResponse.StatusCode != HttpStatusCode.OK)
                {
                    webResponse.Close();
                    throw new ApiException((int)webResponse.StatusCode, webResponse.StatusDescription);
                }

                if (binaryResponse)
                {
                    using (var memoryStream = new MemoryStream())
                    {
                        webResponse.GetResponseStream().CopyTo(memoryStream);
                        return memoryStream.ToArray();
                    }
                }
                else
                {
                    using (var responseReader = new StreamReader(webResponse.GetResponseStream()))
                    {
                        var responseData = responseReader.ReadToEnd();
                        return responseData;
                    }
                }
            }
            catch (WebException ex)
            {
                using (var response = ex.Response as HttpWebResponse)
                {
                    var statusCode = 0;
                    if (response != null)
                    {
                        statusCode = (int)response.StatusCode;
                    }
                    throw new ApiException(statusCode, ex);
                }
            }
        }

        private string CreateQueryString(Dictionary<string, string> queryParams)
        {
            var builder = new StringBuilder();

            foreach (var queryParamItem in queryParams)
            {
                var value = queryParamItem.Value;
                if (value == null) continue;
                builder.Append(builder.Length == 0 ? "?" : "&");
                builder.Append(EscapeString(queryParamItem.Key))
                    .Append("=")
                    .Append(EscapeString(value));
            }

            var querystring = builder.ToString();
            return querystring;
        }

        private static byte[] GetMultipartFormData(Dictionary<string, object> postParameters, string boundary)
        {
            var formDataStream = new MemoryStream();
            var needsCLRF = false;

            foreach (var param in postParameters)
            {
                // Thanks to feedback from commenters, add a CRLF to allow multiple parameters to be added.
                // Skip it on the first parameter, add it to subsequent parameters.
                if (needsCLRF)
                    formDataStream.Write(Encoding.UTF8.GetBytes(Environment.NewLine), 0, Encoding.UTF8.GetByteCount(Environment.NewLine));

                needsCLRF = true;

                if (param.Value is byte[])
                {
                    var postData = string.Format("--{0}{1}Content-Disposition: form-data; name=\"{2}\"; filename=\"{2}\"{1}Content-Type: {3}{1}{1}",
                        boundary,
                        Environment.NewLine,
                        param.Key,
                        "application/octet-stream");
                    formDataStream.Write(Encoding.UTF8.GetBytes(postData), 0, Encoding.UTF8.GetByteCount(postData));

                    // Write the file data directly to the Stream, rather than serializing it to a string.
                    formDataStream.Write((param.Value as byte[]), 0, (param.Value as byte[]).Length);
                }
                else
                {
                    var postData = string.Format("--{0}{1}Content-Disposition: form-data; name=\"{2}\"{1}{3}",
                        boundary,
                        Environment.NewLine,
                        param.Key,
                        param.Value);
                    formDataStream.Write(Encoding.UTF8.GetBytes(postData), 0, Encoding.UTF8.GetByteCount(postData));
                }
            }

            // Add the end of the request.  Start with a newline
            var footer = string.Format("{0}--{1}--{0}", Environment.NewLine, boundary);
            formDataStream.Write(Encoding.UTF8.GetBytes(footer), 0, Encoding.UTF8.GetByteCount(footer));

            // Dump the Stream into a byte[]
            formDataStream.Position = 0;
            var formData = new byte[formDataStream.Length];
            formDataStream.Read(formData, 0, formData.Length);
            formDataStream.Close();

            return formData;
        }
        
         public static String ParameterToQueryString(object param)
        {
            if (param == null)
            {
                return string.Empty;
            }
            if (param is DateTime)
            {
                return ((DateTime)param).ToString("u");
            }
            if (param is ICollection)
            {
                var builder = new StringBuilder();
                foreach (var o in (ICollection)param)
                {
                    if (builder.Length > 0)
                    {
                        builder.Append(",");
                    }
                    builder.Append(o);
                }
                return builder.ToString();
            }
            return param.ToString();
        }
    }
}