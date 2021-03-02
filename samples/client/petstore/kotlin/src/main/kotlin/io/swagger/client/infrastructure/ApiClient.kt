package io.swagger.client.infrastructure

import okhttp3.*
import java.io.File
import java.io.IOException
import java.nio.file.Files;
import java.util.regex.Pattern

open class ApiClient(val baseUrl: String) {
    companion object {
        protected val ContentType = "Content-Type"
        protected val Accept = "Accept"
        protected val JsonMediaType = "application/json"
        protected val FormDataMediaType = "multipart/form-data"
        protected val XmlMediaType = "application/xml"

        @JvmStatic
        val client by lazy {
            builder.build()
        }

        @JvmStatic
        val builder: OkHttpClient.Builder = OkHttpClient.Builder()

        @JvmStatic
        var defaultHeaders: Map<String, String> by ApplicationDelegates.setOnce(mapOf(ContentType to JsonMediaType, Accept to JsonMediaType))

        @JvmStatic
        val jsonHeaders: Map<String, String> = mapOf(ContentType to JsonMediaType, Accept to JsonMediaType)
    }

    inline protected fun <reified T> requestBody(content: T, mediaType: String = JsonMediaType): RequestBody {
        if(content is File) {
            return RequestBody.create(
                    MediaType.parse(mediaType), content
            )
        } else if(mediaType == FormDataMediaType) {
           val requestBodyBuilder = MultipartBody.Builder().setType(MultipartBody.FORM)

            // content's type *must* be Map<String, Any>
            @Suppress("UNCHECKED_CAST")
            (content as Map<String,Any>).forEach { key, value ->
                if(value::class == File::class) {
                    val file = value as File
                    requestBodyBuilder.addFormDataPart(key, file.name, RequestBody.create(MediaType.parse("application/octet-stream"), file))
                } else {
                    val stringValue = value as String
                    requestBodyBuilder.addFormDataPart(key, stringValue)
                }
                TODO("Handle other types inside FormDataMediaType")
            }

            return requestBodyBuilder.build()
        }  else if(mediaType == JsonMediaType) {
            return RequestBody.create(
                    MediaType.parse(mediaType), Serializer.moshi.adapter(T::class.java).toJson(content)
            )
        } else if (mediaType == XmlMediaType) {
            TODO("xml not currently supported.")
        }

        // TODO: this should be extended with other serializers
        TODO("requestBody currently only supports JSON body and File body.")
    }

    inline protected fun <reified T: Any?> responseBody(response: Response, mediaType: String = JsonMediaType): T? {
        if(response.body() == null) return null

        if(T::class.java == java.io.File::class.java){
            return downloadFileFromResponse(response) as T
        } else if(T::class == kotlin.Unit::class) {
            return kotlin.Unit as T
        }

        var contentType = response.headers().get("Content-Type")

        if(contentType == null) {
            contentType = JsonMediaType
        }

		if(isJsonMime(contentType)){
            return Serializer.moshi.adapter(T::class.java).fromJson(response.body()?.source())
        } else if(contentType.equals(String.javaClass)){
            return response.body().toString() as T
        } else {
            TODO("Fill in more types!")
        }
    }

    fun isJsonMime(mime: String?): Boolean {
        val jsonMime = "(?i)^(application/json|[^;/ \t]+/[^;/ \t]+[+]json)[ \t]*(;.*)?$"
        return mime != null && (mime.matches(jsonMime.toRegex()) || mime == "*/*")
    }

    inline protected fun <reified T: Any?> request(requestConfig: RequestConfig, body : Any? = null): ApiInfrastructureResponse<T?> {
        val httpUrl = HttpUrl.parse(baseUrl) ?: throw IllegalStateException("baseUrl is invalid.")

        var urlBuilder = httpUrl.newBuilder()
                .addPathSegments(requestConfig.path.trimStart('/'))

        requestConfig.query.forEach { query ->
            query.value.forEach { queryValue ->
                urlBuilder = urlBuilder.addQueryParameter(query.key, queryValue)
            }
        }

        val url = urlBuilder.build()
        val headers = defaultHeaders + requestConfig.headers

        if(headers[ContentType] ?: "" == "") {
            throw kotlin.IllegalStateException("Missing Content-Type header. This is required.")
        }

        if(headers[Accept] ?: "" == "") {
            throw kotlin.IllegalStateException("Missing Accept header. This is required.")
        }

        // TODO: support multiple contentType,accept options here.
        val contentType = (headers[ContentType] as String).substringBefore(";").toLowerCase()
        val accept = (headers[Accept] as String).substringBefore(";").toLowerCase()

        var request : Request.Builder =  when (requestConfig.method) {
            RequestMethod.DELETE -> Request.Builder().url(url).delete()
            RequestMethod.GET -> Request.Builder().url(url)
            RequestMethod.HEAD -> Request.Builder().url(url).head()
            RequestMethod.PATCH -> Request.Builder().url(url).patch(requestBody(body, contentType))
            RequestMethod.PUT -> Request.Builder().url(url).put(requestBody(body, contentType))
            RequestMethod.POST -> Request.Builder().url(url).post(requestBody(body, contentType))
            RequestMethod.OPTIONS -> Request.Builder().url(url).method("OPTIONS", null)
        }

        headers.forEach { header -> request = request.addHeader(header.key, header.value) }

        val realRequest = request.build()
        val response = client.newCall(realRequest).execute()

        // TODO: handle specific mapping types. e.g. Map<int, Class<?>>
        when {
            response.isRedirect -> return Redirection(
                    response.code(),
                    response.headers().toMultimap()
            )
            response.isInformational -> return Informational(
                    response.message(),
                    response.code(),
                    response.headers().toMultimap()
            )
            response.isSuccessful -> return Success(
                    responseBody(response, accept),
                    response.code(),
                    response.headers().toMultimap()
            )
            response.isClientError -> return ClientError(
                    response.body()?.string(),
                    response.code(),
                    response.headers().toMultimap()
            )
            else -> return ServerError(
                    null,
                    response.body()?.string(),
                    response.code(),
                    response.headers().toMultimap()
            )
        }
    }

    @Throws(IOException::class)
    fun downloadFileFromResponse(response: Response): File {
        val file = prepareDownloadFile(response)

        response.body()?.byteStream().use{ input ->
            File(file.path).outputStream().use { input?.copyTo(it) }
        }

        return file
    }

    @Throws(IOException::class)
    fun prepareDownloadFile(response: Response): File {
        var filename: String? = null
        var contentDisposition = response.headers().get("Content-Disposition")

        if(contentDisposition != null && contentDisposition != ""){
            val pattern = Pattern.compile("filename=['\"]?([^'\"\\s]+)['\"]?")
            val matcher = pattern.matcher(contentDisposition)

            if (matcher.find())
                filename = matcher.group(1)
        }
        var prefix: String
        var suffix: String? = null

        if (filename == null) {
            prefix = "download-"
            suffix = ""
        } else {
            val pos = filename.lastIndexOf('.')

            if (pos == -1) {
            prefix = filename + "-";
            } else {
                prefix = filename.substring(0, pos) + "-"
                suffix = filename.substring(pos)
            }
            // File.createTempFile requires the prefix to be at least three characters long
        if (prefix.length < 3)
            prefix = "download-"
        }

        return Files.createTempFile(prefix, suffix).toFile();
    }
}
