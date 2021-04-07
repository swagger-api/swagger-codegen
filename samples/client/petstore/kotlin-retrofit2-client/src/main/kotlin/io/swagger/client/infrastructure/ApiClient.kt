package io.swagger.client.infrastructure

import com.google.gson.Gson
import com.google.gson.JsonParseException
import okhttp3.OkHttpClient
import okhttp3.RequestBody
import okhttp3.ResponseBody
import retrofit2.Converter
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.converter.scalars.ScalarsConverterFactory

import java.lang.reflect.Type
import java.text.DateFormat
import java.time.format.DateTimeFormatter

class ApiClient(baseUrl:String = "http://petstore.swagger.io:80/v2") {

  private lateinit var okBuilder: OkHttpClient.Builder
  private lateinit var adapterBuilder: Retrofit.Builder
  private lateinit var json: JSON

  init {
    createDefaultAdapter(baseUrl)
  }

  private fun createDefaultAdapter(baseUrl: String) {
    json = JSON()
    okBuilder = OkHttpClient.Builder()

    val url = if (!baseUrl.endsWith("/")) "$baseUrl/" else baseUrl

    adapterBuilder = Retrofit
            .Builder()
            .baseUrl(url)
            .addConverterFactory(ScalarsConverterFactory.create())
            .addConverterFactory(GsonCustomConverterFactory.create(json.gson))
  }

  fun <S> createService(serviceClass: Class<S>):S {
    return adapterBuilder
            .client(okBuilder.build())
            .build()
            .create(serviceClass)
  }

  fun setDateFormat(dateFormat: DateFormat): ApiClient {
    this.json.setDateFormat(dateFormat)
    return this
  }

  fun setSqlDateFormat(dateFormat: DateFormat): ApiClient {
    this.json.setSqlDateFormat(dateFormat)
    return this
  }

  fun setOffsetDateTimeFormat(dateFormat: DateTimeFormatter): ApiClient {
    this.json.setOffsetDateTimeFormat(dateFormat)
    return this
  }

  fun setLocalDateFormat(dateFormat: DateTimeFormatter): ApiClient {
    this.json.setLocalDateFormat(dateFormat)
    return this
  }

}

/**
 * This wrapper is to take care of this case:
 * when the deserialization fails due to JsonParseException and the
 * expected type is String, then just return the body string.
 */
class GsonResponseBodyConverterToString<T>(
  private val gson: Gson,
  private val type: Type
) : Converter<ResponseBody, T> {

  @Suppress("UNCHECKED_CAST")
  override fun convert(value: ResponseBody): T {
    val returned = value.string()
    return try {
      gson.fromJson(returned, type)
    } catch (e: JsonParseException) {
      returned as T
    }
  }
}

class GsonCustomConverterFactory(
  private val gson: Gson
) : Converter.Factory() {
  companion object {
    fun create(gson: Gson): GsonCustomConverterFactory {
      return GsonCustomConverterFactory(gson)
    }
  }

  private val gsonConverterFactory: GsonConverterFactory = GsonConverterFactory.create(gson)

  override fun responseBodyConverter(type: Type, annotations: Array<out Annotation>, retrofit: Retrofit): Converter<ResponseBody, *>? {
    return if (type == String::class)
      GsonResponseBodyConverterToString<Any>(gson, type)
    else
      gsonConverterFactory.responseBodyConverter(type, annotations, retrofit)
  }

  override fun requestBodyConverter(type: Type, parameterAnnotations: Array<out Annotation>, methodAnnotations: Array<out Annotation>, retrofit: Retrofit): Converter<*, RequestBody>? {
    return gsonConverterFactory.requestBodyConverter(type, parameterAnnotations, methodAnnotations, retrofit)
  }
}