package io.swagger.server

// Use this file to hold package-level internal functions that return receiver object passed to the `install` method.
import com.codahale.metrics.*
import com.typesafe.config.*
import io.ktor.application.*
import io.ktor.client.*
import io.ktor.client.engine.apache.*
import io.ktor.config.*
import io.ktor.features.*
import io.ktor.gson.*
import io.ktor.http.*
import io.ktor.locations.*
import io.ktor.metrics.*
import io.ktor.routing.*
import io.ktor.server.engine.*
import io.ktor.auth.*
import java.time.*
import java.util.concurrent.*
import io.swagger.server.infrastructure.*
import io.swagger.server.apis.*


/**
 * Application block for [HSTS] configuration.
 *
 * This file may be excluded in .swagger-codegen-ignore,
 * and application specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/hsts.html
 */
internal fun ApplicationHstsConfiguration(): HSTS.Configuration.() -> Unit {
    return {
        maxAge = Duration.ofDays(365)
        includeSubDomains = true
        preload = false

        // You may also apply any custom directives supported by specific user-agent. For example:
        // customDirectives.put("redirectHttpToHttps", "false")
    }
}

/**
 * Application block for [Compression] configuration.
 *
 * This file may be excluded in .swagger-codegen-ignore,
 * and application specific configuration can be applied in this function.
 *
 * See http://ktor.io/features/compression.html
 */
internal fun ApplicationCompressionConfiguration(): Compression.Configuration.() -> Unit {
    return {
        gzip {
            priority = 1.0
        }
        deflate {
            priority = 10.0
            minimumSize(1024) // condition
        }
    }
}

// Defines authentication mechanisms used throughout the application.
internal fun Application.installAuthProviders() {
    install(Authentication) {
        // "Implement API key auth (api_key) for parameter name 'api_key'."
        apiKey("apikey-api_key") {
            apiKeyName = "api_key"
            authLocation = "header"
            validate {
                // TODO: "Verify key here , accessible as it.value"
                if (it.value == "keyboardcat") {
                    ApiPrincipal(it)
                } else {
                    null
                }
            }
        }
        oauth("oauth-petstore_auth") {
            client = HttpClient(Apache).apply {
                environment.monitor.subscribe(ApplicationStopping) {
                    close()
                }
            }
            providerLookup = {
                OAuthServerSettings.OAuth2ServerSettings(
                    name = "petstore_auth",
                    authorizeUrl = "http://petstore.swagger.io/api/oauth/dialog",
                    accessTokenUrl = "",
                    requestMethod = HttpMethod.Get,
                    clientId = environment.config.property("auth.oauth.petstore_auth.clientId").getString(),
                    clientSecret = environment.config.property("auth.oauth.petstore_auth.clientSecret").getString(),
                    defaultScopes = listOf("write:pets", "read:pets")
                )
            }
            urlProvider = { "/TODO/login/${it.name}" } // TODO: define a callback url here.
        }
    }
}

// Provides an application-level fixed thread pool on which to execute coroutines (mainly)
internal val ApplicationExecutors = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 4)
