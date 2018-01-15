package io.swagger.server

import com.codahale.metrics.*
import io.ktor.application.*
import io.ktor.features.*
import io.ktor.locations.*
import io.ktor.metrics.*
import io.ktor.routing.*
import java.util.concurrent.*
import io.swagger.server.apis.*


fun Application.main() {
    install(DefaultHeaders)
    install(Metrics) {
        val reporter = Slf4jReporter.forRegistry(registry)
                .outputTo(log)
                .convertRatesTo(TimeUnit.SECONDS)
                .convertDurationsTo(TimeUnit.MILLISECONDS)
                .build();
        reporter.start(10, TimeUnit.SECONDS);
    }
    install(Locations)
    install(Routing) {
        PetApi()
        StoreApi()
        UserApi()
    }
}