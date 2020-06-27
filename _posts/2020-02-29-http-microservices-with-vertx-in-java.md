---
title: "HTTP Microservices With Vert.x in Java"
tags: microservices vertx java
---

This article will describe how to create a simple HTTP microservice with Java and the Vert.x library. We'll look at other integration technologies in the future.

**Microservices** are an architectural style for an application where it is composed of independently deployable services. The most common pattern of integrating these services is over HTTP.

The **Vert.x library** was inspired by Node.js. The advantage of choosing Vert.x with Java is it is implemented with multiple threads and can therefore handle more requests or events per process.

## What You'll Need
* [Install JDK on macOS](/2020/02/25/install-jdk-on-macos.html)

## Getting Started

Visit the [Vert.x Starter](https://start.vertx.io/) to create a new projects with Vert.x. In the following screenshot I've selected 3.8.5 for the version, Java for the language, Gradle for the build tool, Vert.x Web for a dependency, and JDK 11 for the JDK version. 

![Create a new Vert.x application](/assets/images/screen_shot_vertx_starter.png)

Finally click the Generate Project button to download the zip file containing the project. Unzip the project to view the project contents.

```zsh
hello-world-service % tree
.
|____.editorconfig
|____gradle
| |____wrapper
| | |____gradle-wrapper.jar
| | |____gradle-wrapper.properties
|____README.adoc
|____gradlew
|____.gitignore
|____build.gradle
|____gradlew.bat
|____settings.gradle
|____src
| |____test
| | |____java
| | | |____com
| | | | |____example
| | | | | |____hello_world_service
| | | | | | |____TestMainVerticle.java
| |____main
| | |____java
| | | |____com
| | | | |____example
| | | | | |____hello_world_service
| | | | | | |____MainVerticle.java
```

## Generated Source

`build.gradle`
```groovy
plugins {
  id 'java'
  id 'application'
  id 'com.github.johnrengelman.shadow' version '5.0.0'
}

group = 'com.example'
version = '1.0.0-SNAPSHOT'

repositories {
  mavenCentral()
}

ext {
  vertxVersion = '3.8.5'
  junitJupiterEngineVersion = '5.4.0'
}

application {
  mainClassName = 'io.vertx.core.Launcher'
}

sourceCompatibility = '11'

def mainVerticleName = 'com.example.hello_world_service.MainVerticle'
def watchForChange = 'src/**/*'
def doOnChange = './gradlew classes'

dependencies {
  implementation "io.vertx:vertx-web:$vertxVersion"

  testImplementation "io.vertx:vertx-junit5:$vertxVersion"
  testRuntimeOnly "org.junit.jupiter:junit-jupiter-engine:$junitJupiterEngineVersion"
  testImplementation "org.junit.jupiter:junit-jupiter-api:$junitJupiterEngineVersion"
}


shadowJar {
  classifier = 'fat'
  manifest {
    attributes 'Main-Verticle': mainVerticleName
  }
  mergeServiceFiles {
    include 'META-INF/services/io.vertx.core.spi.VerticleFactory'
  }
}

test {
  useJUnitPlatform()
  testLogging {
    events 'PASSED', 'FAILED', 'SKIPPED'
  }
}

run {
  args = ['run', mainVerticleName, "--redeploy=$watchForChange", "--launcher-class=$mainClassName", "--on-redeploy=$doOnChange"]
}
```

The main verticle is defined by `def mainVerticleName`. This tells Vert.x where to start. Also note that the dependency `vertx-web` is included. There are junit dependencies that will be discussed in a future post.

`src/main/java/com/example/hello_world_service/MainVerticle.java`
{% highlight java linenos %}
package com.example.hello_world_service;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    vertx.createHttpServer().requestHandler(req -> {
      req.response()
        .putHeader("content-type", "text/plain")
        .end("Hello from Vert.x!");
    }).listen(8888, http -> {
      if (http.succeeded()) {
        startPromise.complete();
        System.out.println("HTTP server started on port 8888");
      } else {
        startPromise.fail(http.cause());
      }
    });
  }
}
{% endhighlight %}

The Vert.x Starter generated this MainVerticle with a stubbed in HTTP server.

* Line 6 the class extends the `AbstractVerticle` to make it a Vert.x **verticle**.
* Line 9 the `start()` method must be overridden and is the entry point into this verticle.
* Line 10 creates an HTTP server on the Vert.x instance that deployed this verticle. It is a protected member of the AbstractVerticle base class.
* Line 13 handles the request with a lambda that returns `Hello from Vert.x!` in the response.
* Line 14 instructs the HTTP server to listen to port 8888.

A verticle is a class of execution that respond to events and is similar to an actor in the [Actor Model](https://en.wikipedia.org/wiki/Actor_model). A Vert.x application is composed of one or more verticles. When the service starts, Vert.x automatically deploys the MainVerticle which calls the `start()` in the verticle.

## Starting the Service

Start the service using the following command:

```zsh
hello-world-service % ./gradlew run
```

This command causes Gradle to compile and run the service in the foreground. Because the plugin is configured to redeploy, the plugin will restart the service when source code is changed.

## Testing the Service

Finally test the HTTP microservice by running the following curl command or navigate to `localhost:8888` with a web browser:

```zsh
% curl localhost:8888
Hello from Vert.x!
```

## Summary

This article describes a simple way to start creating HTTP microservices with the Vert.x library. 

## Source

[GitHub: hello-world-service](https://github.com/jamesdschmidt/blog-examples/tree/master/hello-world-service)

## References

[Vert.x Documentation](https://vertx.io/docs/)
