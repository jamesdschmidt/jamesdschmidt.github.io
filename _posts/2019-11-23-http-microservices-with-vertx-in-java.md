---
title: "HTTP Microservices With Vert.x in Java"
published: true
---

> Mathematics is the language of nature.
> Fibonacci

This article will describe how to create a simple HTTP microservice with Java and the Vert.x library. We'll look at other integration technologies in the future.

**Microservices** are an architectural style for an application where it is composed of independently deployable services. The most common pattern of integrating these services is over HTTP.

The **Vert.x library** was inspired by Node.js. The advantage of choosing Vert.x with Java is it is implemented with multiple threads and can therefore handle more requests or events per process.

## What You'll Need
* [JDK 11](https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot)
* [Maven](https://maven.apache.org/download.cgi)

## Getting Started

Start by creating a directory for the project:

```bash
mkdir hello-world-service
cd hello-world-service
```

Now create the project using Maven and the [vertx-maven-plugin](https://reactiverse.io/vertx-maven-plugin/):

```bash
mvn io.reactiverse:vertx-maven-plugin:1.0.22:setup \
  -DprojectGroupId=com.example \
  -DprojectArtifactId=hello-world-service \
  -Dverticle=com.example.HelloWorldService \
  -Ddependencies=web
```

TODO: describe what maven and the plugin did

### pom.xml
{% highlight xml linenos %}
<?xml version="1.0"?>
<project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" xmlns="http://maven.apache.org/POM/4.0.0"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>hello-world-service</artifactId>
  <version>1.0-SNAPSHOT</version>
  <properties>
    <maven.compiler.target>1.8</maven.compiler.target>
    <vertx-maven-plugin.version>1.0.22</vertx-maven-plugin.version>
    <vertx.verticle>com.example.HelloWorldService</vertx.verticle>
    <maven.compiler.source>1.8</maven.compiler.source>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <vertx.version>3.8.2</vertx.version>
  </properties>
  <dependencyManagement>
    <dependencies>
      <dependency>
        <groupId>io.vertx</groupId>
        <artifactId>vertx-stack-depchain</artifactId>
        <version>${vertx.version}</version>
        <type>pom</type>
        <scope>import</scope>
      </dependency>
    </dependencies>
  </dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>io.vertx</groupId>
      <artifactId>vertx-core</artifactId>
    </dependency>
    <dependency>
      <groupId>io.vertx</groupId>
      <artifactId>vertx-web</artifactId>
    </dependency>
  </dependencies>
  <build>
    <plugins>
      <plugin>
        <groupId>io.reactiverse</groupId>
        <artifactId>vertx-maven-plugin</artifactId>
        <version>${vertx-maven-plugin.version}</version>
        <executions>
          <execution>
            <id>vmp</id>
            <goals>
              <goal>initialize</goal>
              <goal>package</goal>
            </goals>
          </execution>
        </executions>
        <configuration>
          <redeploy>true</redeploy>
        </configuration>
      </plugin>
    </plugins>
  </build>
</project>
{% endhighlight %}

TODO: describe pom.xml

### src/main/java/com/example/HelloWorldService.java
{% highlight java linenos %}
package com.example;

import io.vertx.core.AbstractVerticle;

public class HelloWorldService extends AbstractVerticle {

    @Override
    public void start() {

    }

}
{% endhighlight %}

TODO: describe HelloWorldService.java

## Trying It Out

```bash
mvn vertx:run
```

TODO: describe the maven command

Let's add an HTTP server to the service so it can handle web requests. Update `HelloWorldService.java` to look like the following example:

{% highlight java linenos %}
package com.example;

import io.vertx.core.AbstractVerticle;

public class HelloWorldService extends AbstractVerticle {

  @Override
  public void start() {
    vertx.createHttpServer()
      .requestHandler(req -> req.response().end("hello world\n"))
      .listen(8080);
  }
}

{% endhighlight %}

TODO: describe HelloWorldService.java

Finally let's test the HTTP microservice by running the following curl command or navigate to localhost:8080 with a web browser:

```bash
curl localhost:8080
hello world

```

## An Easier Way

Visit the [Vert.x Starter](https://start.vertx.io/) for an easy way to create a new project with Vert.x.
