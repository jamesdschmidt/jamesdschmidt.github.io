---
title: "REST With Vert.x"
tags: microservices vertx java
published: false
---

- come up with good resource to CRUD (bikes, books, music, greetings by language, fortune cookies, movies, video games, blog posts, ecommerce/products, patients, photos, tweets, hiking trails, biking trails)
- the example should be a crowd sourced hiking trail database: this article will feature easy, medium, and difficult trails

trailwiki
resource = trails
id = brighton-lakes
GET /trails/:id
PUT /trails/:id
PATCH /trails/:id
POST /trails
DELETE /trails/:id

reviews, photos, videos

```json
{
  "id":"brigton-lakes-loop",
  "name":"Brighton Lakes Loop",
  "country":"United States",
  "state":"Utah",
  "region":"Uinta-Wasatch-Cache National Forest"
  "length":"7 miles",
  "elevationGain":"1807 feet",
  "type":"Loop",
  "difficulty":"hard",
  "rating":"4.5",
  "description":"A heavily trafficed loop with several lakes located near Salt Lake City, Utah..."
}
```

- how to use the router object
- how to wire up all the CRUD responses
- how to get path parameters
- how to build out a more complex project with controllers

This article will show how to get started creating RESTful microservices with the Vert.x toolkit. We start with an HTTP microservice. For help creating a microservice see [HTTP Microservices With Vert.x in Java](/2020/02/29/http-microservices-with-vertx-in-java.html).

- describe REST/RESTful

REST is an style of communication over HTTP. 

Open the project in your favorite IDE and edit the `MainVerticle` class as follows:

`src/main/java/com/example/json_hello_world_service/MainVerticle.java`
{% highlight java linenos %}
package com.example.json_hello_world_service;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    vertx.createHttpServer().requestHandler(req -> {

      req.response()
        .putHeader("content-type", "application/json")
        .end("{\"greeting\":\"hello, world\"}");

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

* Line 13 the content-type header is set to `application/json`
* Line 14 writes a raw JSON string

Start the service using the following command:

```zsh
% ./gradlew run
```

Test it with `curl` (or open `localhost:8888` in a web browser):

```zsh
% curl localhost:8888
{"greeting":"hello, world"}
```

To view the content-type header add the `-v` (verbose) flag:

```zsh
% curl -v localhost:8888
*   Trying ::1...
* TCP_NODELAY set
* Connected to localhost (::1) port 8888 (#0)
> GET / HTTP/1.1
> Host: localhost:8888
> User-Agent: curl/7.64.1
> Accept: */*
> 
< HTTP/1.1 200 OK
< content-type: application/json
< content-length: 27
< 
* Connection #0 to host localhost left intact
{"greeting":"hello, world"}* Closing connection 0
```

## JsonObject

Java doesn't have a native JSON object however the Vert.x library provides a JsonObject to serve JSON. Under the hood, JsonObject is a facade over a LinkedHashMap Java collection. It contains many helper methods for getting or setting values in a JSON structure. In addition Vert.x uses Jackson to parse and serialize to and from the JsonObject.

Modify the `MainVerticle` class to use a JsonObject:

`src/main/java/com/example/json_hello_world_service/MainVerticle.java`
{% highlight java linenos %}
package com.example.json_hello_world_service;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    vertx.createHttpServer().requestHandler(req -> {

      var json = new JsonObject().put("greeting", "hello, world");

      req.response()
        .putHeader("content-type", "application/json")
        .end(json.encode());

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

* Line 13 a JsonObject was created. Notice the fluent programming interface of the object.
* Line 17 `encode()` is called on the JsonObject to produce a UTF-8 encoded string.

Notice the output doesn't change:

```zsh
% curl localhost:8888
{"greeting":"hello, world"}%
```

Admittedly the example is overly simple. It is to isolate the key pieces for serving JSON from a Vert.x HTTP microservice.

## Source

[GitHub: json-hello-world-service](https://github.com/jamesdschmidt/blog-examples/tree/master/json-hello-world-service)

## References

[Vert.x Documentation](https://vertx.io/docs/)
