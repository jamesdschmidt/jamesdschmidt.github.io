---
title: "Use Yoda Expressions"
tags: java
published: true
---

I disliked this style the first time I saw a code base with yoda expressions but it has since grown on me.

## Boolean

```java
if (Boolean.TRUE.equals(parameter)) {
  return someValue;
}

if (Boolean.FALSE.equals(parameter)) {
  return anotherValue;
}
```

## String

```java
if ("a literal".equals(parameter)) {
  return someValue;
}
```
