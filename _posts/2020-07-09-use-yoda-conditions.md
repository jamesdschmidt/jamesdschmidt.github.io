---
title: "Use Yoda Conditions"
tags: java
---

I disliked this style the first time I saw a code base with yoda conditions but it has since grown on me. The problem is when working with object types in Java there is a possibility of running into ```null``` and the dredded ```NullPointerException```. To avoid the exception adopt the yoda conditions style using literals or known constants that are a part of the object on the left side of an expression.

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

## Reference
[Yoda conditions - Wikipedia](https://en.wikipedia.org/wiki/Yoda_conditions)
