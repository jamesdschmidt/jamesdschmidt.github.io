---
title: "Use Yoda Conditions"
tags: java
---

I disliked this style the first time I saw a code base with yoda conditions due to the cluttered look of the expressions. It has since grown on me. Getting past opinion there is a technical problem and solution here. The problem is when working with object types in Java there is a possibility of running into ```null``` and the dredded ```NullPointerException```. To avoid the exception adopt the yoda conditions style using literals or constants on the left side of an expression.

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
