A few minutes ago I found myself debugging a strange Java issue related
to Jackson, one of the most common Java JSON serialization libraries.

The gist of the issue was that a short wrapper using some types from
[Javaslang](http://www.javaslang.io/) was causing unexpected problems:

```java
public <T> Try<T> readValue(String json, TypeReference type) {
  return Try.of(() -> objectMapper.readValue(json, type));
}
```

The signature of this function was based on the original Jackson
`readValue` type signature:

```java
public <T> T readValue(String content, TypeReference valueTypeRef)
```

While happily using my wrapper function I suddenly got an unexpected
error telling me that `Object` is incompatible with the type I was
asking Jackson to de-serialize, which got me to re-evaluate the above
type signature again.

Lets look for a second at some code that will *happily compile* if you
are using Jackson\'s own `readValue`:

```java
// This shouldn't compile!
Long l = objectMapper.readValue("\"foo\"", new TypeReference<String>(){});
```

As you can see there we ask Jackson to decode the JSON into a `String`
as enclosed in the `TypeReference`, but assign the result to a `Long`.
And it compiles. And it failes at runtime with
`java.lang.ClassCastException: java.lang.String cannot be cast to java.lang.Long`.
Huh?

Looking at the Jackson `readValue` implementation it becomes clear
what\'s going on here:

```java
@SuppressWarnings({ "unchecked", "rawtypes" })
public <T> T readValue(String content, TypeReference valueTypeRef)
    throws IOException, JsonParseException, JsonMappingException
{
    return (T) _readMapAndClose(/* whatever */);
}
```

The function is parameterised over the type `T`, however the only place
where `T` occurs in the signature is in the parameter declaration and
the function return type. Java will happily let you use generic
functions and types without specifying type parameters:

```java
// Compiles fine!
final List myList = List.of(1,2,3);

// Type is now myList : List<Object>
```

Meaning that those parameters default to `Object`. Now in the code above
Jackson also explicitly casts the return value of its inner function
call to `T`.

What ends up happening is that Java infers the expected return type from
the context of the `readValue` and then happily uses the unchecked cast
to fit that return type. If the type hints of the context aren\'t strong
enough we simply get `Object` back.

So what\'s the fix for this? It\'s quite simple:

```java
public <T> T readValue(String content, TypeReference<T> valueTypeRef)
```

By also making the parameter appear in the `TypeReference` we \"bind\"
`T` to the type enclosed in the type reference. The cast can then also
safely be removed.

The cherries on top of this are:

1.  `@SuppressWarnings({ "rawtypes" })` explicitly disables a
    warning that would\'ve caught this

2.  the `readValue` implementation using the less powerful `Class`
    class to carry the type parameter does this correctly: `public <T>
    T readValue(String content, Class<T> valueType)`

The big question I have about this is *why* does Jackson do it this way?
Obviously the warning did not just appear there by chance, so somebody
must have thought about this?

If anyone knows what the reason is, I\'d be happy to hear from you.

PS: Shoutout to David & Lucia for helping me not lose my sanity over
this.
