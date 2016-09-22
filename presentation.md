slidenumbers: true
Erlang.
======

### Fault-tolerant, concurrent programming.

---

## A brief history of Erlang

---

![](https://www.ericsson.com/thinkingahead/the-networked-society-blog/wp-content/uploads/2014/09/bfW5FSr.jpg)


^ Telefontornet in Stockholm, around 1890. Used until 1913. 

---

![](https://3.bp.blogspot.com/-UF7W9yTUO2g/VBqw-1HNTzI/AAAAAAAAPeg/KvsMbNSAcII/s1600/6835942484_1531372d8f_b.jpg)

^ Telephones were operated manually at Switchboards. Anyone old enough to remember? I'm certainly not. 

---

![fit](https://russcam.github.io/fsharp-akka-talk/images/ericsson-301-AXD.png)

^ Eventually we did that in software, and we got better at it over time. Ericsson AXD 301, first commercial Erlang switch. But lets take a step back.

---

## Phone switches must be ...

Highly concurrent

Fault-tolerant

Distributed

(Fast!)

![right 150%](http://learnyousomeerlang.com/static/img/erlang-the-movie.png)

---

## ... and so is Erlang!

---

## Erlang as a whole:

- Unique process model (actors!)
- Built-in fault-tolerance & error handling
- Distributed processes
- Three parts!

---

## Part 1: Erlang, the language

- Functional
- Prolog-inspired syntax
- Everything is immutable
- *Extreme* pattern-matching

---
### Hello Joe

```erlang
hello_joe.
```

---
### Hello Joe

```erlang
-module(hello1).
-export([hello_joe/0]).

hello_joe() ->
    hello_joe.
```

---
### Hello Joe

```erlang
-module(hello1).
-export([hello_joe/0]).

hello_joe() ->
    hello_joe.
    
% 1> c(hello1).
% {ok,hello1}
% 2> hello1:hello_joe().
% hello_joe
```

---
### Hello Joe

```erlang
-module(hello2).
-export([hello/1]).

hello(Name) ->
    io:format("Hello ~s!~n", [Name]).

% 3> c(hello2).
% {ok,hello2}
% 4> hello2:hello("Joe").
% Hello Joe!
% ok
```


---

## Drag & Drop images

### Simply *drop an image onto the Deckset window* and the Markdown you need to display the image is automatically created and *copied to the clipboard.*

---

* This works with both local files and web images
* You don’t _need_ to drag the file, you can also type the Markdown yourself if you know how

![left,filtered](http://deckset-assets.s3-website-us-east-1.amazonaws.com/colnago1.jpg)
