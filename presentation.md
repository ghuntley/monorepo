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

## [fit] Hello ~~world~~ Joe is boring!
## [fit] Lets do it with processes.

---
### Hello Server

```erlang
-module(hello_server).
-export([start_server/0]).

start_server() ->
    spawn(fun() -> server() end).

server() ->
    receive
        {greet, Name} ->
            io:format("Hello ~s!~n", [Name]),
            server()
    end.
```

---

## [fit] Some issues with that ...

- What about unused messages?
- What if the server crashes?

---

## [fit] Part 2: Open Telecom Platform

### **It's called Erlang/OTP for a reason.**

---

# OTP: An Application Framework

- Supervision - keep processes alive!

- OTP Behaviours - common process patterns

- Extensive standard library

- Error handling, debuggers, testing, ...

- Lots more!

^ Standard library includes lots of things from simple network libraries over testing frameworks to cryptography, complete LDAP clients etc.

---

# Supervision

![inline](http://erlang.org/doc/design_principles/sup6.gif)

^ Supervision keeps processes alive, different restart behaviours, everything should be supervised to avoid "process" (and therefore memory) leaks

---

# OTP Behaviours

* `gen_server`
* `gen_statem` 
* `gen_event`
* `supervisor`

^ gen = generic. explain server, explain statem, event = event handling with registered handlers, supervisor ...

---

`gen_server`

---

## [fit] Part 3: BEAM

### Bogdan/Bjørn Erlang Abstract machine

---

## A VM for Erlang

* Many were written, BEAM survived
* Concurrent garbage-collection
* Lower-level bytecode than JVM
* Very open to new languages
  (Elixir, LFE, Joxa, ...)

---

## What next?

* Ole's talk, obviously!
* Learn You Some Erlang!
  www.learnyousomeerlang.com
* Watch *Erlang the Movie*
* (soon!) Join the Oslo BEAM meetup group

---

# [fit] Questions?

`@tazjin`
