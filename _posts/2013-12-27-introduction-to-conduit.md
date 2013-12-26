---
layout: post
title: "Summary of the Haskell Conduit package"
description: "Introduces the conduits IO library"
category: "IO"
tags: [IO,conduit,homepage]
---
{% include JB/setup %}

## Conduits

As part of my [PhD](http://research.zcourts.com) I'm developing a distributed graph database in _Haskell_, [Conduits](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview) is used heavily for IO (Network and File) and where ever else it's appropriate. 
So here's a quick summary of what it is, it's main concepts and primitives/operations. This is largely a re-hash of [Michael Snoyman](https://www.fpcomplete.com/user/snoyberg) post but only the most important details are included. I'll do more posts later with concrete code examples, this one is all about the concepts.

### What is it?
Conduit is a library which provides a solution for streaming data. It uses a pipeline like mechanism to allow the production, transformation and consumption of streams in constant memory. There are modules for [File IO](http://hackage.haskell.org/package/filesystem-conduit), [Network IO](http://hackage.haskell.org/package/network-conduit) and parsing structured data with [attoparsec](http://hackage.haskell.org/package/attoparsec).

### 3 Main components

1. __`Source`__ Produces a stream of data values and sends them _downstream_.

2. __`Sink`__ Consumes a stream of data values from _upstream_ and produces a return value.

3. __`Conduit`__ Consumers a stream from _upstream_ and produces a new stream of the same type and send it _downstream_. In other words, a conduit is a way of modifying a stream on demand, before it is consumed.

### 1 Connect operator

1. __`$$`__ is the __connect__ operator which connects a `Source` to a `Sink`. It feeds the values from the `Source` into the `Sink` to produce a final result.

### 3 Fusion operators

Fusion is the combination of two components to form a new one.

1. __`=$`__ takes __two components__ and generates a new one. For example it can fuse a `Conduit` and a `Sink` into a __new `Sink`__which consumes the same values as the original `Conduit` and produce the same result as the original `Sink`.

2. __`$=`__ combines a `Source` and a `Conduit` into a new `Source`.

3. __`=$=`__ combines __two `Conduit`s__ into a __new `Conduit`__

### 3 Operations

1. __`await`__ takes a single value from _upstream_, if available

2. __`yield`__ sends a single value _downstream_

3. __`leftover`__ puts a single value back in the upstream queue for it to be read by the next call to `await`.

# Life cycle

Imagine conduits as being a pipeline where by data starts at the source, possibly moves to one or more conduits and are consumed by a sink as in, `Source` => `Conduit` => `Sink`.

The pipeline is driven by the `Sink`. When the sink requests data with await, it pauses until input is available from _upstream_. The `Source` will be woken up and asked to produce more output for _downstream_, it effectively goes back to sleep after producing a value until the Sink requests another value. When the Sink completes, the entire pipeline terminates, causing resources to be freed.

# Misc / TODO

Conduit comes with a way to provide exception safety, this will be covered in another post. Similarly, conduit has the notion of a _resumable source_, this is a source that's been run partially but can be continued by reconnecting it to another sink.