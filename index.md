---
layout: page
title: Learning Haskell
tagline: Progressive tutorials aimed at teaching Haskell
category: [Haskell,  beginner]
tags: [beginner, hello-world]
---
{% include JB/setup %}

How it started?
===============

This repo is created to maintain some docs as I learn haskell. 
I read [Learn you a Haskell](http://learnyouahaskell.com/chapters) and [Real world Haskell](http://book.realworldhaskell.org/read/). Both great books that I would recommend.
Actually technically I skimmed both books. The point is by the end of it all I could read and understand most Haskell source code I came across but when I was ready to write some custom stuff I came up blank... didn't know where or how to start.

So this repo is a simple site that includes a bunch of progressive exercises I've done which helped.

Unfortunately I didn't do this while I was stuck, the repo came after I could write some OK looking haskell so it may not be quite accurate in portraying exactly what I did, but it's as much as I remember.

Since I continue to learn new things about Haskell, I'll try to continue adding stuff.

See [haskell.zcourts.com](http://haskell.zcourts.com).

Why Haskell
===========

* The concepts and ideas you find in Haskell opens your mind to a raft of different things. 
* It performs well.
* It integrates amazingly with C and C++ (with some C wrapper or small modifications)
* It's interesting and sufficiently different to other languages to the point where it doesn't feel like a boring routine working with it.
* It chips away so much boiler plate code.
* It allows all sorts of expressiveness.
* Very modular, re-usable
* You can express otherwise impossible things(i.e. impossible in say Java)

So tell me, what is Haskell?
============================

Haskell is a statically typed,pure, expressive functional programming language.
In other words, you compile it to machine code. Every variable in Haskell has a type e.g Int, String etc...
It is pure, this means a couple of things. Applying a function multiple times with the same parameters will produce the same results. There are no side effects...usually anyway (there's one exception), in reality what it means is that if a function causes any side effects it is explicit.

Perhaps one of the most awesome things about Haskell is it's type inference. In other typed languages you have to say what a type of a variable is...in Haskell the compiler figures things out for you so you don't have to type it.

#Pages

<ul>
  {% assign pages_list = site.pages %}
  {% include JB/pages_list %}
</ul>

## Categories

<ul>
  {% assign categories_list = site.categories %}
  {% include JB/categories_list %}
</ul>

## Posts
<div>
{% assign posts_collate = site.tags.homepage %}
{% include JB/posts_collate %}
</div>

## Tags

<ul>
  {% assign tags_list = site.tags %}
  {% include JB/tags_list %}
</ul>