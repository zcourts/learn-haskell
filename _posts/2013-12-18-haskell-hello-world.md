---
layout: post
title: "Haskell: Hello world"
description: "A simple Hello world example in Haskell"
categories: [Haskell,beginner]
tags: [haskell,beginner,hello-world]
---
{% include JB/setup %}

To begin with we will setup a project that we'll continue to use in all the examples.
In Haskell it is typical for a project to use Cabal to describe itself, dependencies and other useful properties. "Cabal describes what a Haskell package is, how these packages interact with the language, and what Haskell implementations must to do to support packages".

It can be thought of as being similar to ant, make, maven, groovy or Ruby's gem package tools. Don't assume it works the same way but in the bigger picture of things they are trying to acheive similar goals to some extent.

This assumes you've followed the ["Using an IDE or Text editor post"]({% post_url 2013-12-17-haskell-using-and-ide-or-text-editor %}) and have one of them ready to go. I'll be using Eclipse FP, if you use Sublime then just skip the "Create an Eclipse Haskell Project" section.

### Create a new cabal project

From the command line start a new project using:
{% highlight bash %}
mkdir learn-haskell
cd learn-haskell
cabal init
{% endhighlight %}

You will be propted with a series of questions about the project, just answer each or accept the defaults where provided. The interaction may look similar to:

<pre>
Courtney@ZCOURTS ~/Documents/projects/learn-haskell (master)
$ cabal init
Package name? [default: learn-haskell]
Package version? [default: 0.1.0.0] 0.0.1
Please choose a license:
 * 1) (none)
   2) GPL-2
   3) GPL-3
   4) LGPL-2.1
   5) LGPL-3
   6) BSD3
   7) MIT
   8) Apache-2.0
   9) PublicDomain
  10) AllRightsReserved
  11) Other (specify)
Your choice? [default: (none)] 6
Author name? Courtney Robinson
Maintainer email? learn-haskell@crlog.info
Project homepage URL? http://haskell.zcourts.com
Project synopsis? A series of notes and tutorials to help learn Haskell
Project category:
 * 1) (none)
   2) Codec
   3) Concurrency
   4) Control
   5) Data
   6) Database
   7) Development
   8) Distribution
   9) Game
  10) Graphics
  11) Language
  12) Math
  13) Network
  14) Sound
  15) System
  16) Testing
  17) Text
  18) Web
  19) Other (specify)
Your choice? [default: (none)] 19
Please specify? Language
What does the package build:
   1) Library
   2) Executable
Your choice? 2
Include documentation on what each field means (y/n)? [default: n] y

Guessing dependencies...

Generating LICENSE...
Generating Setup.hs...
Generating learn-haskell.cabal...

You may want to edit the .cabal file and add a Description field.

Courtney@ZCOURTS ~/Documents/projects/learn-haskell (master)
</pre>


Open learn-haskell.cabal and under executable, uncomment "main-is" so that is says

{% highlight yaml %}
  main-is: Main.hs
{% endhighlight %}

Below main-is add
{% highlight yaml %}
  hs-source-dirs:      src
{% endhighlight %}

### Create an Eclipse Haskell Project

In eclipse go to File -> New -> Haskell Project
Select the button that says "Create project in external location" and then browse to the folder you just ran cabal init in.
Make sure that "Executable" is selected in the "Components" section, then click finish.

What you've just done is created a Haskell project which uses Eclipse FP. The cabal init step was done so that the cabal project can be setup which means it would also be recognised and used in Sublime Haskell. 

### Haskell, Hello world

Create a file called Main.hs

{% highlight haskell %}
--this defines the module called Main, p.s. this is a comment
module Main where

main :: IO ()
main = print "Hello World"
{% endhighlight %}

### What just happened?

Haskell uses modules which are similar to namespaces or packages in other languages.
A module is used to contain definitions. These definitions can be "exported" or left "private" to the module. More on that later.

In the example above we created a module called "Main".
The definitions for the module then followed the "where" keyword.

Like other languages, most Haskell executable programs have a "main" method and we defined this as "main :: IO ()".
"main" being the name of the function we are defining and everything after the double colon (::) defines the signature of the main function. In this case

{% highlight haskell %}
main :: IO ()
{% endhighlight %}

We'll come back to signatures when we discuss types and type safety in Haskell.
For the record, everything in Haskell has a type. All types are known at compile time but you can often omit the type information and leave the compiler to "infer" it for you.

The same Hello world program could be written as

{% highlight haskell %}
module Main where

main = print "Hello World"
{% endhighlight %}

The type will be inferred. So Haskell is a static/strongly typed language. Omitting the type signature doesn't mean you can mix types i.e. you can't add a number and a string.

### Interpretting or Compiling a Haskell module

Haskell source code should typically be compiled. In some cases though it's convenient to be able to quickly run a program/moudle and test the functions you've written.

To interactively run a module without compiling start up ghci from the command line with:
<pre>

</pre>
<pre>
Courtney@ZCOURTS ~/Documents/projects/learn-haskell/src (master)
$ ghc --make Main.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main.exe ...

Courtney@ZCOURTS ~/Documents/projects/learn-haskell/src (master)
$ ls
Main.exe*  Main.hi  Main.hs  Main.o

Courtney@ZCOURTS ~/Documents/projects/learn-haskell/src (master)
$ ./Main.exe
"Hello World"
</pre>

