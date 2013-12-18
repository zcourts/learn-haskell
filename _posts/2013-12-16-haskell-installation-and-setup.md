---
layout: post
title: "Haskell: Installation and Setup"
description: "How to setup and install Haskell Platform"
category: "Installation"
tags: [setup,install,homepage]
---
{% include JB/setup %}

The first thing to do is install Haskell Platform on your development machine.

Get the installer from [http://www.haskell.org/platform/](http://www.haskell.org/platform/).

If you're on linux then use your package manager's distribution. Most of them are either up to date or not that far behind.

For e.g. on Ubuntu you would use


{% highlight bash %}
sudo apt-get install haskell-platform
{% endhighlight %}

If your platform doesn't have a pre-built package, download the source and unpack it to a directory, haskell_build. And execute the following to install it.

{% highlight bash %}
cd ./haskell_build
./configure # --prefix # can be added to change the default installation path
make #-j 8 # if you have a multi-core machine with 4 cores, 8 virtual or adjust accordingly
sudo make install
{% endhighlight %}

Once installed. Check that you have the following programs availailable.

{% highlight bash %}
ghci
{% endhighlight %}

Should output something similar to:

{% highlight bash %}
$ ghci
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :q
Leaving GHCi
{% endhighlight %}
__Type :q and press enter to quit GHCi.__

and 
{% highlight bash %}
cabal -V #uppercase V
{% endhighlight %}

should output something similar to:

{% highlight bash %}
$ cabal -V
cabal-install version 1.16.0.2
using version 1.16.0 of the Cabal library
{% endhighlight %}
