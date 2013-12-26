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

# Custom MinGW on Windows

I had issues with the C compiler that Haskell Platform ships with on Windows.
I tried to install the "network" module but it wouldn't work because it needed to build native dependencies and it was having problems with the C compiler.

Firstly, I installed my own version of MinGW to C:/MinGw
Haskell Platform is installed in "C:\Program Files (x86)\Haskell Platform\2013.2.0.0"
In that directory there's a folder called "mingw"...I renamed it to "mingw-old".
I edited the GHC settings file located at "C:\Program Files (x86)\Haskell Platform\2013.2.0.0\lib\settings"
It had:
{% highlight haskell %}
[("GCC extra via C opts", " -fwrapv"),
 ("C compiler command", "$topdir/../mingw/bin/gcc.exe"),
 ("C compiler flags", " -fno-stack-protector  -Wl,--hash-size=31 -Wl,--reduce-memory-overheads"),
 ("ar command", "$topdir/../mingw/bin/ar.exe"),
 ("ar flags", "q"),
 ("ar supports at file", "YES"),
 ("touch command", "$topdir/touchy.exe"),
 ("dllwrap command", "$topdir/../mingw/bin/dllwrap.exe"),
 ("windres command", "$topdir/../mingw/bin/windres.exe"),
 ("perl command", "$topdir/../perl/perl.exe"),
 ("target os", "OSMinGW32"),
 ("target arch", "ArchX86"),
 ("target word size", "4"),
 ("target has GNU nonexec stack", "False"),
 ("target has .ident directive", "True"),
 ("target has subsections via symbols", "False"),
 ("LLVM llc command", ""),
 ("LLVM opt command", "")
 ]
{% endhighlight %}

I updated it and replaced any mention of the Haskell's mingw and left only the executable's name. i.e. remove the path from "$topdir/../mingw/bin/gcc.exe" and turn it into "gcc.exe".

{% highlight haskell %}
[("GCC extra via C opts", " -fwrapv"),
 ("C compiler command", "gcc.exe"),
 ("C compiler flags", " -fno-stack-protector  -Wl,--hash-size=31 -Wl,--reduce-memory-overheads"),
 ("ar command", "ar.exe"),
 ("ar flags", "q"),
 ("ar supports at file", "YES"),
 ("touch command", "$topdir/touchy.exe"),
 ("dllwrap command", "dllwrap.exe"),
 ("windres command", "windres.exe"),
 ("perl command", "$topdir/../perl/perl.exe"),
 ("target os", "OSMinGW32"),
 ("target arch", "ArchX86"),
 ("target word size", "4"),
 ("target has GNU nonexec stack", "False"),
 ("target has .ident directive", "True"),
 ("target has subsections via symbols", "False"),
 ("LLVM llc command", ""),
 ("LLVM opt command", "")
 ]
 {% endhighlight %}

Finally in Windows edit your system Path variable and add the custom MinGW installation's bin directory e.g. "C:/MinGw/bin"

When I work on Ubuntu I never get this problem so presumably this is a Windows issue.