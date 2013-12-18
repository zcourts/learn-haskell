---
layout: post
title: "Haskell: Using and IDE or Text editor"
description: ""
category: Installation
tags: [setup,ide,install,homepage]
---
{% include JB/setup %}

Haskell IDE support is somewhat lacking but there are some projects attempting to fill the void. The two I'd recommend are [Sublime Haskell](https://github.com/SublimeHaskell/SublimeHaskell) and [Eclipse FP](http://eclipsefp.github.io/).

I won't go into any real detail on installing these two. Follow the links above and look at the documentation both projects provide on how to install them. Below is just a quick section taken from each project.

## Sublime Haskell

This is a plugin for the SublimeText editor. If you're a fan then it's a great addition because you can continue working in a familiar environment. Even if you're not Sublime Text is a great text editor so it may be worth a try.

Requirements
------------

Necessary:
* ghc and a recent Haskell Platform (>= 2012 should do fine)
* cabal
* Cabal packages: base, bytestring, aeson, haskell-src-exts, haddock 

e.g. cabal install aeson haskell-src-exts haddock

Optional, but useful:
* [ghc-mod](http://hackage.haskell.org/package/ghc-mod) (for import and LANGUAGE completions and type inference, `cabal install ghc-mod`)
* [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) (for code prettification, `cabal install stylish-haskell`)
* [cabal-dev](http://hackage.haskell.org/package/cabal-dev) if you want to use it
* [haskell-docs](http://hackage.haskell.org/package/haskell-docs) (for documentation in 'Symbol info' command, `cabal install haskell-docs`)
* [hdevtools](https://github.com/bitc/hdevtools) (or [fork for windows](https://github.com/mvoidex/hdevtools)) (for type inference, `cabal install hdevtools`)

Installation
------------
1. Get Sublime Text 2: <http://www.sublimetext.com/>
2. Install the Sublime Package Control package: <http://wbond.net/sublime_packages/package_control/installation>
3. Use Package Control to install this package (SublimeHaskell)

## Eclipse FP 

This is a plugin for the Eclipse IDE. Unlike Sublime, eclipse is far more than a text editor and as such tends to be more feature rich. This is my favorite of the two (even though I'm not a big fan of Eclipse for Java, Eclipse FP for Haskell has proven to be a great tool).

### Installation

* You'll need of course a Java Runtime Environment (JRE), version 6 or above. If you need to download it, go to this page.

* Go to the Eclipse download page and get any of the Eclipse bundles. With each of them you will get a different initial set of language support. I recommend going through some Eclipse tutorials and learning about other plug-ins such as EGit and Mylyn.

* Uncompress the archive you just downloaded. A eclipse folder will be created.

* Inside this folder, you will find an executable called eclipse. Double-click it to start the Eclipse environment (yes, "installing Eclipse" means uncompressing it somewhere, even on a portable drive).

* The first time you execute Eclipse, you will be asked about a workspace. A workspace is just the folder where your preferences and configurations are stored (you can have different sets of preferences using different workspaces), and where your projects will be created by default. For trying, you can just use the default choice (usually +your user folder/workspace+).

* Now, let's install EclipseFP. First, on the menu, go to Help > Install New Software....

* In the Available software window that will appear, click the Add... button.

* You will be asked about the details of the update site you want to add. An update site is just a place on the internet where your Eclipse installation can find new plug-ins to install. If you use Linux, the concept is very similar to a repository. The name is not important, but the URL must point to http://eclipsefp.sf.net/updates. 

* The Available software window will show the plug-ins in the EclipseFP repository. Check FP: Haskell support for Eclipse and click Next.

* After clicking Next a few more times (for accepting the licenses), the plug-in will be downloaded and installed. 

* You will be asked for an Eclipse restart. After doing it, you can start using EclipseFP by going to Window > Open perspective > Other... and selecting Haskell.