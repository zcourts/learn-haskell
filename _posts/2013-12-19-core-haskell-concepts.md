---
layout: post
title: "Core Haskell concepts"
description: "Introduces some of the core ideas you'll come accross in Haskell"
category: "beginner"
tags: [introduction,language,homepage]
---
{% include JB/setup %}

I suspect one of the reasons I had so much trouble learning Haskell was the amount of theory that is explained in the currently available resources. While interesting it is a lot to digest. Learning Haskell involves a massive paradigm shift. Throw that in with a huge amount of theory and new concepts you've probably never heard about just compounds the issue. Pretty much everything I can think of in Haskell has some deep rooted theory behind it. But it is not necessary to know or understand that theory to be practical in Haskell. And I think that's where some resources go wrong, they get so caught up in the fascinating theory that practical application of it is just glanced over. 


That's the more "scientific" approach I suppose. But I think programming in general is something that can be introduced the brute force way. Give an introduction, the absolute minimum that is necessary to get on with it and then the rest of the time is devoted to practicing. If it had to be a ratio I'd say it should be 1:9, i.e. spend 90% of the time practicing and the other 10% introducing the foundation needed to do the 90%... This section is about 8 of the 10%. It'll include a bunch of Haskell features, it need not all be understood right now but having an awareness of their existence will prove useful for the the things practiced later.

As the later examples are covered the features mentioned in this section will become more and more useful. Treat it as a reference for the examples to come. If an example isn't obvious or the explanation isn't good enough, flip back to this and try to make the connection.

# Functions

Perhaps not surprisingly functions are very important in Haskell, so being able to read, write and understand them is a fundamental skill you'll need in the Haskell world.


Haskell uses spaces to delimit tokens. In some other languages you might write a function definition like this (Java):

{% highlight java linenos %}
void f(int age, String name){
	System.out.println(name + " is "+age+" years old");
}

//use it like this
f(23,"Courtney");
//outputs
//Courtney is 23 years old
{% endhighlight %}

In Haskell you could write the same function like this:

{% highlight haskell linenos %}
-- line below is optional but typically included by convention to help readability
f :: Int -> String -> IO ()  
f age name = print (name ++ " is " ++ show age ++ " years old")
--us it like this
f 23 "Courtney"
--outputs
--Courtney is 23 years old
{% endhighlight %}

* Types can be though of as a way to group things (data or functions). For example the type "Num" represents operations that apply to numbers in general, the type Int represents counting numbers i.e. 1,2,3... etc. A type allows us to group things that are similar.

* Identifiers e.g. function or variable names can optionally specify a type (Identifiers ALWAYS have a type in Haskell but if you don't specify one the compiler can normally infer the type for you). The identifier name preceeds it's type and is separated by ::, i.e. double colon. In the string "a :: Int" the identifier a is an integer.                                               

* In a function's declaration, each type is separated by a right arrow i.e. "->", such that the declaration "f :: Int -> String -> IO ()" can be read as; The function f accepts two parameters (not strictly true but can be interpreted as such), an Int and a string and returns an IO Unit (i.e. IO ()). 

* () is effectively saying nothing is returned. Tuples are explained later where this syntax will show up again.

* Things are immutable in Haskell by default. So if you say " let a = 1". The value of 1 is always going to be the result of using the identifier a.

* You declare a function by providing a series of equations that are to match the expected inputs to the function.


* A function's parameters are separated by a whitespace.

* The first identifer in a function's declaration is the function's name, all valid identifers after are parameters which the function accepts.

* Parameters to a function are followed by an equal sign after which the value of each parameter will be bound to each identifier and can then be used.


## Higher order functions and & Lambdas

A higher order function is any function which accepts another function as it's parameter. In Haskell you tend to define what things are rather than the steps required to change the state of the world around you. Higher order functions make this a cinch.

{% highlight haskell linenos %}

-- usage with a lambda: higherOrderFn (\n -> even n) [1,2,3,4,5,6,7,8] returns [2,4,6,8]
-- with a normal function: higherOrderFn isEven [1,2,3,4,5,6,7,8] still returns [2,4,6,8]                           
higherOrderFn :: (Integer -> Bool) -> [Integer] -> [Integer]
higherOrderFn fn [] = []
higherOrderFn fn (x:xs) 
                | fn x = x : higherOrderFn fn xs
                | otherwise = higherOrderFn fn xs

isEven n = even n     
{% endhighlight %}

The function's type "higherOrderFn :: (Int -> Bool) -> [Int] -> [Int]" can be read as 'The function higherOrderFn accepts a function which accepts an integer and returns a boolean, higherOrderFn also accepts a list of integers and returns a list of integers'. Quite a mouth full, but read it again, it makes sense...
The bit that says "(Int -> Bool)" makes this a higher order function. That's the section that's saying a function can be passed in as a parameter.

Two ways to use this function is included in the code listing above as a comment.
The first way may be the more natural way by appearance where the function can be invoked with "higherOrderFn isEven [1,2,3,4,5,6,7,8]". This invokes the function passing isEven as the function to be used and a list of integers.

The second and perhaps more interesting syntax is to define is even in place using what's known as the lambda notation, "higherOrderFn (\n -> even n) [1,2,3,4,5,6,7,8]". The "(\n -> even n)" is a lambda function and is typically used when the entire function can be represented very succinctly.

## η-reduction

Is a way to simplify functions in Haskell. There's a whole thing behind it see this [Wikipedia](http://en.wikipedia.org/wiki/Combinatory_logic#.CE.B7-reduction) entry for background on the compinatorial problem/solution.

In short it says that if we have a function that takes a parameter x and applies a function E to x, then this is extensionally equal to the function E itself....blah blah blah. So what does that actually mean for our Haskell functions?

isEven deifned previously was done intentionally to demonstrate this (there's some method behind the madness). Take that definition of isEven. η-reduction simply means we can take this

{% highlight haskell linenos %}
isEven n = even n              
{% endhighlight %}

turn it into this


{% highlight haskell linenos %}
isEven = even             
{% endhighlight %}

because the function effectively just does what even does by extension...
That's all there is to it.


## Pattern matching and Partial functions

A partial function is one that is not defined for all possible arugments of a given type. The following is a partial function, where we define a match for when the age is 12 but all other cases are undefined. These undefined cases makes it a partial function, while this will compile and run, if an age other than 12 is passed in then we'll get an error.

{% highlight haskell linenos %}
partial :: Int -> String -> IO ()  
partial 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")
{% endhighlight %}

Testing the function with valid (12) and invalid age demonstrates what happens

{% highlight bash linenos %}

*CoreConcepts> :load CoreConcepts.hs
[1 of 1] Compiling CoreConcepts     ( CoreConcepts.hs, interpreted )
Ok, modules loaded: CoreConcepts.
*CoreConcepts> partial 12 "Courtney"
"Haha you're a child! Courtney you're 12 years old"
*CoreConcepts> partial 13 "Courtney"
*** Exception: CoreConcepts.hs:7:1-83: Non-exhaustive patterns in function partial
 /;./
{% endhighlight %}

When you specify a function it is important to ensure that all possible inputs are matched and where it is not possible to list all posibilities define the function the way "f" was defined or use a "catch-all". In order to make our function "exhaustive" in what it matches we could re-define it like this:

{% highlight haskell linenos %}
total :: Int -> String -> IO ()  
total 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")
total _ name = print (name ++ " maybe you're a child, maybe you're not...but we'll just say you are :)")
{% endhighlight %}

Now our function can handle any age and it won't blow up. The underscore i.e. _ acts as a sort of "catch-all" and just says we don't care what age is if it's not 12, this bit of our function should be executed.

{% highlight bash linenos %}

*CoreConcepts> :load CoreConcepts.hs
[1 of 1] Compiling CoreConcepts     ( CoreConcepts.hs, interpreted )
Ok, modules loaded: CoreConcepts.
*CoreConcepts> total 12 "Courtney"
"Haha you're a child! Courtney you're 12 years old"
*CoreConcepts> total 13 "Courtney"
"Courtney maybe you're a child, maybe you're not...but we'll just say you are :)"
*CoreConcepts> total 1 "Courtney"
"Courtney maybe you're a child, maybe you're not...but we'll just say you are :)"
*CoreConcepts>

{% endhighlight %}

What if we wanted to get access to the age? But didn't know or care what it was? Well, we did this earlier so we can re-define our function to be this:


{% highlight haskell linenos %}
total2 :: Int -> String -> IO ()  
total2 12 name = print ("Haha you're a child! " ++ name ++ " you're 12 years old")
total2 age name = print (name ++ " you're " ++ show age ++ " years old")
{% endhighlight %}

{% highlight haskell linenos %}
*CoreConcepts> total2 12 "Courtney"
"Haha you're a child! Courtney you're 12 years old"
*CoreConcepts> total2 13 "Courtney"
"Courtney you're 13 years old"
*CoreConcepts> total2 23 "Courtney"
"Courtney you're 23 years old"
*CoreConcepts>
{% endhighlight %}

This section started out describing partial functions but in fact we've been doing at least two things here. Creating partial and total functions is one but the other thing we've been doing is known as pattern matching. So when we define the function called "partial" we said "when age is 12 execute this block of code". Then we defined "total" we were saying "when age is 12 perform this action but if age is anything else do this action".

By setting specific values such as 12 we're matching the input parameter which would cause the block of code after the equal sign to be executed.

## Gaurds

Guards are a way for us to check if a condition is true or false. Similar to the pattern matching we did previously. We can rewrite our total function to include some guards like this:

{% highlight haskell linenos %}
gaurdedTotal :: Int -> String -> IO ()  
gaurdedTotal age name 
           | age <= 12 = print ("Haha you're " ++ show age ++ " you're a child, " ++ name)
           | age <= 35 = print ("You're " ++ show age ++ ", supposedly this is the best time of your life !")
           | age <= 55 = print ("Ohhh, " ++ name ++ " you're getting on a bit there at" ++ show age ++ " aren't you!")
           | otherwise = print ("To be honest " ++ name ++ " at "++ show age ++ " it's cruel to take a jab at you :P!")
{% endhighlight %}

See how similar it looks? The "otherwise" is equivalent to the underscore we used earlier to catch any case we didn't explicitly specify. In fact otherwise is pretty much just using "True" as the condition which will always succeed.

__Notice the order in which gaurds and patterns are given. The more specific patterns and gaurds are given first.__ 

Here's the output of trying it:

{% highlight bash %}
*CoreConcepts> :load CoreConcepts.hs
[1 of 1] Compiling CoreConcepts     ( CoreConcepts.hs, interpreted )
Ok, modules loaded: CoreConcepts.
*CoreConcepts> gaurdedTotal 12 "Courtney"
"Haha you're 12 you're a child, Courtney"
*CoreConcepts> gaurdedTotal 35 "Courtney"
"You're 35, supposedly this is the best time of your life !"
*CoreConcepts> gaurdedTotal 50 "Courtney"
"Ohhh, Courtney you're getting on a bit there at50 aren't you!"
*CoreConcepts> gaurdedTotal 98 "Courtney"
"To be honest Courtney at 98 it's cruel to take a jab at you :P!"
*CoreConcepts>
{% endhighlight%}

## Where expressions

Patterns and gaurds define the conditions on which a block of code should be executed. But once we're in that block, what if we need to use a computed value that is shared accross all the function's code blocks? For example in the function

{% highlight haskell linenos %}
sum :: Int -> Int -> IO ()  
sum a b 
  | a + b == 2 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 2" )
  | a + b == 10 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 10 " )
  | otherwise = print "You're not very good at this math thing are you?" 
{% endhighlight %}

We're repeating a+b and if added more patterns we'd repeat it even more. This reptition can be replaced. Actually, it's not the repitition so much as putting the logic for the calculation in one place that matters. Imagine we made a mistake in one instance where we repeated a computation? To help this we can use the where keyword as in:

{% highlight haskell linenos %}
sumWhere :: Int -> Int -> IO ()  
sumWhere a b 
  | combined == 2 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 2" )
  | combined == 10 = print ("Yup " ++ show a ++ "+" ++ show b ++ " = 10 " )
  | otherwise = print "You're not very good at this math thing are you?"  
   where combined = a + b
{% endhighlight %}

{% highlight bash %}
*CoreConcepts> :load CoreConcepts.hs
[1 of 1] Compiling CoreConcepts     ( CoreConcepts.hs, interpreted )
Ok, modules loaded: CoreConcepts.
*CoreConcepts> mySum 1 1
"Yup 1+1 = 2"
*CoreConcepts> mySum 1 9
"Yup 1+9 = 10 "
*CoreConcepts> mySum 1 10
"You're not very good at this math thing are you?"
*CoreConcepts> sumWhere  1 10
"You're not very good at this math thing are you?"
*CoreConcepts> sumWhere  1 9
"Yup 1+9 = 10 "
*CoreConcepts> sumWhere  1 1
"Yup 1+1 = 2"
*CoreConcepts>
{% endhighlight %}

How awesome is that? This means you should never have a valid excuse for duplicating the code to perform the same computation.

## Let expressions

Let expressions do a similar thing to where. It allows you to bind an expression to an identifier. The major difference however is that a let expression is local to the code block in which it is bound. i.e. they are no accessible from multiple gaurds or patterns. You create them like so:

{% highlight haskell linenos %}
sumLet :: Int -> Int -> IO ()  
sumLet a b =
        let total = a + b
        in  print ("The total is " ++ show total)
{% endhighlight %}

Notice the other difference? With let expressions the definitions come before the usage whereas the where keyword allows definitions to be used before they are defined in the source. Let expressions also go with the "in" key word. Multiple identifers can be bound between the "let" and "in" keywords. Those bindings are then available for use in the expressions that follow the "in" keyword. As in:

{% highlight haskell linenos %}
sumLet2 :: Int -> Int -> IO ()  
sumLet2 a b =
        let total = a + b
            times = a * b
        in  print ("The total is " ++ show total ++ " and times each other the value is " ++ show times)        
{% endhighlight %}

{% highlight bash %}
*CoreConcepts> :load CoreConcepts.hs
[1 of 1] Compiling CoreConcepts     ( CoreConcepts.hs, interpreted )
Ok, modules loaded: CoreConcepts.
*CoreConcepts> sumLet2 1 1
"The total is 2 and times each other the value is 1"
*CoreConcepts> sumLet2 2 2
"The total is 4 and times each other the value is 4"
*CoreConcepts> sumLet2 2 3
"The total is 5 and times each other the value is 6"

{% endhighlight %}

## Case expressions

Case expressions are effectively the same as pattern matching. Take a look:

{% highlight haskell linenos %}
caseExpr :: [Int] -> IO ()
caseExpr xs = case xs of [] -> print "Yeah, we don't like empty lists"
                         x:xss -> print x
{% endhighlight %}

In fact pattern matching is just a slightly better looking way of doing case expressions...

This function also introduces something else that we'll get to when we cover data structures later. But for now just go with [] being a list...it is, really.

So in our "caseExpr" function we've used case identifier of pattern1 -> code pattern2 -> code2

## Function currying

## $ makes the world go round

## Composition

# Functors

## Maps

## Folds
# Repeating yourself, recursion is Okay

# Types

## Type alias || Type synonyms

Sometimes we want something to be identifiable by a specific name but there's already a type which is a good if not perfect representation of what we want. Haskell allows us to create  an alias to a type such that, this alias is treated as it's own independent type but using the deifnition of another. For example:

{% highlight haskell linenos %}
  type Name = String

  me :: Name -> String
  me a = "My name is " ++ a
{% endhighlight %}

This defines a function "me" that accepts a name and returns a string. Useful if say we wanted to pass Name around and have it treated differently to Strings.

## Data types
Let's say you wanted to pass around a group of related data. Not just single values such as a single Int,String, etc... Imagine we wanted to represent some information about a person in Haskell. We could begin by identifying what we want to repreent.
Let's say, age, height and foodLevel (so we can tell if a person is hungry).

You could do this using Haskell's data syntax (also called type construction in various places). As in:

{% highlight haskell linenos %}
	data Person1 = Person1 Int Float Float
{% endhighlight %}

The first thing might notice is we have "Person1" twice.
The first occurance i.e. on the left of the equal sign is known as the "type constructor". The second occurence on the right of the equal sign is called a "data constructor".

A type constructor defines the name under which this data collection is known. The data constructor as the name may imply specifies what is usedto "construct" a Person1. The type and data constructor can have different names, but by covnention the same name is used some times. For e.g. the same collection can be represented as

{% highlight haskell linenos %}
	data Person1 = Adult Int Float Float
{% endhighlight %}

To use these you could create a Person1 by either of the following

{% highlight haskell linenos %}
 let p1 = Person1 1 1.1 0.5
 -- or using the Adult data constructor
 let p2 = Adult 1 1.1 0.5
{% endhighlight %}

Both of the above represents exactly the same data.
One thing that might come to mind is, how do you know which float is the age and which is the foodLevel? And in reality it's only by convention that you can know which is which. This is a limitation of the syntax. 

### Data types with the Record syntax

However, do not despair, Haskell provides another syntax which allows you to label your data properties. Using that syntax the same structure could be represented as:

{% highlight haskell linenos %}
	data Person = Person {age::Int, height::Float, foodLevel::Float}
{% endhighlight %}

You would use it the same way, except now you can be explicit about what is what:

{% highlight haskell linenos %}
  let p1 = Person{age = 1, height = 1.1, foodLevel = 0.5}
{% endhighlight %}

One other important disinction is that the compiler automatically generates a function with the same name as your labels. These functions will extract the value of the label with the same name. For example, if we wanted to get age we could do so with the function called age as in:

{% highlight haskell linenos %}
  let p1 = Person{age = 1, height = 1.1, foodLevel = 0.5}
  --in some function that accept a person
  f p = age p
{% endhighlight %}

Our function f above, accepts a person and returns the person's age using the function age that was automatically generated. This is known as the "record" syntax and in many if not all cases it's the better choice.

### Algebraic data types

There are many cases in the everyday world where we have some abstract collections that often have more specific types. Take humans for example, we can be classified throughout our life cycle as infants, children, teenagers, adults or elderly, for e.g. and there are many cases where this type of classifcation occurrs. To represent this, Haskell has what it calls abstract data types. It's pretty much what we've been doing with the data keyword so far but going a bit further. e.g.:

{% highlight haskell linenos %}
data Person = Infant{age::Int, height::Float, foodLevel::Float, diaperCount::Int}
			  | Child {}
			  | Teenager {}
			  | Adult {}
			  | Elderly {}
{% endhighlight %}


### Data structures

### Recursive data structures

## Type classes

A type class is a mechanism for grouping function definitions. These definitions together define the behaviour of this type class' __instances__ (instances are covered next). A type class can also be thought of as an interface or to some extent an abstract class from an OO language such as Java. Don't get too caught up in the comparison though, they are not the same things. A type class simply means that a definition provides a group of function definitions without their bodies (hence the comparison to Java interfaces). However, you can optionally provide default implementations of the functions (hence why I say it's like abstract classes).

For a type to belong to a type class an implementation of all of the functions must exist for that type.

Enough talk, more code:

{% highlight haskell linenos %}
class (Eq a) => Animal a where  
    grow :: a -> a  
    eat :: a -> Float -> a
    sleep :: a -> Float -> a
    --default implementation of eat in terms of how much you sleep
    eat = sleep
{% endhighlight %}

Here we use the "class" keyword to create an Animal type class with three functions namely, grow, eat and sleep. By providing a definition of eat, any instance of this typeclass can omit providing their own implementation.

## Instances and Type parameters

Type classes are somewhat useless without instances. I mean, what is the point of an interface that has no concrete implementation? An instance is a specific implementation of a typeclass for a given type.

Give our previous implementation of Person. We;ve got this data but what if we wanted to give it some behaviour?
Our Animal typeclass just so happens to have some behaviour that applies to a person. So Haskell allows us to associate a set of behaviours with a type, in this case Person. We can do this by creating an instance of Animal for the Person type.

{% highlight haskell linenos %}
instance Animal Person where
    grow p = p{age = age p + 1}
    
    eat p a = p{height =  newHeight p a}
        where h = height p
              newHeight p amount = h + (1 / (amount * h))
              
    sleep p t = p{height = newHeight (h p t) t}
        where h p t= height p * t / 2
              newHeight height time = height + (1 / (time * height))   
{% endhighlight %}

This creates an instance of Animal for the type Person. 
This is where we need to take a step back, we've been using this "a" in the definition of the Animal type class without any explanation of what it is. Well, it's a type parameter. A type parameter is a way of saying the type that goes here doesn't matter as long as it meets whatever constraints I put in place.

When we defined Animal we said "class (Eq a) => Animal a where". There are two important things happening in that line.
We use a type parameter a, this allows us to define a polymorhpic type class. This means that we the type "a" can be replaced with another type. That's where instances come into the picture.
The second thing happening in that line is where we use "(Eq a) =>", this allows us to specify what's called a class constraint. This says to the Haskell compiler, "whatever the type of a is, it needs to be a member of the Eq type class". In a way this is inheritance, the definition of Eq immediately becomes applicable to Animal.

## Modules

A module is a group of related functions, types and type classes. You create a module by creating a file with the same name as the module and using the module where syntax as in:

{% highlight haskell linenos %}
module CoreConcepts where
{% endhighlight %}
This would be placed in a file called CoreConcepts.hs - Simples!

# Monads

