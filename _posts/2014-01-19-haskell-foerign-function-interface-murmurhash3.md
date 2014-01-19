---
layout: post
title: "DiSh (DIgest | HaSH) : Hash functions for Haskell"
description: "Introduces the dish hash function library"
categories: [FFI]
tags: [haskell,intermediate,hash,murmur3,"ansi C",C,homepage, "foreign function interface"]
---
{% include JB/setup %}

Continuing my work on my [PhD](http://research.zcourts.com), I found the need for a hash function. Of all the options available, I've found that [MurmurHash](https://code.google.com/p/smhasher/wiki/MurmurHash3) has been a good fit for my use case. Unfortunately there wasn't yet a Haskell wrapper for the C++ implementation, so I decided to create one.

For the impatient the library is available on Hackage under [Dish](http://hackage.haskell.org/package/Dish), [MurmurHash3, API Documentation](http://hackage.haskell.org/package/Dish-0.0.0.4/docs/Data-Dish-Murmur3.html) is also available.

When interfacing with C/C++ Haskell provides the [Foreign function interface](http://www.haskell.org/haskellwiki/FFI_Introduction) i.e. FFI, the main focus of this post.
I've heard, but not tested myself, that interfacing with C is easier than with C++ for various reasons so you either write a C wrapper for a C++ lib or go it the "hard" way and interface with C++ (I think there are just some extra things that need to be done because the whole "object" paradigm in C++). I don't have the time so I looked around for a C implementation of MurmurHash3.

I found one by [Peter Scott](https://github.com/PeterScott/murmur3) and decided I'd wrap it for Haskell...

This implementation provides 3 variants of MurmurHash.

{% highlight C linenos %}
void MurmurHash3_x86_32 (const void *key, int len, uint32_t seed, void *out);

void MurmurHash3_x86_128(const void *key, int len, uint32_t seed, void *out);

void MurmurHash3_x64_128(const void *key, int len, uint32_t seed, void *out);
{% endhighlight %}

The first version is optimized for creating hashes on x86 architectures and gives you a 32 bit hash of your input.
The second, is still for x86 but produces a 128 bit hash.
Finally, the third is for x64 architectures and also produces a 128 bit hash. 

That said, _all_ of them work on any of the architectures (or at least should) but each has optimizations for a specific architecture.

The first thing to try and figure out when using Haskell's FFI is which C data type maps to which Haskell type. Typically and as far as I've found the answer is _none_, instead of using the normal Haskell types (Int, Char, etc), there are types in Haskell, specifically for interfacing with C, these all start, unsurprisingly with the letter C. So instead of Int, you'll have CInt or CChar and so on. The package [Foreign.C](http://hackage.haskell.org/package/base-4.6.0.1/docs/Foreign-C.html) has a more complete list.

It is recommended that you do any (un)marshalling in Haskell instead of C, for various reasons.

To use the FFI, you begin by adding a pragma at the top of your source file "{-# LANGUAGE ForeignFunctionInterface #-}" and/or in the cabal file.

Next proceed by creating a foreign import of the function or functions. In our case

{% highlight haskell %}
foreign import ccall "MurmurHash3_x86_32" c_x86_32
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()

foreign import ccall "MurmurHash3_x86_128" c_x86_128
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()

foreign import ccall "MurmurHash3_x64_128" c_x64_128
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()
{% endhighlight %}

The foreign import is largely two parts, the first part starts with the "foreign import ccall" keyword, then the __exact__ name of the C function, followed by the name you want to refer to the imported function by from Haskell,  i.e. "keywords 'C name' 'Haskell name'".

This is then followed by the type signature of the __C__ function i.e. the part after the double colons, ::.

In our case we're saying the C function takes a CString, CInt, CUInt, Ptr CUInt and returns nothing i.e. ()/void...
where CInt is as the name implies a C Int, C String may be a tad misleading, it is actually a pointer to an array of C characters, CUInt is an unsigned C Int and finally Ptr CUInt is just a pointer to an array of unsigned C Ints... I've found that figuring out which types map to C data types can be a hit and miss if you're not proefficient in C...which I'm not :) - but thankfully most are obvious the most important thing I've found is that the Haskell docs are indispensible (P.S. look out for any caveats documented and avoid if possible).


So now we have our Haskell functions that point to our C functions c_x86_32,c_x86_128 and c_x64_128 are all valid Haskell functions that you can now use from the IO monad...but do you want to? I mean, if we're wrapping a C library doesn't mean we have to present a C API right? So I went a bit further...

Haskell is perhaps the best suited language for the 128 bit hash values because it actually has a built in type that can handle 128 bits with no issues...

I wanted a single function that could be used to invoke any of the 3 C functions because most of the code needed to work with all 3 from Haskell is exactly the same with the only difference being which C function is called so I created a data type to represent the options, MVH i.e. Murmur Hash Version

{% highlight haskell %}
data MHV = X86_32 | X86_128 | X64_128
{% endhighlight %}

Our C function accepts a pointer to an array of unsigned ints. In haskell of course we'd use a list of ints importantly though, we're accepting native Haskell types, String and Int instead of CString or CInt. But we actually return a list of CUInts. Why? Because, the API we provide will want to do different things to the results, in one instance it might want to just return one of the values from the list, in another it might want to merge them...where's the sense in converting the CUInt to a Haskell Int then operating on that which may lead to another conversion?
So I cam up with the following function:

{% highlight haskell %}
murmur3Raw :: String -> Int -> MHV -> IO [CUInt]
murmur3Raw val seed ver = do
  val' <- withCAStringLen val $ \x -> return x
  let cstr = strFromCStr val'
  let strLength = strLFromCStr val'
  outPtr <- mallocArray arrSize
  doHash ver cstr strLength (fromIntegral seed) outPtr
  peekArray arrSize outPtr
  where arrSize = 4
        strFromCStr :: CStringLen -> CString
        strFromCStr = fst
        strLFromCStr :: CStringLen -> CInt
        strLFromCStr i = fromIntegral $ snd i
        --version value size seed out 
        doHash :: MHV -> CString -> CInt -> CUInt -> Ptr CUInt -> IO()
        doHash X86_32  v s se o = c_x86_32 v s se o
        doHash X86_128 v s se o = c_x86_128 v s se o
        doHash X64_128 v s se o = c_x64_128 v s se o
{% endhighlight %}

The array size is always 4, that's what our C function expects and uses. Each entry in the array is a 32 bit Int. So when the c_x86_32 function is used, only the first element of the array is used in the C function, for the 128 bit variants, all four array elements are set to a 32 bit value each... i.e. 4 * 32 = 128 bits

So now comes the fun part, we have a 128 bit value represented as 4 different 32 bit values, haskell's Integer type can represent these 4 values as one big value...and that'll be normal is Haskell. How do you make 4 into 1? A little bit twiddling of course!

You create an integer m and for each 32 bit int i in our list left shift m by the bit size of i and xor the result with i (thanks to [merijin](https://github.com/merijn) for the suggestion).
So what I mean is you have the 4 numbers (represented as bytes):

0000011 00000001 01010101 10101010

And your integer m, left shift m by sizeOf(0000011) so m is now 
0000011 
repeat until you end up with m being the Integer
0000011000000010101010110101010

Notice I left shift by the sizeOf(i) because i is not guaranteed to be 8. Of course it'd still work if you always left shift by 8 it'd just mean your final hash value would potentially be 0 padded where a value was missing. 

To represent all of this I wrote the function x128

{% highlight haskell %}
x128 :: String -> Int -> MHV -> IO Integer
x128 val seed ver= do 
  v <- hash ver 
  return $ twiddle 0 v 
  where hash :: MHV -> IO [CUInt]
        hash X86_128 = murmur3Raw val seed X86_128
        hash X64_128 = murmur3Raw val seed X64_128
        hash _       = return []
        twiddle :: Integer -> [CUInt] -> Integer
        twiddle i [] = i
        twiddle i (0:xs) = twiddle i xs -- don't shift when val is 0
        twiddle i (x:xs) = twiddle (B.shift i (B.bitSize x) `B.xor` fromIntegral x) xs
{% endhighlight %}

Once I had this, I just provided some convenience functions for getting hashes out.

{% highlight haskell %}
murmur3 :: String  -> Int  -> MHV    -> [Int] 
murmur3 v s ver = US.unsafePerformIO $ murmur3' v s ver

murmur3' :: String  -> Int  -> MHV -> IO [Int] 
murmur3' v s ver = do m <- murmur3Raw v s ver; toArr m
  where 
    toArr :: [CUInt] -> IO [Int]
    toArr [] = return []
    toArr l = return $ b l []     
              where b :: [CUInt] -> [Int] -> [Int]
                    b xs l2 = foldl (\ list x -> list ++ [w x] ) l2 xs
                    w :: CUInt -> Int
                    w = fromIntegral

murmur3Int' :: String -> Int -> IO Int 
murmur3Int' val seed = do v <- murmur3Raw val seed X86_32 
                         -- safe to use L.head, list is never empty even if all vals are 0
                          return $ fromIntegral (L.head v)

murmur3Int :: String -> Int  -> Int 
murmur3Int val seed = US.unsafePerformIO $ murmur3Int' val seed


murmur3IntegerX86' :: String  -> Int -> IO Integer 
murmur3IntegerX86' val seed = x128 val seed X86_128
   
murmur3IntegerX86 :: String -> Int -> Integer 
murmur3IntegerX86 val seed = US.unsafePerformIO $ murmur3IntegerX86' val seed

murmur3IntegerX64' :: String -> Int  -> IO Integer 
murmur3IntegerX64' val seed = x128 val seed X64_128

murmur3IntegerX64 :: String -> Int -> Integer
murmur3IntegerX64 val seed = US.unsafePerformIO $ murmur3IntegerX64' val seed

{% endhighlight %}

This was my first bit of dabbling with the Haskell FFI, and except for asking 2 questions one the #haskell IRC channel, it was largely painless.

For the vigilant Haskeller, you may have noticed my use of unsafePerformIO, in this case however I think this is justified. A hash function should always return the same value for the same input so if the compiler decided to do any optimization, inlining, caching etc, this would not only be OK but preferred. Why have the hash computed multiple times when you know the value is always the same? Plus this was one of my questions on the IRC channel, whether using unsafePerformIO was justified for this case and turns out that this si exactly the kind of thing it was created for so win! :)