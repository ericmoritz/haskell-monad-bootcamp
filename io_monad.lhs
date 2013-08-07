> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char
> import Control.Monad

This is my hope to become a Monad ninja.  Starting with elementary manipulation up to use of Monad transformers.

For each monad type I will go through the each function in Control.Monad to see how it can be applied to the Monad.

Starting with the IO monad.


Here the hello function produces an IO action 

> hello :: IO ()
> hello = putStrLn "Hello, World"

Main needs to return an IO (). so if we make main1 (called in the actual main function)
The "Hello, World" is outputted with putStrLn

> main1 = hello

This function returns a string in an IO Monad.  This is a common return type
of reading files for instance.

> ioString :: String -> IO String
> ioString line = return line

mapM will let you take a (a -> m a) function and a list of values and return
an  (m [a]) result

> mapMDemo :: IO [String]
> mapMDemo = mapM ioString ["a", "b", "c"]

How lazy is this?  If I have an (a -> IO a) with a side effect, will
the side effect occur when doing mapM or will it be defered until I use
the result of mapM

> ioStringPutStr :: String -> IO String
> ioStringPutStr line = do
>   putStrLn line
>   ioString line

> mapMDemoWithSideEffects :: IO [String]
> mapMDemoWithSideEffects = mapM ioStringPutStr ["a", "b", "c"]

> main2 :: IO ()
> main2 = do
>   putStrLn "main2 {"
>   x <- mapMDemoWithSideEffects
>   putStrLn "} main2"

So even though I didn't use the value, the putStrLn in ioStringPutStr was
fired off because I used "<-".  "<-" pulls a value out of the IO monad.

> main3 :: IO ()
> main3 = do
>   putStrLn "main3 {"
>   mapMDemoWithSideEffects
>   putStrLn "} main3"

forM is the same as mapM but with the arguments flipped

> forMDemo :: IO [String]
> forMDemo = forM ["a", "b", "c"] ioString

I am not sure when this is useful but I would guess it is for
nicer looking compositions

> forMDemoUpper :: IO [String]
> forMDemoUpper = forM ["a", "b", "c"] $ ioString . map toUpper

> main4 :: IO ()
> main4 = do
>   putStrLn "main4 {"
>   x <- forMDemoUpper
>   putStrLn $ show x
>   putStrLn "} main4"

> main = do
>   main1
>   main2
>   main3
>   main4
