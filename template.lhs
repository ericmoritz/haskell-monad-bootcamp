> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char
> import Data.List
> import Control.Monad

## Test

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
>   x <- mapMDemoWithSideEffects
>   return ()

So even though I didn't use the value, the putStrLn in ioStringPutStr was
fired off because I used "<-".  "<-" pulls a value out of the IO monad which executes the actions

However if I don't use "<-", the actions are not executed because no value is pulled out of the IO monad.

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
>   x <- forMDemoUpper
>   putStrLn $ show x

sequence takes an a [IO m] and turns it into a IO [m]

> main5 :: IO ()
> main5 = do
>   x <- sequence [ioString "a", ioString "b", ioString "c"]
>   putStrLn $ show x

The >=> function composes two (a -> m b) functions together.  It is an
analog to . functions that return Monads i.e. (a -> m b)

> ioUpper :: String -> IO String
> ioUpper = ioString . map toUpper
> 
> ioAddSpaces :: String -> IO String
> ioAddSpaces = ioString . intersperse ' '

> ioUpperAndSpace :: String -> IO String
> ioUpperAndSpace= ioUpper >=> ioAddSpaces

> main6 = do
>   x <- ioUpperAndSpace  "test"
>   putStrLn x

Next up is join.  join will take an IO (IO String) and turn it into an IO String

> nestedIO :: String -> IO (IO String)
> nestedIO s = do
>   putStrLn "outer"
>   return $ do
>     putStrLn "test"
>     ioString s

> main7 :: IO ()
> main7 = do
>   x <- join $ nestedIO "value"
>   putStrLn x

That executes both the inner and outer IO actions, The act of joining
the nested IO monads causes the outer IO action to be executed:

> main8 :: IO ()
> main8 = do
>   join $ nestedIO "value"
>   return ()

Skipping msum and mfilter because IO is not an instance of MonadPlus


> main = do
>   wrap "main1" main1
>   wrap "main2" main2
>   wrap "main3" main3
>   wrap "main4" main4
>   wrap "main5" main5
>   wrap "main6" main6
>   wrap "main7" main7
>   wrap "main8" main8

> wrap :: String -> IO () -> IO ()
> wrap name f = do
>   putStrLn ""
>   putStrLn $ name ++ "{"
>   f
>   putStrLn $ "}" ++ name
