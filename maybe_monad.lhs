> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char
> import Data.List
> import Control.Monad
> import BootCamp

The Maybe Monad has two different values Just a and Nothing.

This monad is used when normally a null or None value is used in other
languages.  It is used when you have a series of steps where each step
may return Nothing and you want the bail out and return Nothing.

An example could be looking up a value in the database using a
query parameter than may or may not be there:

> database = [(1, "Eric"), (2, "Jon")]
> 
> query_by_qs :: [(String, Int)] -> Maybe String
> query_by_qs qs = do
>   id <- lookup "id" qs
>   lookup id database

> maybeExample :: IO ()
> maybeExample = do
>   trace $ query_by_qs [("id", 1)]
>   trace $ query_by_qs []          
>   trace $ query_by_qs [("id", 3)]


The lookup of id in the qs string returns a Maybe and the lookup into
the "database" returns a Maybe.  If either of those lookups returns a
Nothing; the query_by_qs function returns a Nothing.

You can easily see how the Maybe monad can simplify code.  No more "if x is None and y is None...".

we can also eliminate the do keyword and make query_by_qs simplier

> query_by_qs2 :: [(String, Int)] -> Maybe String
> query_by_qs2 qs = lookup "id" qs >>= flip lookup database


If lookup succeeds, >>= will feed the value of "id" into the flipped
lookup function.  I had to flip lookup because normally the list is in
the 2nd position and I needed a function that took the key in the 2nd
position.


> maybeExample2 :: IO ()
> maybeExample2 = do
>   -- Just "Eric"
>   trace $ query_by_qs2 [("id", 1)]
> 
>   -- Nothing because the param is missing
>   trace $ query_by_qs2 []          
>
>   -- Nothing because 3 is not in the "database"
>   trace $ query_by_qs2 [("id", 3)]


mapM/forM
-------------------------------------------------------------------

mapM is an interesting function when used with the Maybe monad.

All values have to map to (Just a) for the result return Just [a]:

> mapMExample = do
>   trace $ mapM (flip lookup database) [1, 2, 3]
>   trace $ mapM (flip lookup database) [1, 2]

While this is interesting; I fail to think of a good use case for
this. 


sequence
-------------------------------------------------------------------

This will return Nothing if all the values are Nothing:

> sequenceExample :: IO ()
> sequenceExample = do
>   trace $ sequence [Just 1, Nothing, Just 2]
>   trace $ sequence [Just 1, Just 2] 


 >=> 
-------------------------------------------------------------------

Lets use compose a (a -> Maybe a) and an (b -> Maybe c) into an
(a -> Maybe c) function:

This allows us to build point free versions of these types of functions.

For instance a point free version of query_by_qs2:

> query_by_qs3 = flip lookup database <=< lookup "id"

 join
-------------------------------------------------------------------

> joinExample :: IO ()
> joinExample = do
>   trace $ join (Just (Just 1))
>   trace $ join (Just (Nothing :: Maybe Int))


msum
-------------------------------------------------------------------

This is an interesting function with Maybe; it returns the first
Just value:

> msumExample :: IO ()
> msumExample = do
>   trace $ msum [Nothing, Just 1, Just 2]
>   -- We have to use a cast here because just using Nothing
>   -- doesn't tell the complier what kind of Maybe is in the list.
>   trace $ (msum [Nothing, Nothing, Nothing] :: Maybe Int)
>   trace $ msum [Nothing, Nothing, Just 1]

mfilter
-------------------------------------------------------------------

mfilter is used to turn a Maybe into Nothing if a predicate fails:

> mfilterExample :: IO ()
> mfilterExample = do
>   trace $ mfilter odd (Just 1)
>   trace $ mfilter odd (Just 2)

filterM, mapAndUnzipM, zipWithM, foldM, replicateM
-------------------------------------------------------------------

These are self explanitory

guard
-------------------------------------------------------------------

guard is useful when you want Nothing to come be returned when an
expression returns False:

> onlyEric :: [(String, Int)] -> Maybe String
> onlyEric qs = do
>   person <- query_by_qs3 qs
>   guard (person == "Eric")
>   return person

> guardExample :: IO ()
> guardExample = do
>   trace $ onlyEric [("id", 1)]
>   trace $ onlyEric [("id", 2)]

when/unless
-------------------------------------------------------------------

Not sure if I can use when and unless with Maybe.  It doesn't seem
to make any sense.


liftM; liftMn
-------------------------------------------------------------------

liftMn for Maybe is pretty straight forward.  Again, these are useful
in lifting pure functions into the Maybe monad.

Where evaluating the lifted function, if any of the parameters are
Nothing, the result is Nothing.

> liftExample = do
>   trace $ liftM (+1) Nothing
>   trace $ liftM (+1) (Just 1)
>   trace $ liftM2 (+) (Just 1) (Just 1)
>   trace $ liftM2 (+) (Just 1) Nothing

> main = do
>   wrap "maybeExample" maybeExample
>   wrap "maybeExample2" maybeExample2
>   wrap "mapMExample" mapMExample
>   wrap "sequenceExample" sequenceExample
>   wrap "joinExample" joinExample
>   wrap "mfilterExample" mfilterExample
>   wrap "guardExample" guardExample
>   wrap "liftExample" liftExample
