> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char
> import Data.List
> import Control.Monad

## mapM
## forM
## sequence
## >=> 
## join
## msum
## mfilter
## filterM
## mapAndUnzipM
## zipWithM
## foldM
## replicateM
## guard
## unless
## liftM; liftM*
## ap

> main = putStrLn "Get to work!"

> wrap :: String -> IO () -> IO ()
> wrap name f = do
>   putStrLn ""
>   putStrLn $ name ++ "{"
>   f
>   putStrLn $ "}" ++ name
