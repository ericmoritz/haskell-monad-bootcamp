module BootCamp where

ioShow :: Show a => IO a -> IO ()
ioShow = (putStrLn . show =<<)

trace :: Show a => a -> IO ()
trace = print

assert :: Bool -> IO ()
assert True = return ()
assert False = error "liar"


wrap :: String -> IO () -> IO ()
wrap name f = do
  putStrLn ""
  putStrLn $ name ++ "{"
  f
  putStrLn $ "}" ++ name
