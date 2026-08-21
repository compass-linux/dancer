module Dancer.Logging where

logStep :: String -> IO ()
logStep msg = putStrLn $ "====> " ++ msg

logSubStep :: String -> IO ()
logSubStep msg = putStrLn $ "----> " ++ msg

logProgress :: String -> IO ()
logProgress msg = putStrLn $ "..... " ++ msg

logOK :: String -> IO ()
logOK msg = putStrLn $ "[OK] " ++ msg

logFail :: String -> IO ()
logFail msg = putStrLn $ "[FAIL] " ++ msg

logWarn :: String -> IO ()
logWarn msg = putStrLn $ "! " ++ msg
