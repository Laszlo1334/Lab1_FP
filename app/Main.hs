module Main where

import CLI
import DB
import Control.Exception
import System.IO

main :: IO ()
main = do
    -- Set UTF-8 encoding for Windows console
    -- Note: Windows PowerShell may not display UTF-8 correctly
    -- Run: chcp 65001 before cabal run for better results
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetEncoding stdin utf8
    
    putStrLn "Connecting to database..."
    conn <- connectDB `catch` (\e -> do
        putStrLn $ "Error connecting to database: " ++ show (e :: SomeException)
        error "Database connection failed")
    putStrLn "Connected successfully!"
    mainMenu conn


