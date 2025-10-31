module DB where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Control.Exception
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

-- Database connection configuration
dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo {
    ciHost = "localhost",
    ciPort = 3306,
    ciUser = BS.pack "haskell",
    ciPassword = BS.pack "haskell",
    ciDatabase = BS.pack "faculty_services"
}

-- Connect to database
connectDB :: IO MySQLConn
connectDB = connect dbConfig

-- Execute a query and return rows
queryDB :: MySQLConn -> String -> [String] -> IO [[MySQLValue]]
queryDB conn sql params = do
    let sqlQuery = Query (LBS.pack sql)
    if null params
        then do
            (defs, is) <- query_ conn sqlQuery
            result <- Streams.toList is
            return result
        else do
            (defs, is) <- query conn sqlQuery (map toMySQLString params)
            result <- Streams.toList is
            return result

-- Execute an insert/update/delete query
executeDB :: MySQLConn -> String -> [String] -> IO Integer
executeDB conn sql params = do
    let sqlQuery = Query (LBS.pack sql)
    if null params
        then do
            query_ conn sqlQuery
            return 1  -- Assume success
        else do
            query conn sqlQuery (map toMySQLString params)
            return 1  -- Assume success

-- Helper function to convert String to MySQLValue
toMySQLString :: String -> MySQLValue
toMySQLString s = MySQLText (T.pack s)

-- Helper function to convert MySQLValue to String
fromMySQLValue :: MySQLValue -> String
fromMySQLValue (MySQLText bs) = T.unpack bs
fromMySQLValue (MySQLInt32 i) = show i
fromMySQLValue (MySQLInt64 i) = show i
fromMySQLValue (MySQLNull) = ""
fromMySQLValue (MySQLDouble d) = show d
fromMySQLValue (MySQLDate day) = show day
fromMySQLValue (MySQLDateTime day) = show day
fromMySQLValue (MySQLTime time _) = show time
fromMySQLValue _ = ""

-- Helper function to safely get string from Maybe MySQLValue
maybeString :: Maybe MySQLValue -> Maybe String
maybeString Nothing = Nothing
maybeString (Just v) = Just (fromMySQLValue v)

