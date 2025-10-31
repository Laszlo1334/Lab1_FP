module DB where

import Database.MySQL.Base
import qualified Data.ByteString.Char8 as BS

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo {
    ciHost = "localhost",
    ciPort = 3306,
    ciUser = BS.pack "haskell",
    ciPassword = BS.pack "haskell",
    ciDatabase = BS.pack "faculty_services"
}

connectDB :: IO MySQLConn
connectDB = connect dbConfig
