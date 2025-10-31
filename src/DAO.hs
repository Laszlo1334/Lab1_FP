module DAO where

import Models
import DB
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Maybe
import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS

-- Conversion functions: transform Haskell types into MySQL values
toMySQLText :: String -> MySQLValue
toMySQLText = MySQLText . T.pack

toMySQLMaybeText :: Maybe String -> MySQLValue
toMySQLMaybeText = maybe MySQLNull (MySQLText . T.pack)

toMySQLInt :: Int -> MySQLValue
toMySQLInt = MySQLInt32 . fromIntegral

toMySQLMaybeInt :: Maybe Int -> MySQLValue
toMySQLMaybeInt = maybe MySQLNull (MySQLInt32 . fromIntegral)

-- Extract string representation from MySQL values
fromMySQLValue :: MySQLValue -> String
fromMySQLValue (MySQLText bs) = T.unpack bs
fromMySQLValue (MySQLInt32 i) = show i
fromMySQLValue (MySQLInt64 i) = show i
fromMySQLValue (MySQLDouble d) = show d
fromMySQLValue (MySQLDate day) = show day
fromMySQLValue (MySQLDateTime day) = show day
fromMySQLValue (MySQLTime time _) = show time
fromMySQLValue MySQLNull = ""
fromMySQLValue _ = ""

-- Helper: convert MySQL value to Maybe String, handling NULL
parseMaybeString :: MySQLValue -> Maybe String
parseMaybeString MySQLNull = Nothing
parseMaybeString v = case null (fromMySQLValue v) of
    True -> Nothing
    False -> Just (fromMySQLValue v)

-- Helper: extract Maybe Int from MySQL values
parseMaybeInt :: MySQLValue -> Maybe Int
parseMaybeInt MySQLNull = Nothing
parseMaybeInt (MySQLInt32 i) = Just (fromIntegral i)
parseMaybeInt (MySQLInt64 i) = Just (fromIntegral i)
parseMaybeInt v = case reads (fromMySQLValue v) of
    [(i, "")] -> Just i
    _ -> Nothing

-- | Author-related database operations
-- Functions for managing author records in the database

listAuthors :: MySQLConn -> IO [Author]
listAuthors conn = do
    let sql = Query (LBS.pack "SELECT id, name, email, department FROM authors ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseAuthor rows
  where
    parseAuthor [authorId, authorName, authorEmail, authorDept] = Author
        (read $ fromMySQLValue authorId :: Int)
        (fromMySQLValue authorName)
        (parseMaybeString authorEmail)
        (parseMaybeString authorDept)

getAuthorById :: MySQLConn -> Int -> IO (Maybe Author)
getAuthorById conn authorId = do
    let sql = Query (LBS.pack "SELECT id, name, email, department FROM authors WHERE id = ?")
    (_, is) <- query conn sql [toMySQLInt authorId]
    rows <- Streams.toList is
    case null rows of
        True -> return Nothing
        False -> return . Just $ parseAuthor (head rows)
  where
    parseAuthor [authorId, authorName, authorEmail, authorDept] = Author
        (read $ fromMySQLValue authorId :: Int)
        (fromMySQLValue authorName)
        (parseMaybeString authorEmail)
        (parseMaybeString authorDept)

createAuthor :: MySQLConn -> String -> Maybe String -> Maybe String -> IO Int
createAuthor conn name email dept = do
    let sql = Query (LBS.pack "INSERT INTO authors (name, email, department) VALUES (?, ?, ?)")
        params = [toMySQLText name, toMySQLMaybeText email, toMySQLMaybeText dept]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

updateAuthor :: MySQLConn -> Int -> String -> Maybe String -> Maybe String -> IO Bool
updateAuthor conn authorId name email dept = do
    let sql = Query (LBS.pack "UPDATE authors SET name = ?, email = ?, department = ? WHERE id = ?")
        params = [toMySQLText name, toMySQLMaybeText email, toMySQLMaybeText dept, toMySQLInt authorId]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return True

deleteAuthor :: MySQLConn -> Int -> IO Bool
deleteAuthor conn authorId = do
    let sql = Query (LBS.pack "DELETE FROM authors WHERE id = ?")
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt [toMySQLInt authorId])
    return True

-- | Service type operations
-- Managing service type definitions

listServiceTypes :: MySQLConn -> IO [ServiceType]
listServiceTypes conn = do
    let sql = Query (LBS.pack "SELECT id, name, description FROM service_types ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseServiceType rows
  where
    parseServiceType [typeId, typeName, typeDesc] = ServiceType
        (read $ fromMySQLValue typeId :: Int)
        (fromMySQLValue typeName)
        (parseMaybeString typeDesc)

createServiceType :: MySQLConn -> String -> Maybe String -> IO Int
createServiceType conn name desc = do
    let sql = Query (LBS.pack "INSERT INTO service_types (name, description) VALUES (?, ?)")
        params = [toMySQLText name, toMySQLMaybeText desc]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

-- | Service management operations
-- Core functions for working with services

listServices :: MySQLConn -> IO [Service]
listServices conn = do
    let sql = Query (LBS.pack "SELECT id, name, author_id, annotation, service_type_id FROM services ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseService rows
  where
    parseService [serviceId, serviceName, authorId, annotation, serviceTypeId] = Service
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue serviceName)
        (read $ fromMySQLValue authorId :: Int)
        (parseMaybeString annotation)
        (read $ fromMySQLValue serviceTypeId :: Int)

getServiceById :: MySQLConn -> Int -> IO (Maybe Service)
getServiceById conn serviceId = do
    let sql = Query (LBS.pack "SELECT id, name, author_id, annotation, service_type_id FROM services WHERE id = ?")
    (_, is) <- query conn sql [toMySQLInt serviceId]
    rows <- Streams.toList is
    case null rows of
        True -> return Nothing
        False -> return . Just $ parseService (head rows)
  where
    parseService [serviceId, serviceName, authorId, annotation, serviceTypeId] = Service
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue serviceName)
        (read $ fromMySQLValue authorId :: Int)
        (parseMaybeString annotation)
        (read $ fromMySQLValue serviceTypeId :: Int)

createService :: MySQLConn -> String -> Int -> Maybe String -> Int -> IO Int
createService conn name authorId annotation typeId = do
    let sql = Query (LBS.pack "INSERT INTO services (name, author_id, annotation, service_type_id) VALUES (?, ?, ?, ?)")
        params = [toMySQLText name, toMySQLInt authorId, toMySQLMaybeText annotation, toMySQLInt typeId]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

updateService :: MySQLConn -> Int -> String -> Int -> Maybe String -> Int -> IO Bool
updateService conn serviceId name authorId annotation typeId = do
    let sql = Query (LBS.pack "UPDATE services SET name = ?, author_id = ?, annotation = ?, service_type_id = ? WHERE id = ?")
        params = [toMySQLText name, toMySQLInt authorId, toMySQLMaybeText annotation, toMySQLInt typeId, toMySQLInt serviceId]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return True

deleteService :: MySQLConn -> Int -> IO Bool
deleteService conn serviceId = do
    let sql = Query (LBS.pack "DELETE FROM services WHERE id = ?")
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt [toMySQLInt serviceId])
    return True

-- | System user management
-- Operations for handling system user accounts

listSystemUsers :: MySQLConn -> IO [SystemUser]
listSystemUsers conn = do
    let sql = Query (LBS.pack "SELECT id, username, full_name, email, role FROM system_users ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseSystemUser rows
  where
    parseSystemUser [userId, username, fullName, userEmail, userRole] = SystemUser
        (read $ fromMySQLValue userId :: Int)
        (fromMySQLValue username)
        (parseMaybeString fullName)
        (parseMaybeString userEmail)
        (parseMaybeString userRole)

getSystemUserById :: MySQLConn -> Int -> IO (Maybe SystemUser)
getSystemUserById conn userId = do
    let sql = Query (LBS.pack "SELECT id, username, full_name, email, role FROM system_users WHERE id = ?")
    (_, is) <- query conn sql [toMySQLInt userId]
    rows <- Streams.toList is
    case null rows of
        True -> return Nothing
        False -> return . Just $ parseSystemUser (head rows)
  where
    parseSystemUser [userId, username, fullName, userEmail, userRole] = SystemUser
        (read $ fromMySQLValue userId :: Int)
        (fromMySQLValue username)
        (parseMaybeString fullName)
        (parseMaybeString userEmail)
        (parseMaybeString userRole)

createSystemUser :: MySQLConn -> String -> Maybe String -> Maybe String -> Maybe String -> IO Int
createSystemUser conn username fullName email role = do
    let sql = Query (LBS.pack "INSERT INTO system_users (username, full_name, email, role) VALUES (?, ?, ?, ?)")
        params = [toMySQLText username, toMySQLMaybeText fullName, toMySQLMaybeText email, toMySQLMaybeText role]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

updateSystemUser :: MySQLConn -> Int -> String -> Maybe String -> Maybe String -> Maybe String -> IO Bool
updateSystemUser conn userId username fullName email role = do
    let sql = Query (LBS.pack "UPDATE system_users SET username = ?, full_name = ?, email = ?, role = ? WHERE id = ?")
        params = [toMySQLText username, toMySQLMaybeText fullName, toMySQLMaybeText email, toMySQLMaybeText role, toMySQLInt userId]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return True

deleteSystemUser :: MySQLConn -> Int -> IO Bool
deleteSystemUser conn userId = do
    let sql = Query (LBS.pack "DELETE FROM system_users WHERE id = ?")
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt [toMySQLInt userId])
    return True

-- | User registration operations
-- Functions for managing service registrations

listUserRegistrations :: MySQLConn -> IO [UserRegistration]
listUserRegistrations conn = do
    let sql = Query (LBS.pack "SELECT id, user_id, service_id, registration_date, status FROM user_registrations ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseUserRegistration rows
  where
    parseUserRegistration [regId, userId, serviceId, regDate, regStatus] = UserRegistration
        (read $ fromMySQLValue regId :: Int)
        (read $ fromMySQLValue userId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (parseMaybeString regDate)
        (parseMaybeString regStatus)

listRegistrationsByUser :: MySQLConn -> Int -> IO [UserRegistration]
listRegistrationsByUser conn userId = do
    let sql = Query (LBS.pack "SELECT id, user_id, service_id, registration_date, status FROM user_registrations WHERE user_id = ?")
    (_, is) <- query conn sql [toMySQLInt userId]
    rows <- Streams.toList is
    return $ map parseUserRegistration rows
  where
    parseUserRegistration [regId, userId, serviceId, regDate, regStatus] = UserRegistration
        (read $ fromMySQLValue regId :: Int)
        (read $ fromMySQLValue userId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (parseMaybeString regDate)
        (parseMaybeString regStatus)

registerUserToService :: MySQLConn -> Int -> Int -> IO Int
registerUserToService conn userId serviceId = do
    let sql = Query (LBS.pack "INSERT INTO user_registrations (user_id, service_id, status) VALUES (?, ?, 'active')")
        params = [toMySQLInt userId, toMySQLInt serviceId]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

-- | Usage statistics tracking
-- Functions for recording and retrieving usage data

listUsageStatistics :: MySQLConn -> IO [UsageStatistics]
listUsageStatistics conn = do
    let sql = Query (LBS.pack "SELECT id, service_id, user_id, access_date, duration_minutes, action_type FROM usage_statistics ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseUsageStatistics rows
  where
    parseUsageStatistics [statId, serviceId, userId, accessDate, duration, actionType] = UsageStatistics
        (read $ fromMySQLValue statId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (parseMaybeInt userId)
        (parseMaybeString accessDate)
        (parseMaybeInt duration)
        (parseMaybeString actionType)

getServiceStatistics :: MySQLConn -> Int -> IO [UsageStatistics]
getServiceStatistics conn serviceId = do
    let sql = Query (LBS.pack "SELECT id, service_id, user_id, access_date, duration_minutes, action_type FROM usage_statistics WHERE service_id = ?")
    (_, is) <- query conn sql [toMySQLInt serviceId]
    rows <- Streams.toList is
    return $ map parseUsageStatistics rows
  where
    parseUsageStatistics [statId, serviceId, userId, accessDate, duration, actionType] = UsageStatistics
        (read $ fromMySQLValue statId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (parseMaybeInt userId)
        (parseMaybeString accessDate)
        (parseMaybeInt duration)
        (parseMaybeString actionType)

recordUsage :: MySQLConn -> Int -> Maybe Int -> Maybe Int -> Maybe String -> IO Int
recordUsage conn serviceId userId duration action = do
    let sql = Query (LBS.pack "INSERT INTO usage_statistics (service_id, user_id, duration_minutes, action_type) VALUES (?, ?, ?, ?)")
        params = [toMySQLInt serviceId, toMySQLMaybeInt userId, toMySQLMaybeInt duration, toMySQLMaybeText action]
    bracket (prepareStmt conn sql)
            (closeStmt conn)
            (\stmt -> executeStmt conn stmt params)
    return 1

-- | Version management
-- Handling service version information

listVersions :: MySQLConn -> IO [Version]
listVersions conn = do
    let sql = Query (LBS.pack "SELECT id, service_id, version_number, release_date, is_current FROM versions ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseVersion rows
  where
    parseVersion [versionId, serviceId, versionNumber, releaseDate, isCurrent] = Version
        (read $ fromMySQLValue versionId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue versionNumber)
        (parseMaybeString releaseDate)
        (let val = fromMySQLValue isCurrent in val == "1" || val == "True" || val == "true")

-- | Terms and conditions management
-- Operations for handling service terms

listTermsConditions :: MySQLConn -> IO [TermsConditions]
listTermsConditions conn = do
    let sql = Query (LBS.pack "SELECT id, service_id, rules, usage_conditions, deadline FROM terms_conditions ORDER BY id")
    (_, is) <- query_ conn sql
    rows <- Streams.toList is
    return $ map parseTerms rows
  where
    parseTerms [termsId, serviceId, rules, conditions, deadline] = TermsConditions
        (read $ fromMySQLValue termsId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue rules)
        (parseMaybeString conditions)
        (parseMaybeString deadline)
