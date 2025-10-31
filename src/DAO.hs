module DAO where

import Models
import DB
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Data.Maybe

-- Authors CRUD operations

listAuthors :: MySQLConn -> IO [Author]
listAuthors conn = do
    rows <- queryDB conn "SELECT id, name, email, department FROM authors ORDER BY id" []
    return $ map parseAuthor rows
  where
    parseAuthor [id, name, email, dept] = Author
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue name)
        (if fromMySQLValue email == "" then Nothing else Just $ fromMySQLValue email)
        (if fromMySQLValue dept == "" then Nothing else Just $ fromMySQLValue dept)

getAuthorById :: MySQLConn -> Int -> IO (Maybe Author)
getAuthorById conn id = do
    rows <- queryDB conn "SELECT id, name, email, department FROM authors WHERE id = ?" [show id]
    if null rows
        then return Nothing
        else return . Just $ parseAuthor (head rows)
  where
    parseAuthor [id, name, email, dept] = Author
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue name)
        (if fromMySQLValue email == "" then Nothing else Just $ fromMySQLValue email)
        (if fromMySQLValue dept == "" then Nothing else Just $ fromMySQLValue dept)

createAuthor :: MySQLConn -> String -> Maybe String -> Maybe String -> IO Int
createAuthor conn name email dept = do
    executeDB conn "INSERT INTO authors (name, email, department) VALUES (?, ?, ?)"
        [name, maybe "" (\x -> x) email, maybe "" (\x -> x) dept]
    return 0

updateAuthor :: MySQLConn -> Int -> String -> Maybe String -> Maybe String -> IO Bool
updateAuthor conn id name email dept = do
    rows <- executeDB conn "UPDATE authors SET name = ?, email = ?, department = ? WHERE id = ?"
        [name, maybe "" (\x -> x) email, maybe "" (\x -> x) dept, show id]
    return (rows > 0)

deleteAuthor :: MySQLConn -> Int -> IO Bool
deleteAuthor conn id = do
    rows <- executeDB conn "DELETE FROM authors WHERE id = ?" [show id]
    return (rows > 0)

-- ServiceTypes CRUD operations

listServiceTypes :: MySQLConn -> IO [ServiceType]
listServiceTypes conn = do
    rows <- queryDB conn "SELECT id, name, description FROM service_types ORDER BY id" []
    return $ map parseServiceType rows
  where
    parseServiceType [id, name, desc] = ServiceType
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue name)
        (if fromMySQLValue desc == "" then Nothing else Just $ fromMySQLValue desc)

createServiceType :: MySQLConn -> String -> Maybe String -> IO Int
createServiceType conn name desc = do
    executeDB conn "INSERT INTO service_types (name, description) VALUES (?, ?)"
        [name, maybe "" (\x -> x) desc]
    return 0

-- Services CRUD operations

listServices :: MySQLConn -> IO [Service]
listServices conn = do
    rows <- queryDB conn "SELECT id, name, author_id, annotation, service_type_id FROM services ORDER BY id" []
    return $ map parseService rows
  where
    parseService [id, name, authorId, annotation, typeId] = Service
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue name)
        (read $ fromMySQLValue authorId :: Int)
        (if fromMySQLValue annotation == "" then Nothing else Just $ fromMySQLValue annotation)
        (read $ fromMySQLValue typeId :: Int)

getServiceById :: MySQLConn -> Int -> IO (Maybe Service)
getServiceById conn id = do
    rows <- queryDB conn "SELECT id, name, author_id, annotation, service_type_id FROM services WHERE id = ?" [show id]
    if null rows
        then return Nothing
        else return . Just $ parseService (head rows)
  where
    parseService [id, name, authorId, annotation, typeId] = Service
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue name)
        (read $ fromMySQLValue authorId :: Int)
        (if fromMySQLValue annotation == "" then Nothing else Just $ fromMySQLValue annotation)
        (read $ fromMySQLValue typeId :: Int)

createService :: MySQLConn -> String -> Int -> Maybe String -> Int -> IO Int
createService conn name authorId annotation typeId = do
    executeDB conn "INSERT INTO services (name, author_id, annotation, service_type_id) VALUES (?, ?, ?, ?)"
        [name, show authorId, maybe "" (\x -> x) annotation, show typeId]
    return 0

updateService :: MySQLConn -> Int -> String -> Int -> Maybe String -> Int -> IO Bool
updateService conn id name authorId annotation typeId = do
    rows <- executeDB conn "UPDATE services SET name = ?, author_id = ?, annotation = ?, service_type_id = ? WHERE id = ?"
        [name, show authorId, maybe "" (\x -> x) annotation, show typeId, show id]
    return (rows > 0)

deleteService :: MySQLConn -> Int -> IO Bool
deleteService conn id = do
    rows <- executeDB conn "DELETE FROM services WHERE id = ?" [show id]
    return (rows > 0)

-- SystemUsers CRUD operations

listSystemUsers :: MySQLConn -> IO [SystemUser]
listSystemUsers conn = do
    rows <- queryDB conn "SELECT id, username, full_name, email, role FROM system_users ORDER BY id" []
    return $ map parseSystemUser rows
  where
    parseSystemUser [id, username, fullName, email, role] = SystemUser
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue username)
        (if fromMySQLValue fullName == "" then Nothing else Just $ fromMySQLValue fullName)
        (if fromMySQLValue email == "" then Nothing else Just $ fromMySQLValue email)
        (if fromMySQLValue role == "" then Nothing else Just $ fromMySQLValue role)

getSystemUserById :: MySQLConn -> Int -> IO (Maybe SystemUser)
getSystemUserById conn id = do
    rows <- queryDB conn "SELECT id, username, full_name, email, role FROM system_users WHERE id = ?" [show id]
    if null rows
        then return Nothing
        else return . Just $ parseSystemUser (head rows)
  where
    parseSystemUser [id, username, fullName, email, role] = SystemUser
        (read $ fromMySQLValue id :: Int)
        (fromMySQLValue username)
        (if fromMySQLValue fullName == "" then Nothing else Just $ fromMySQLValue fullName)
        (if fromMySQLValue email == "" then Nothing else Just $ fromMySQLValue email)
        (if fromMySQLValue role == "" then Nothing else Just $ fromMySQLValue role)

createSystemUser :: MySQLConn -> String -> Maybe String -> Maybe String -> Maybe String -> IO Int
createSystemUser conn username fullName email role = do
    executeDB conn "INSERT INTO system_users (username, full_name, email, role) VALUES (?, ?, ?, ?)"
        [username, maybe "" (\x -> x) fullName, maybe "" (\x -> x) email, maybe "" (\x -> x) role]
    return 0

updateSystemUser :: MySQLConn -> Int -> String -> Maybe String -> Maybe String -> Maybe String -> IO Bool
updateSystemUser conn id username fullName email role = do
    rows <- executeDB conn "UPDATE system_users SET username = ?, full_name = ?, email = ?, role = ? WHERE id = ?"
        [username, maybe "" (\x -> x) fullName, maybe "" (\x -> x) email, maybe "" (\x -> x) role, show id]
    return (rows > 0)

deleteSystemUser :: MySQLConn -> Int -> IO Bool
deleteSystemUser conn id = do
    rows <- executeDB conn "DELETE FROM system_users WHERE id = ?" [show id]
    return (rows > 0)

-- UserRegistrations operations

listUserRegistrations :: MySQLConn -> IO [UserRegistration]
listUserRegistrations conn = do
    rows <- queryDB conn "SELECT id, user_id, service_id, registration_date, status FROM user_registrations ORDER BY id" []
    return $ map parseUserRegistration rows
  where
    parseUserRegistration [id, userId, serviceId, date, status] = UserRegistration
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue userId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (if fromMySQLValue date == "" then Nothing else Just $ fromMySQLValue date)
        (if fromMySQLValue status == "" then Nothing else Just $ fromMySQLValue status)

listRegistrationsByUser :: MySQLConn -> Int -> IO [UserRegistration]
listRegistrationsByUser conn userId = do
    rows <- queryDB conn "SELECT id, user_id, service_id, registration_date, status FROM user_registrations WHERE user_id = ?" [show userId]
    return $ map parseUserRegistration rows
  where
    parseUserRegistration [id, userId, serviceId, date, status] = UserRegistration
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue userId :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (if fromMySQLValue date == "" then Nothing else Just $ fromMySQLValue date)
        (if fromMySQLValue status == "" then Nothing else Just $ fromMySQLValue status)

registerUserToService :: MySQLConn -> Int -> Int -> IO Int
registerUserToService conn userId serviceId = do
    executeDB conn "INSERT INTO user_registrations (user_id, service_id, status) VALUES (?, ?, 'active')"
        [show userId, show serviceId]
    return 0

-- UsageStatistics operations

listUsageStatistics :: MySQLConn -> IO [UsageStatistics]
listUsageStatistics conn = do
    rows <- queryDB conn "SELECT id, service_id, user_id, access_date, duration_minutes, action_type FROM usage_statistics ORDER BY id" []
    return $ map parseUsageStatistics rows
  where
    parseUsageStatistics [id, serviceId, userId, date, duration, action] = UsageStatistics
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (if fromMySQLValue userId == "" then Nothing else Just (read $ fromMySQLValue userId :: Int))
        (if fromMySQLValue date == "" then Nothing else Just $ fromMySQLValue date)
        (if fromMySQLValue duration == "" then Nothing else Just (read $ fromMySQLValue duration :: Int))
        (if fromMySQLValue action == "" then Nothing else Just $ fromMySQLValue action)

getServiceStatistics :: MySQLConn -> Int -> IO [UsageStatistics]
getServiceStatistics conn serviceId = do
    rows <- queryDB conn "SELECT id, service_id, user_id, access_date, duration_minutes, action_type FROM usage_statistics WHERE service_id = ?" [show serviceId]
    return $ map parseUsageStatistics rows
  where
    parseUsageStatistics [id, serviceId, userId, date, duration, action] = UsageStatistics
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (if fromMySQLValue userId == "" then Nothing else Just (read $ fromMySQLValue userId :: Int))
        (if fromMySQLValue date == "" then Nothing else Just $ fromMySQLValue date)
        (if fromMySQLValue duration == "" then Nothing else Just (read $ fromMySQLValue duration :: Int))
        (if fromMySQLValue action == "" then Nothing else Just $ fromMySQLValue action)

recordUsage :: MySQLConn -> Int -> Maybe Int -> Maybe Int -> Maybe String -> IO Int
recordUsage conn serviceId userId duration action = do
    executeDB conn "INSERT INTO usage_statistics (service_id, user_id, duration_minutes, action_type) VALUES (?, ?, ?, ?)"
        [show serviceId, maybe "" show userId, maybe "" show duration, maybe "" (\x -> x) action]
    return 0

-- Versions operations

listVersions :: MySQLConn -> IO [Version]
listVersions conn = do
    rows <- queryDB conn "SELECT id, service_id, version_number, release_date, is_current FROM versions ORDER BY id" []
    return $ map parseVersion rows
  where
    parseVersion [id, serviceId, num, date, isCurrent] = Version
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue num)
        (if fromMySQLValue date == "" then Nothing else Just $ fromMySQLValue date)
        (read $ fromMySQLValue isCurrent :: Bool)

-- TermsConditions operations

listTermsConditions :: MySQLConn -> IO [TermsConditions]
listTermsConditions conn = do
    rows <- queryDB conn "SELECT id, service_id, rules, usage_conditions, deadline FROM terms_conditions ORDER BY id" []
    return $ map parseTerms rows
  where
    parseTerms [id, serviceId, rules, conditions, deadline] = TermsConditions
        (read $ fromMySQLValue id :: Int)
        (read $ fromMySQLValue serviceId :: Int)
        (fromMySQLValue rules)
        (if fromMySQLValue conditions == "" then Nothing else Just $ fromMySQLValue conditions)
        (if fromMySQLValue deadline == "" then Nothing else Just $ fromMySQLValue deadline)

