module CLI where

import Models
import DAO
import DB
import Database.MySQL.Base
import Control.Monad
import Data.Maybe
import System.IO (hFlush, stdout)

-- Display entities with their IDs for selection
displayListWithIds :: (Entity a, DetailedShow a) => [a] -> String -> IO ()
displayListWithIds items entityTypeName = case null items of
    True -> putStrLn $ "No " ++ entityTypeName ++ " found."
    False -> do
        putStrLn $ "\nAvailable " ++ entityTypeName ++ " (select by ID):"
        putStrLn "----------------------------------------"
        mapM_ (\item -> putStrLn $ "ID: " ++ show (entityId item) ++ " - " ++ entityName item) items
        putStrLn "----------------------------------------"

-- Helper function to safely read Int
safeReadInt :: String -> IO Int
safeReadInt prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    case reads input :: [(Int, String)] of
        [(val, "")] -> return val
        _ -> do
            putStrLn "Invalid input! Please enter a valid number."
            safeReadInt prompt

-- Main application menu
mainMenu :: MySQLConn -> IO ()
mainMenu conn = do
    putStrLn "\n[ Faculty Network Services Information System ]"
    putStrLn "1. Authors"
    putStrLn "2. Service Types"
    putStrLn "3. Services"
    putStrLn "4. System Users"
    putStrLn "5. User Registrations"
    putStrLn "6. Usage Statistics"
    putStrLn "7. Versions"
    putStrLn "8. Terms & Conditions"
    putStrLn "0. Exit"
    putStr "> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> authorsMenu conn
        "2" -> serviceTypesMenu conn
        "3" -> servicesMenu conn
        "4" -> systemUsersMenu conn
        "5" -> registrationsMenu conn
        "6" -> statisticsMenu conn
        "7" -> versionsMenu conn
        "8" -> termsMenu conn
        "0" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid option. Please enter a number from 0 to 8."
            mainMenu conn

-- Author management menu
authorsMenu :: MySQLConn -> IO ()
authorsMenu conn = do
    putStrLn "\nAuthor Management Menu"
    putStrLn "1. List all authors"
    putStrLn "2. Add author"
    putStrLn "3. Update author"
    putStrLn "4. Delete author"
    putStrLn "0. Back to main menu"
    putStr "> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            authors <- listAuthors conn
            case null authors of
                True -> putStrLn "No authors found in the database."
                False -> do
                    putStrLn $ "Displaying " ++ show (length authors) ++ " authors:"
                    mapM_ (\a -> putStrLn $ "\n" ++ detailedShow a) authors
            authorsMenu conn
        "2" -> do
            putStr "Enter author's full name: "
            hFlush stdout
            name <- getLine
            case null name of
                True -> do
                    putStrLn "Error: Name is required! Operation cancelled."
                    authorsMenu conn
                False -> do
                    putStr "Email (optional, press Enter to skip): "
                    hFlush stdout
                    email <- getLine
                    putStr "Department (optional, press Enter to skip): "
                    hFlush stdout
                    dept <- getLine
                    let authorEmail = if null email then Nothing else Just email
                        authorDept = if null dept then Nothing else Just dept
                    createAuthor conn name authorEmail authorDept
                    putStrLn "Author added."
                    authorsMenu conn
        "3" -> do
            authors <- listAuthors conn
            displayListWithIds authors "authors"
            authorId <- safeReadInt "Enter author ID: "
            author <- getAuthorById conn authorId
            case author of
                Nothing -> do
                    putStrLn $ "Error: Author with ID " ++ show authorId ++ " not found! Operation cancelled."
                    authorsMenu conn
                Just a -> do
                    putStrLn $ "\nCurrent: " ++ detailedShow a
                    putStr "Author's Full Name: "
                    hFlush stdout
                    name <- getLine
                    let updatedName = case null name of
                            True -> entityName a
                            False -> name
                    putStr "Email: "
                    hFlush stdout
                    email <- getLine
                    putStr "Department: "
                    hFlush stdout
                    dept <- getLine
                    let authorEmail = if null email then Nothing else Just email
                        authorDept = if null dept then Nothing else Just dept
                    wasUpdated <- updateAuthor conn authorId updatedName authorEmail authorDept
                    case wasUpdated of
                        True -> putStrLn "Author updated."
                        False -> putStrLn "Failed to update author!"
                    authorsMenu conn
        "4" -> do
            authors <- listAuthors conn
            displayListWithIds authors "authors"
            authorId <- safeReadInt "Enter author ID: "
            wasDeleted <- deleteAuthor conn authorId
            case wasDeleted of
                True -> putStrLn "Author deleted."
                False -> putStrLn "Failed to delete author!"
            authorsMenu conn
        "0" -> mainMenu conn
        _ -> authorsMenu conn

-- Service type management menu
serviceTypesMenu :: MySQLConn -> IO ()
serviceTypesMenu conn = do
    putStrLn "\n[ Service Types ]"
    putStrLn "1. List all service types"
    putStrLn "2. Add service type"
    putStrLn "0. Back to main menu"
    putStr "> "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            types <- listServiceTypes conn
            case null types of
                True -> putStrLn "No service types found in the database."
                False -> do
                    putStrLn $ "Showing " ++ show (length types) ++ " service type(s):"
                    mapM_ (\t -> putStrLn $ "\n" ++ detailedShow t) types
            serviceTypesMenu conn
        "2" -> do
            putStr "Service type name: "
            hFlush stdout
            name <- getLine
            case null name of
                True -> do
                    putStrLn "Error: Name is required! Operation cancelled."
                    serviceTypesMenu conn
                False -> do
                    putStr "Description: "
                    hFlush stdout
                    desc <- getLine
                    let serviceDesc = if null desc then Nothing else Just desc
                    createServiceType conn name serviceDesc
                    putStrLn "Service type added."
                    serviceTypesMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0, 1, or 2."
            serviceTypesMenu conn

-- Services menu
servicesMenu :: MySQLConn -> IO ()
servicesMenu conn = do
    putStrLn "\n Services Management"
    putStrLn "1. List all services"
    putStrLn "2. Add service"
    putStrLn "3. Update service"
    putStrLn "4. Delete service"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            services <- listServices conn
            case null services of
                True -> putStrLn "No services found in the database"
                False -> do
                    putStrLn $ "Displaying " ++ show (length services) ++ " service(s):"
                    mapM_ (\s -> putStrLn $ "\n" ++ detailedShow s) services
            servicesMenu conn
        "2" -> do
            authors <- listAuthors conn
            displayListWithIds authors "authors"
            authorId <- safeReadInt "Author ID: "
            types <- listServiceTypes conn
            displayListWithIds types "service types"
            typeId <- safeReadInt "Service Type ID: "
            putStr "Service name: "
            hFlush stdout
            name <- getLine
            case null name of
                True -> do
                    putStrLn "Error: Service name is required! Operation cancelled."
                    servicesMenu conn
                False -> do
                    putStr "Annotation: "
                    hFlush stdout
                    annotation <- getLine
                    let serviceAnnotation = if null annotation then Nothing else Just annotation
                    createService conn name authorId serviceAnnotation typeId
                    putStrLn "Service added."
                    servicesMenu conn
        "3" -> do
            services <- listServices conn
            displayListWithIds services "services"
            serviceId <- safeReadInt "Enter service ID: "
            service <- getServiceById conn serviceId
            case service of
                Nothing -> do
                    putStrLn $ "Error: Service with ID " ++ show serviceId ++ " not found!"
                    servicesMenu conn
                Just s -> do
                    authors <- listAuthors conn
                    displayListWithIds authors "authors"
                    putStr "Author ID: "
                    hFlush stdout
                    authorIdStr <- getLine
                    types <- listServiceTypes conn
                    displayListWithIds types "service types"
                    putStr "Service Type ID: "
                    hFlush stdout
                    typeIdStr <- getLine
                    putStr "Service Name: "
                    hFlush stdout
                    name <- getLine
                    putStr "Annotation: "
                    hFlush stdout
                    annotation <- getLine
                    let (Service _ _ currentAuthorId _ currentTypeId) = s
                        finalName = if null name then entityName s else name
                        finalAuthorId = if null authorIdStr then currentAuthorId else read authorIdStr
                        finalTypeId = if null typeIdStr then currentTypeId else read typeIdStr
                        serviceAnnotation = if null annotation then Nothing else Just annotation
                    wasUpdated <- updateService conn serviceId finalName finalAuthorId serviceAnnotation finalTypeId
                    case wasUpdated of
                        True -> putStrLn "Service updated."
                        False -> putStrLn "Failed to update service!"
                    servicesMenu conn
        "4" -> do
            services <- listServices conn
            displayListWithIds services "services"
            serviceId <- safeReadInt "Enter service ID: "
            wasDeleted <- deleteService conn serviceId
            case wasDeleted of
                True -> putStrLn "Service deleted."
                False -> putStrLn "Failed to delete service!"
            servicesMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0, 1, 2, 3, or 4."
            servicesMenu conn

-- System Users menu
systemUsersMenu :: MySQLConn -> IO ()
systemUsersMenu conn = do
    putStrLn "\nSystem Users Management"
    putStrLn "1. List all users"
    putStrLn "2. Add user"
    putStrLn "3. Update user"
    putStrLn "4. Delete user"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            users <- listSystemUsers conn
            case null users of
                True -> putStrLn "No users found in the database."
                False -> do
                    putStrLn $ "Displaying " ++ show (length users) ++ " user(s):"
                    mapM_ (\u -> putStrLn $ "\n" ++ detailedShow u) users
            systemUsersMenu conn
        "2" -> do
            putStr "Username (unique): "
            hFlush stdout
            username <- getLine
            case null username of
                True -> do
                    putStrLn "Error: Username is required! Operation cancelled."
                    systemUsersMenu conn
                False -> do
                    putStr "Full Name: "
                    hFlush stdout
                    fullName <- getLine
                    putStr "Email: "
                    hFlush stdout
                    email <- getLine
                    putStr "Role: "
                    hFlush stdout
                    role <- getLine
                    let userFullName = if null fullName then Nothing else Just fullName
                        userEmail = if null email then Nothing else Just email
                        userRole = if null role then Nothing else Just role
                    createSystemUser conn username userFullName userEmail userRole
                    putStrLn "User added."
                    systemUsersMenu conn
        "3" -> do
            users <- listSystemUsers conn
            displayListWithIds users "users"
            userId <- safeReadInt "Enter user ID: "
            user <- getSystemUserById conn userId
            case user of
                Nothing -> do
                    putStrLn $ "Error: User with ID " ++ show userId ++ " not found! Operation cancelled."
                    systemUsersMenu conn
                Just u -> do
                    putStrLn $ "\nCurrent: " ++ detailedShow u
                    putStr "Username (unique): "
                    hFlush stdout
                    username <- getLine
                    let updatedUsername = case null username of
                            True -> entityName u
                            False -> username
                    putStr "Full Name: "
                    hFlush stdout
                    fullName <- getLine
                    putStr "Email: "
                    hFlush stdout
                    email <- getLine
                    putStr "Role: "
                    hFlush stdout
                    role <- getLine
                    let userFullName = if null fullName then Nothing else Just fullName
                        userEmail = if null email then Nothing else Just email
                        userRole = if null role then Nothing else Just role
                    wasUpdated <- updateSystemUser conn userId updatedUsername userFullName userEmail userRole
                    case wasUpdated of
                        True -> putStrLn "User updated."
                        False -> putStrLn "Failed to update user!"
                    systemUsersMenu conn
        "4" -> do
            users <- listSystemUsers conn
            displayListWithIds users "users"
            userId <- safeReadInt "Enter user ID: "
            wasDeleted <- deleteSystemUser conn userId
            case wasDeleted of
                True -> putStrLn "User deleted."
                False -> putStrLn "Failed to delete user!"
            systemUsersMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0, 1, 2, 3, or 4."
            systemUsersMenu conn

-- User Registrations menu
registrationsMenu :: MySQLConn -> IO ()
registrationsMenu conn = do
    putStrLn "\nUser Registrations"
    putStrLn "1. List all registrations"
    putStrLn "2. List registrations by user"
    putStrLn "3. Register user to service"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            regs <- listUserRegistrations conn
            if null regs
                then putStrLn "No registrations found in the database."
                else do
                    putStrLn $ "Found " ++ show (length regs) ++ " registration(s):"
                    mapM_ (\r -> putStrLn $ "\n" ++ detailedShow r) regs
            registrationsMenu conn
        "2" -> do
            users <- listSystemUsers conn
            displayListWithIds users "users"
            userId <- safeReadInt "Enter user ID: "
            regs <- listRegistrationsByUser conn userId
            if null regs
                then putStrLn $ "No registrations found for user ID " ++ show userId ++ "."
                else do
                    putStrLn $ "\nFound " ++ show (length regs) ++ " registration(s) for this user:"
                    mapM_ (\r -> putStrLn $ "\n" ++ detailedShow r) regs
            registrationsMenu conn
        "3" -> do
            users <- listSystemUsers conn
            displayListWithIds users "users"
            userId <- safeReadInt "User ID: "
            services <- listServices conn
            displayListWithIds services "services"
            serviceId <- safeReadInt "Service ID: "
            registerUserToService conn userId serviceId
            putStrLn "User registered."
            registrationsMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0, 1, 2, or 3."
            registrationsMenu conn

-- Usage Statistics menu
statisticsMenu :: MySQLConn -> IO ()
statisticsMenu conn = do
    putStrLn "\nUsage Statistics"
    putStrLn "1. List all statistics"
    putStrLn "2. Get service statistics"
    putStrLn "3. Record usage"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            stats <- listUsageStatistics conn
            if null stats
                then putStrLn "No statistics found in the database."
                else do
                    putStrLn $ "Found " ++ show (length stats) ++ " statistics:"
                    mapM_ (\s -> putStrLn $ "\n" ++ detailedShow s) stats
            statisticsMenu conn
        "2" -> do
            services <- listServices conn
            displayListWithIds services "services"
            serviceId <- safeReadInt "Enter service ID: "
            stats <- getServiceStatistics conn serviceId
            if null stats
                then putStrLn $ "No statistics found for service ID " ++ show serviceId ++ "."
                else do
                    putStrLn $ "\nFound " ++ show (length stats) ++ " statistics for this service:"
                    mapM_ (\s -> putStrLn $ "\n" ++ detailedShow s) stats
            statisticsMenu conn
        "3" -> do
            services <- listServices conn
            displayListWithIds services "services"
            serviceId <- safeReadInt "Service ID: "
            users <- listSystemUsers conn
            displayListWithIds users "users"
            putStr "User ID: "
            hFlush stdout
            userIdStr <- getLine
            putStr "Duration in Minutes: "
            hFlush stdout
            durationStr <- getLine
            putStr "Action Type: "
            hFlush stdout
            actionStr <- getLine
            let userId = if null userIdStr then Nothing else Just (read userIdStr :: Int)
            let duration = if null durationStr then Nothing else Just (read durationStr :: Int)
            let action = if null actionStr then Nothing else Just actionStr
            recordUsage conn serviceId userId duration action
            putStrLn "Usage recorded."
            statisticsMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0, 1, 2, or 3."
            statisticsMenu conn

-- Versions menu
versionsMenu :: MySQLConn -> IO ()
versionsMenu conn = do
    putStrLn "\nVersions Management"
    putStrLn "1. List all versions"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            versions <- listVersions conn
            if null versions
                then putStrLn "No versions found in the database."
                else do
                    putStrLn $ "Found " ++ show (length versions) ++ " versions:"
                    mapM_ (\v -> putStrLn $ "\n" ++ detailedShow v) versions
            versionsMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0 or 1."
            versionsMenu conn

-- Terms & Conditions menu
termsMenu :: MySQLConn -> IO ()
termsMenu conn = do
    putStrLn "\nTerms & Conditions Management"
    putStrLn "1. List all terms & conditions"
    putStrLn "0. Back to main menu"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            terms <- listTermsConditions conn
            if null terms
                then putStrLn "No terms & conditions found in the database."
                else do
                    putStrLn $ "Found " ++ show (length terms) ++ " term(s) & condition(s):"
                    mapM_ (\t -> putStrLn $ "\n" ++ detailedShow t) terms
            termsMenu conn
        "0" -> mainMenu conn
        _ -> do
            putStrLn "Invalid option! Please enter 0 or 1."
            termsMenu conn

