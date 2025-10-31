module CLI where

import Models
import DAO
import DB
import Database.MySQL.Base
import Control.Monad
import Data.Maybe

-- Main menu
mainMenu :: MySQLConn -> IO ()
mainMenu conn = do
    putStrLn "\n=== Faculty Network Services Information System ==="
    putStrLn "1. Authors"
    putStrLn "2. Service Types"
    putStrLn "3. Services"
    putStrLn "4. System Users"
    putStrLn "5. User Registrations"
    putStrLn "6. Usage Statistics"
    putStrLn "7. Versions"
    putStrLn "8. Terms & Conditions"
    putStrLn "0. Exit"
    putStr "Choose option: "
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
            putStrLn "Invalid option!"
            mainMenu conn

-- Authors menu
authorsMenu :: MySQLConn -> IO ()
authorsMenu conn = do
    putStrLn "\n=== Authors Management ==="
    putStrLn "1. List all authors"
    putStrLn "2. Add author"
    putStrLn "3. Update author"
    putStrLn "4. Delete author"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            authors <- listAuthors conn
            mapM_ (\a -> putStrLn $ detailedShow a) authors
            authorsMenu conn
        "2" -> do
            putStr "Enter name: "
            name <- getLine
            putStr "Enter email (or press Enter to skip): "
            email <- getLine
            putStr "Enter department (or press Enter to skip): "
            dept <- getLine
            createAuthor conn name (if null email then Nothing else Just email) (if null dept then Nothing else Just dept)
            putStrLn "Author added!"
            authorsMenu conn
        "3" -> do
            putStr "Enter author ID: "
            id <- readLn
            putStr "Enter new name: "
            name <- getLine
            putStr "Enter new email (or press Enter to skip): "
            email <- getLine
            putStr "Enter new department (or press Enter to skip): "
            dept <- getLine
            success <- updateAuthor conn id name (if null email then Nothing else Just email) (if null dept then Nothing else Just dept)
            if success then putStrLn "Author updated!" else putStrLn "Author not found!"
            authorsMenu conn
        "4" -> do
            putStr "Enter author ID: "
            id <- readLn
            success <- deleteAuthor conn id
            if success then putStrLn "Author deleted!" else putStrLn "Author not found!"
            authorsMenu conn
        "0" -> mainMenu conn
        _ -> authorsMenu conn

-- Service Types menu
serviceTypesMenu :: MySQLConn -> IO ()
serviceTypesMenu conn = do
    putStrLn "\n=== Service Types Management ==="
    putStrLn "1. List all service types"
    putStrLn "2. Add service type"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            types <- listServiceTypes conn
            mapM_ (\t -> putStrLn $ detailedShow t) types
            serviceTypesMenu conn
        "2" -> do
            putStr "Enter name: "
            name <- getLine
            putStr "Enter description (or press Enter to skip): "
            desc <- getLine
            createServiceType conn name (if null desc then Nothing else Just desc)
            putStrLn "Service type added!"
            serviceTypesMenu conn
        "0" -> mainMenu conn
        _ -> serviceTypesMenu conn

-- Services menu
servicesMenu :: MySQLConn -> IO ()
servicesMenu conn = do
    putStrLn "\n=== Services Management ==="
    putStrLn "1. List all services"
    putStrLn "2. Add service"
    putStrLn "3. Update service"
    putStrLn "4. Delete service"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            services <- listServices conn
            mapM_ (\s -> putStrLn $ detailedShow s) services
            servicesMenu conn
        "2" -> do
            putStr "Enter name: "
            name <- getLine
            putStr "Enter author ID: "
            authorId <- readLn
            putStr "Enter annotation (or press Enter to skip): "
            annotation <- getLine
            putStr "Enter service type ID: "
            typeId <- readLn
            createService conn name authorId (if null annotation then Nothing else Just annotation) typeId
            putStrLn "Service added!"
            servicesMenu conn
        "3" -> do
            putStr "Enter service ID: "
            id <- readLn
            putStr "Enter new name: "
            name <- getLine
            putStr "Enter new author ID: "
            authorId <- readLn
            putStr "Enter new annotation (or press Enter to skip): "
            annotation <- getLine
            putStr "Enter new service type ID: "
            typeId <- readLn
            success <- updateService conn id name authorId (if null annotation then Nothing else Just annotation) typeId
            if success then putStrLn "Service updated!" else putStrLn "Service not found!"
            servicesMenu conn
        "4" -> do
            putStr "Enter service ID: "
            id <- readLn
            success <- deleteService conn id
            if success then putStrLn "Service deleted!" else putStrLn "Service not found!"
            servicesMenu conn
        "0" -> mainMenu conn
        _ -> servicesMenu conn

-- System Users menu
systemUsersMenu :: MySQLConn -> IO ()
systemUsersMenu conn = do
    putStrLn "\n=== System Users Management ==="
    putStrLn "1. List all users"
    putStrLn "2. Add user"
    putStrLn "3. Update user"
    putStrLn "4. Delete user"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            users <- listSystemUsers conn
            mapM_ (\u -> putStrLn $ detailedShow u) users
            systemUsersMenu conn
        "2" -> do
            putStr "Enter username: "
            username <- getLine
            putStr "Enter full name (or press Enter to skip): "
            fullName <- getLine
            putStr "Enter email (or press Enter to skip): "
            email <- getLine
            putStr "Enter role (or press Enter to skip): "
            role <- getLine
            createSystemUser conn username (if null fullName then Nothing else Just fullName) (if null email then Nothing else Just email) (if null role then Nothing else Just role)
            putStrLn "User added!"
            systemUsersMenu conn
        "3" -> do
            putStr "Enter user ID: "
            id <- readLn
            putStr "Enter new username: "
            username <- getLine
            putStr "Enter new full name (or press Enter to skip): "
            fullName <- getLine
            putStr "Enter new email (or press Enter to skip): "
            email <- getLine
            putStr "Enter new role (or press Enter to skip): "
            role <- getLine
            success <- updateSystemUser conn id username (if null fullName then Nothing else Just fullName) (if null email then Nothing else Just email) (if null role then Nothing else Just role)
            if success then putStrLn "User updated!" else putStrLn "User not found!"
            systemUsersMenu conn
        "4" -> do
            putStr "Enter user ID: "
            id <- readLn
            success <- deleteSystemUser conn id
            if success then putStrLn "User deleted!" else putStrLn "User not found!"
            systemUsersMenu conn
        "0" -> mainMenu conn
        _ -> systemUsersMenu conn

-- User Registrations menu
registrationsMenu :: MySQLConn -> IO ()
registrationsMenu conn = do
    putStrLn "\n=== User Registrations ==="
    putStrLn "1. List all registrations"
    putStrLn "2. List registrations by user"
    putStrLn "3. Register user to service"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            regs <- listUserRegistrations conn
            mapM_ (\r -> putStrLn $ detailedShow r) regs
            registrationsMenu conn
        "2" -> do
            putStr "Enter user ID: "
            userId <- readLn
            regs <- listRegistrationsByUser conn userId
            mapM_ (\r -> putStrLn $ detailedShow r) regs
            registrationsMenu conn
        "3" -> do
            putStr "Enter user ID: "
            userId <- readLn
            putStr "Enter service ID: "
            serviceId <- readLn
            registerUserToService conn userId serviceId
            putStrLn "User registered to service!"
            registrationsMenu conn
        "0" -> mainMenu conn
        _ -> registrationsMenu conn

-- Usage Statistics menu
statisticsMenu :: MySQLConn -> IO ()
statisticsMenu conn = do
    putStrLn "\n=== Usage Statistics ==="
    putStrLn "1. List all statistics"
    putStrLn "2. Get service statistics"
    putStrLn "3. Record usage"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            stats <- listUsageStatistics conn
            mapM_ (\s -> putStrLn $ detailedShow s) stats
            statisticsMenu conn
        "2" -> do
            putStr "Enter service ID: "
            serviceId <- readLn
            stats <- getServiceStatistics conn serviceId
            mapM_ (\s -> putStrLn $ detailedShow s) stats
            statisticsMenu conn
        "3" -> do
            putStr "Enter service ID: "
            serviceId <- readLn
            putStr "Enter user ID (or press Enter to skip): "
            userIdStr <- getLine
            putStr "Enter duration in minutes (or press Enter to skip): "
            durationStr <- getLine
            putStr "Enter action type (or press Enter to skip): "
            actionStr <- getLine
            let userId = if null userIdStr then Nothing else Just (read userIdStr :: Int)
            let duration = if null durationStr then Nothing else Just (read durationStr :: Int)
            let action = if null actionStr then Nothing else Just actionStr
            recordUsage conn serviceId userId duration action
            putStrLn "Usage recorded!"
            statisticsMenu conn
        "0" -> mainMenu conn
        _ -> statisticsMenu conn

-- Versions menu
versionsMenu :: MySQLConn -> IO ()
versionsMenu conn = do
    putStrLn "\n=== Versions ==="
    putStrLn "1. List all versions"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            versions <- listVersions conn
            mapM_ (\v -> putStrLn $ detailedShow v) versions
            versionsMenu conn
        "0" -> mainMenu conn
        _ -> versionsMenu conn

-- Terms & Conditions menu
termsMenu :: MySQLConn -> IO ()
termsMenu conn = do
    putStrLn "\n=== Terms & Conditions ==="
    putStrLn "1. List all terms & conditions"
    putStrLn "0. Back to main menu"
    putStr "Choose option: "
    choice <- getLine
    case choice of
        "1" -> do
            terms <- listTermsConditions conn
            mapM_ (\t -> putStrLn $ detailedShow t) terms
            termsMenu conn
        "0" -> mainMenu conn
        _ -> termsMenu conn


