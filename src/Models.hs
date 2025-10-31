{-# LANGUAGE DuplicateRecordFields #-}
module Models where

-- Type classes for entities

class Entity a where
    entityId :: a -> Int
    entityName :: a -> String

class DetailedShow a where
    detailedShow :: a -> String

-- Author model
data Author = Author {
    authorId :: Int,
    authorName :: String,
    authorEmail :: Maybe String,
    authorDepartment :: Maybe String
} deriving (Show)

instance Entity Author where
    entityId = authorId
    entityName = authorName

instance DetailedShow Author where
    detailedShow (Author id name email dept) =
        "Author #" ++ show id ++ "\n" ++
        "  Name: " ++ name ++ "\n" ++
        "  Email: " ++ maybe "N/A" (\x -> x) email ++ "\n" ++
        "  Department: " ++ maybe "N/A" (\x -> x) dept

-- ServiceType model
data ServiceType = ServiceType {
    serviceTypeId :: Int,
    serviceTypeName :: String,
    serviceTypeDescription :: Maybe String
} deriving (Show)

instance Entity ServiceType where
    entityId (ServiceType id _ _) = id
    entityName = serviceTypeName

instance DetailedShow ServiceType where
    detailedShow (ServiceType id name desc) =
        "Service Type #" ++ show id ++ "\n" ++
        "  Name: " ++ name ++ "\n" ++
        "  Description: " ++ maybe "N/A" (\x -> x) desc

-- Service model
data Service = Service {
    serviceId :: Int,
    serviceName :: String,
    serviceAuthorId :: Int,
    serviceAnnotation :: Maybe String,
    serviceTypeId :: Int
} deriving (Show)

instance Entity Service where
    entityId = serviceId
    entityName = serviceName

instance DetailedShow Service where
    detailedShow (Service id name authorId annotation typeId) =
        "Service #" ++ show id ++ "\n" ++
        "  Name: " ++ name ++ "\n" ++
        "  Author ID: " ++ show authorId ++ "\n" ++
        "  Annotation: " ++ maybe "N/A" (\x -> x) annotation ++ "\n" ++
        "  Service Type ID: " ++ show typeId

-- Version model
data Version = Version {
    versionId :: Int,
    versionServiceId :: Int,
    versionNumber :: String,
    versionReleaseDate :: Maybe String,
    versionIsCurrent :: Bool
} deriving (Show)

instance Entity Version where
    entityId = versionId
    entityName = versionNumber

instance DetailedShow Version where
    detailedShow (Version id serviceId num date isCurrent) =
        "Version #" ++ show id ++ "\n" ++
        "  Service ID: " ++ show serviceId ++ "\n" ++
        "  Version Number: " ++ num ++ "\n" ++
        "  Release Date: " ++ maybe "N/A" (\x -> x) date ++ "\n" ++
        "  Is Current: " ++ show isCurrent

-- TermsConditions model
data TermsConditions = TermsConditions {
    termsId :: Int,
    termsServiceId :: Int,
    termsRules :: String,
    termsUsageConditions :: Maybe String,
    termsDeadline :: Maybe String
} deriving (Show)

instance Entity TermsConditions where
    entityId = termsId
    entityName = termsRules

instance DetailedShow TermsConditions where
    detailedShow (TermsConditions id serviceId rules conditions deadline) =
        "Terms & Conditions #" ++ show id ++ "\n" ++
        "  Service ID: " ++ show serviceId ++ "\n" ++
        "  Rules: " ++ rules ++ "\n" ++
        "  Usage Conditions: " ++ maybe "N/A" (\x -> x) conditions ++ "\n" ++
        "  Deadline: " ++ maybe "N/A" (\x -> x) deadline

-- SystemUser model
data SystemUser = SystemUser {
    systemUserId :: Int,
    systemUserUsername :: String,
    systemUserFullName :: Maybe String,
    systemUserEmail :: Maybe String,
    systemUserRole :: Maybe String
} deriving (Show)

instance Entity SystemUser where
    entityId = systemUserId
    entityName = systemUserUsername

instance DetailedShow SystemUser where
    detailedShow (SystemUser id username fullName email role) =
        "System User #" ++ show id ++ "\n" ++
        "  Username: " ++ username ++ "\n" ++
        "  Full Name: " ++ maybe "N/A" (\x -> x) fullName ++ "\n" ++
        "  Email: " ++ maybe "N/A" (\x -> x) email ++ "\n" ++
        "  Role: " ++ maybe "N/A" (\x -> x) role

-- UserRegistration model
data UserRegistration = UserRegistration {
    registrationId :: Int,
    registrationUserId :: Int,
    registrationServiceId :: Int,
    registrationDate :: Maybe String,
    registrationStatus :: Maybe String
} deriving (Show)

instance Entity UserRegistration where
    entityId = registrationId
    entityName = ("Registration #" ++) . show . registrationId

instance DetailedShow UserRegistration where
    detailedShow (UserRegistration id userId serviceId date status) =
        "User Registration #" ++ show id ++ "\n" ++
        "  User ID: " ++ show userId ++ "\n" ++
        "  Service ID: " ++ show serviceId ++ "\n" ++
        "  Registration Date: " ++ maybe "N/A" (\x -> x) date ++ "\n" ++
        "  Status: " ++ maybe "N/A" (\x -> x) status

-- UsageStatistics model
data UsageStatistics = UsageStatistics {
    statisticsId :: Int,
    statisticsServiceId :: Int,
    statisticsUserId :: Maybe Int,
    statisticsAccessDate :: Maybe String,
    statisticsDurationMinutes :: Maybe Int,
    statisticsActionType :: Maybe String
} deriving (Show)

instance Entity UsageStatistics where
    entityId = statisticsId
    entityName = ("Statistics #" ++) . show . statisticsId

instance DetailedShow UsageStatistics where
    detailedShow (UsageStatistics id serviceId userId date duration action) =
        "Usage Statistics #" ++ show id ++ "\n" ++
        "  Service ID: " ++ show serviceId ++ "\n" ++
        "  User ID: " ++ maybe "N/A" (show) userId ++ "\n" ++
        "  Access Date: " ++ maybe "N/A" (\x -> x) date ++ "\n" ++
        "  Duration (minutes): " ++ maybe "N/A" (show) duration ++ "\n" ++
        "  Action Type: " ++ maybe "N/A" (\x -> x) action


