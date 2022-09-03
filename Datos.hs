module Datos 
( Game
, parseGameList
, dateParser
, getId
, getName
, getRelDate
, inEnglish
, getDeveloper
, getPublisher
, getPlatforms
, getMinAge
, getCategories
, getGenres
, getTags
, getAchievements
, getPosRatings
, getNegRatings
, getAvgPlaytime
, getMedPlaytime
, getOwners
, getPrice
, Owners
, lBound
, uBound
) where

-- ------------------------------------------------------------------------
-- Modulo que continene los tipos de datos algebraicos para albergar
-- los datos leidos del CSV y las funciones que son necesarias para leer
-- y parsear los datos.
-- ------------------------------------------------------------------------


-- Imports
import Data.Dates
import Data.Time
import Data.List.Split


-- Tipos de datos Game

data Game = 
    G {
        gId :: Int,
        name :: String,
        rDate :: DateTime,
        english :: Bool,
        developer :: [String],
        publisher :: [String],
        platforms :: [String],
        minAge :: Int,
        categories :: [String],
        genres :: [String],
        tags :: [String],
        achievements :: Int,
        pRatings :: Int,
        nRatings :: Int,
        avgPlaytime :: Int,
        medPlaytime :: Int,
        owners :: Owners,
        price :: Double
    }
    deriving (Show, Eq)

-- Funciones de parseo de Game

readGame :: [String] -> Game
readGame g = G {
    gId = read (g!!0) :: Int,
    name = g!!1,
    rDate = dateParser (g!!2),
    english = if g!!3 == "1" then True else False,
    developer = parseList (g!!4),
    publisher = parseList (g!!5),
    platforms = parseList (g!!6),
    minAge = read (g!!7) :: Int,
    categories = parseList (g!!8),
    genres =  parseList(g!!9),
    tags = parseList (g!!10),
    achievements = read (g!!11) :: Int,
    pRatings = read (g!!12) :: Int,
    nRatings = read (g!!13) :: Int,
    avgPlaytime = read (g!!14) :: Int,
    medPlaytime = read (g!!15) :: Int,
    owners = parseOwners (g!!16),
    price = read (g!!17) :: Double
}

dateParser :: String -> DateTime
dateParser fecha = dayToDateTime (fromGregorian a m d)
    where a = read (take 4 fecha) :: Integer
          m = read (take 2 (drop 5 fecha)) :: Int
          d = read (take 2 (drop 8 fecha)) :: Int

parseList :: String -> [String]
parseList xs = splitOn ";" xs


parseGameList :: [[String]] -> [Game]
parseGameList gs = [readGame g | g <- gs]

-- Funciones para trabajar con Game

getId :: Game -> Int
getId x = gId x

getName :: Game -> String
getName x = name x

getRelDate :: Game -> DateTime
getRelDate x = rDate x

inEnglish :: Game -> Bool
inEnglish x = english x

getDeveloper :: Game -> [String]
getDeveloper x = developer x

getPublisher :: Game -> [String]
getPublisher x = publisher x

getPlatforms :: Game -> [String]
getPlatforms x = platforms x

getMinAge :: Game -> Int
getMinAge x = minAge x

getCategories :: Game -> [String]
getCategories x = categories x

getGenres :: Game -> [String]
getGenres x = genres x

getTags :: Game -> [String]
getTags x = tags x

getAchievements :: Game -> Int
getAchievements x = achievements x

getPosRatings :: Game -> Int
getPosRatings x = pRatings x

getNegRatings :: Game -> Int
getNegRatings x = nRatings x

getAvgPlaytime :: Game -> Int
getAvgPlaytime x = avgPlaytime x

getMedPlaytime :: Game -> Int
getMedPlaytime x = medPlaytime x
        
getOwners :: Game -> Owners
getOwners x = owners x
        
getPrice :: Game -> Double
getPrice x = price x


-- Tipo de datos Owners

data Owners = NOwners Int Int 
    deriving (Eq)

instance Show Owners where
    show (NOwners lb ub) = (show lb) ++ "-" ++ (show ub)


-- Funciones de Owners

lBound :: Owners -> Int
lBound (NOwners lb ub) = lb

uBound :: Owners -> Int
uBound (NOwners lb ub) = ub


-- Parseador de Owners

parseOwners :: String -> Owners
parseOwners xs = NOwners lb ub
    where lb = read (head numbers) :: Int
          ub = read (last numbers) :: Int
          numbers = splitOn "-" xs