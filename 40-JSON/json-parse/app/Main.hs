module Main where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

{-
data ErrorMessage = ErrorMessage
    { message :: T.Text
    , errorCode :: Int
    } deriving (Show, Generic)

--instance FromJSON ErrorMessage
--instance ToJSON ErrorMessage

Сделать ЭррорМесседж членом ФромДЖСОН и ТуДЖСОН - либо сделать ЭррорМесседж
членом Generic и написать строку "instance ToJSON ErrorMessage"
либо написать все вручную вот так:

instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage
        <$> v .: "message" -- (.:) :: FromJSON a => Object -> Text -> Parser a
        <*> v .: "error"   -- Получается тупо конструктор ЭррорМесседжа в контексте

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) =
        object [ "message" .= message
               , "error" .= errorCode
               ]
-}

data NOAAResult = NOAAResult
    { uid :: T.Text
    , mindate :: T.Text
    , maxdate :: T.Text
    , name :: T.Text
    , datacoverage :: Float
    , resultId :: T.Text
    } deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) = NOAAResult
        <$> v .: "uid"
        <*> v .: "mindate"
        <*> v .: "maxdate"
        <*> v .: "name"
        <*> v .: "datacoverage"
        <*> v .: "id" -- Пишем инстанс сами потому что в ДЖСОНе поле называется id а у нас в коде - resultId

data Resultset = Resultset 
    { offset :: Int
    , count :: Int
    , limit :: Int
    } deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata
    { resultset :: Resultset
    } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
    { metadata :: Metadata
    , results :: [NOAAResult]
    } deriving (Show, Generic)

instance FromJSON NOAAResponse

printResult :: Maybe [NOAAResult] -> IO ()
printResult Nothing = putStrLn "Error"
printResult (Just results) = forM_ results (print.name)

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResult noaaResults
