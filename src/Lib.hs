{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( exportEnvVars
    ) where

import Turtle
import Data.Map as Map
import Data.Maybe
import Prelude hiding (FilePath)

data JavaVersion 
    = Java7 
    | Java8
    deriving (Eq, Ord)

data IdeaVersion
    = Idea14
    | Idea15
    deriving (Eq, Ord)

exportEnvVars :: IO ()
exportEnvVars = putStrLn . show . fromJust . Map.lookup Java8 $ javaPathMap  

javaPathMap :: Map JavaVersion FilePath
javaPathMap = Map.fromList 
    [ (Java7, "/usr/lib/jvm/java-7-oracle")
    , (Java8, "/usr/lib/jvm/java-8-oracle") ]

ideaPathMap :: Map IdeaVersion FilePath
ideaPathMap = Map.fromList
    [ (Idea14, "/opt/idea14/idea-IU-141.3056.4")
    , (Idea15, "/opt/idea15/idea-IU-143.2332.3")]  

