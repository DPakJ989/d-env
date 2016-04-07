{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( exportEnvVars,
      JavaVersion(Java7, Java8),
      IdeaVersion(Idea14, Idea15)
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

exportEnvVars :: JavaVersion -> IdeaVersion -> [String] -> IO ()
exportEnvVars j i gwArgs = (putStrLn . show . getJavaPath) j >> (putStrLn . show . getIdeaPath) i  

getJavaPath :: JavaVersion -> FilePath
getJavaPath Java7 = "/usr/lib/jvm/java-7-oracle"
getJavaPath Java8 = "/usr/lib/jvm/java-8-oracle"

getIdeaPath :: IdeaVersion -> FilePath
getIdeaPath Idea14 = "/opt/idea14/idea-IU-141.3056.4"
getIdeaPath Idea15 = "/opt/idea15/idea-IU-143.2332.3"

