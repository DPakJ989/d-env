{-# LANGUAGE OverloadedStrings #-}

module RunIdea
    ( exportEnvVars,
      JavaVersion(Java7, Java8),
      IdeaVersion(Idea14, Idea15)
    ) where

import Turtle
import Data.Maybe
import Prelude hiding (FilePath)

data JavaVersion
    = Java7
    | Java8

data IdeaVersion
    = Idea14
    | Idea15

exportEnvVars :: JavaVersion -> IdeaVersion -> [Text] -> IO ()
exportEnvVars j i gwArgs = do
    echo "Setting environment variables.."
    export "JAVA_HOME" $ getJavaPath j
    export "IDEA_HOME" $ getIdeaPath i
    echo $ "export PATH=$PATH:" <> getIdeaPath i <> "/bin"
    envVars <- env
    printVar "JAVA_HOME" envVars
    printVar "IDEA_HOME" envVars
    printVar "PATH" envVars
    dir <- pwd
    cd dir
    exitCode <- proc "./gwb" gwArgs empty
    print exitCode
    where
      printVar :: Text -> [(Text, Text)] -> IO ()
      printVar a l = echo $ case lookup a l of
          Just x -> a <> "=" <> x
          Nothing -> "Variable " <> a <> " doesn't appear to be set"

getJavaPath :: JavaVersion -> Text
getJavaPath Java7 = "/usr/lib/jvm/java-7-oracle"
getJavaPath Java8 = "/usr/lib/jvm/java-8-oracle"

getIdeaPath :: IdeaVersion -> Text
getIdeaPath Idea14 = "/opt/idea14/idea-IU-141.3056.4"
getIdeaPath Idea15 = "/opt/idea15/idea-IU-143.2370.31"

