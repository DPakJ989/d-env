module Main where

import Lib
import Options.Applicative

main :: IO ()
main = exportEnvVars

javaVersion :: Parser String
javaVersion = strOption . long $ "java"

ideaVersion :: Parser String
ideaVersion = strOption . long $ "idea"

gwbCommands :: Parser [String]
gwbCommands = some (strArgument . idm $ "gwbOptions")


