module Main where

import Lib as L
import Options.Applicative

data CommandLineArgs = CommandLineArgs 
    { maybeJavaVersion :: Maybe L.JavaVersion
    , maybeIdeaVersion :: Maybe L.IdeaVersion 
    , gwbArgs     :: [String] }

main :: IO ()
main = execParser (info commandLineArgs idm) >>= validate

validate :: Main.CommandLineArgs -> IO () 
validate args
    | maybeJavaVersion args == Nothing    = putStrLn ("Error: Invalid Java version specified")
    | maybeIdeaVersion args == Nothing    = putStrLn ("Error: Invalid Idea version specified")
    | otherwise                           = putStrLn ("Setting environment variables ...")

javaVersion :: Parser (Maybe L.JavaVersion)
javaVersion = parseJavaVersion <$> (strOption . long $ "java")
  where
    parseJavaVersion :: String -> Maybe L.JavaVersion
    parseJavaVersion s = case s of 
      "8" -> Just L.Java8
      "7" -> Just L.Java7
      _   -> Nothing

ideaVersion :: Parser (Maybe L.IdeaVersion)
ideaVersion = parseIdeaVersion <$> (strOption . long $ "idea")
  where 
    parseIdeaVersion :: String -> Maybe L.IdeaVersion
    parseIdeaVersion s = case s of 
      "14" -> Just L.Idea14
      "15" -> Just L.Idea15
      _    -> Nothing

gwbCommands :: Parser [String]
gwbCommands = some (strArgument . idm $ "gwbArgs")

commandLineArgs :: Parser Main.CommandLineArgs
commandLineArgs = Main.CommandLineArgs <$> Main.javaVersion <*> Main.ideaVersion <*> gwbCommands
