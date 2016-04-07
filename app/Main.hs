module Main where

import Lib 
import Options.Applicative
import Data.Maybe

data CommandLineArgs = CommandLineArgs 
    { maybeJavaVersion :: Maybe JavaVersion
    , maybeIdeaVersion :: Maybe IdeaVersion 
    , gwbArgs     :: [String] }

main :: IO ()
main = do
  args <- execParser (info commandLineArgsParser idm) 
  validate args

validate :: CommandLineArgs -> IO () 
validate args
    | maybeJavaVersion args == Nothing    = putStrLn ("Error: Invalid Java version specified")
    | maybeIdeaVersion args == Nothing    = putStrLn ("Error: Invalid Idea version specified")
    | otherwise                           = exportEnvVars (fromJust . maybeJavaVersion $ args) (fromJust . maybeIdeaVersion $ args) (gwbArgs $ args)

javaVersionParser :: Parser (Maybe JavaVersion)
javaVersionParser = parseJavaVersion <$> (strOption . long $ "java")
  where
    parseJavaVersion :: String -> Maybe JavaVersion
    parseJavaVersion s = case s of 
      "8" -> Just Java8
      "7" -> Just Java7
      _   -> Nothing

ideaVersionParser :: Parser (Maybe IdeaVersion)
ideaVersionParser = parseIdeaVersion <$> (strOption . long $ "idea")
  where 
    parseIdeaVersion :: String -> Maybe IdeaVersion
    parseIdeaVersion s = case s of 
      "14" -> Just Idea14
      "15" -> Just Idea15
      _    -> Nothing

gwbArgsParser :: Parser [String]
gwbArgsParser = some (strArgument . idm $ "gwbArgs")

commandLineArgsParser :: Parser Main.CommandLineArgs
commandLineArgsParser = CommandLineArgs <$> javaVersionParser <*> ideaVersionParser <*> gwbArgsParser
