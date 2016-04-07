module Main where

import RunIdea 
import Options.Applicative
import Data.Maybe
import Data.Text

data CommandLineArgs = CommandLineArgs 
    { maybeJavaVersion :: Maybe JavaVersion
    , maybeIdeaVersion :: Maybe IdeaVersion 
    , gwbArgs     :: [Text] }

main :: IO ()
main = do
  args <- execParser (info commandLineArgsParser idm) 
  let(jv, iv, ga) = validate args
  exportEnvVars jv iv ga

validate :: CommandLineArgs -> (JavaVersion, IdeaVersion, [Text]) 
validate args = case (maybeJavaVersion args, maybeIdeaVersion args, gwbArgs args) of
    (Nothing, _, _)         -> error "Invalid Java version specified"
    (_, Nothing, _)         -> error "Invalid Idea version specified"
    (Just jv, Just iv, ga)  -> (jv, iv, ga)

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

gwbArgsParser :: Parser [Text]
gwbArgsParser = some $ pack <$> strArgument idm

commandLineArgsParser :: Parser Main.CommandLineArgs
commandLineArgsParser = CommandLineArgs <$> javaVersionParser <*> ideaVersionParser <*> gwbArgsParser
