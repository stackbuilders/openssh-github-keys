module System.OpensshGithubKeys.Config (options, readDefaultValues) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Exception.Base         (handle, throw)
import           Data.KeywordArgs.Parse         (configParser)
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid                    (mconcat, (<>))
import           Options.Applicative            (InfoMod, Mod, OptionFields,
                                                 Parser, ParserInfo, argument,
                                                 fullDesc, header, help, helper,
                                                 info, long, metavar, optional,
                                                 progDesc, short, some, str,
                                                 strOption, value)
import           System.IO.Error                (isDoesNotExistError)
import           System.OpensshGithubKeys.Types (Options (..))
import           Text.Parsec                    (ParseError, parse)

options :: [(String, String)] -> ParserInfo Options
options d = info (helper <*> optionsParser d) infoMod
  where
    infoMod :: InfoMod a
    infoMod = mconcat [ fullDesc
                      , progDesc desc
                      , header "openssh-github-keys - fetches team member keys from GitHub"
                      ]

    desc :: String
    desc = unlines [ "Fetches ssh public keys from TEAMS under ORGANIZATION on GitHub."
                   , "The output format is suitable for AuthorizedKeysCommand in"
                   , "sshd_config. Requires a github token, which can be specified"
                   , "in an environment variable GITHUB_TOKEN or in a dotenv file which"
                   , "can be specified with the -f option."
                   ]

optionsParser :: [(String, String)] -> Parser Options
optionsParser v = Options <$> authenticateArgument
                  <*> organizationOption (lookup "organization" v)
                  <*> teamOption (lookup "team" v)
                  <*> usersOption
                  <*> dotfileOption (lookup "dotfile" v)

authenticateArgument :: Parser String
authenticateArgument = argument str $ metavar "AUTHENTICATE" <> help "Local user trying to authenticate currently"

organizationOption :: Maybe String -> Parser String
organizationOption v = optionWithDefaultValue v [ long "organization"
                                                , short 'o'
                                                , metavar "ORGANIZATION"
                                                , help "GitHub organization from which to select teams"
                                                ]


teamOption :: Maybe String -> Parser String
teamOption v = optionWithDefaultValue v [ long "team"
                                        , short 't'
                                        , metavar "TEAM"
                                        , help "GitHub team from which to select members' keys"
                                        ]

usersOption :: Parser [String]
usersOption = some userOption

userOption :: Parser String
userOption = optionConcat [ long "user"
                          , short 'u'
                          , metavar "USER"
                          , help "A local user that we should try to authenticate using Github"
                          ]

dotfileOption :: Maybe String -> Parser (Maybe String)
dotfileOption v = optional $ optionWithDefaultValue v [ long "dotfile"
                                                      , short 'f'
                                                      , metavar "DOTFILE"
                                                      , help "File in 'dotfile' format specifying GITHUB_TOKEN"
                                                      ]

optionWithDefaultValue :: Maybe String -> [Mod OptionFields String] -> Parser String
optionWithDefaultValue Nothing  xs = optionConcat xs
optionWithDefaultValue (Just v) xs = strOption $ mconcat xs <> value v

optionConcat :: [Mod OptionFields String] -> Parser String
optionConcat = strOption . mconcat

readDefaultValues :: FilePath -> IO [(String, String)]
readDefaultValues fp = handle readHandler $ do
  content <- readFile fp
  case parseContent content of
    Left e  -> error $ concat ["Unable to parse file \"" , fp ,"\" cause: ",  show e]
    Right c -> return $ toSingleValueMap c

readHandler :: IOError -> IO [(String, String)]
readHandler e
      | isDoesNotExistError e = return []
      | otherwise             = throw e

toSingleValueMap :: [(String, [String])] -> [(String, String)]
toSingleValueMap = mapMaybe toSingleValue
  where
    toSingleValue :: (String, [String]) -> Maybe (String, String)
    toSingleValue (_, [])  = Nothing
    toSingleValue (k, v:_) = Just (k, v)

parseContent :: String -> Either ParseError [(String, [String])]
parseContent = parse configParser "(openssh-github-keys)"
