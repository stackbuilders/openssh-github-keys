module System.OpensshGithubKeys.Config (options, readFileOptions) where

import           Control.Applicative            (pure, (<$>), (<*>), (<|>))
import           Control.Exception.Base         (handle, throw)
import           Data.KeywordArgs.Parse         (configParser)
import           Data.Monoid                    (mconcat, (<>))
import           Options.Applicative            (InfoMod, Mod, OptionFields,
                                                 Parser, ParserInfo, argument,
                                                 fullDesc, header, help, helper,
                                                 info, long, metavar, progDesc,
                                                 short, some, str, strOption,
                                                 value)
import           System.IO.Error                (isDoesNotExistError)
import           System.OpensshGithubKeys.Types (FileOptions (..), Options (..),
                                                 emptyFileOptions,
                                                 mapToFileOptions)
import           Text.Parsec                    (ParseError, parse)

options :: FileOptions -> ParserInfo Options
options fo = info (helper <*> optionsParser fo) infoMod
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

optionsParser :: FileOptions -> Parser Options
optionsParser fo = Options <$> authenticateArgument
                    <*> organizationOption (fileOptionsOrganization fo)
                    <*> teamOption (fileOptionsTeam fo)
                    <*> usersOption (fileOptionsUsers fo)
                    <*> dotfileOption

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

usersOption :: Maybe [String] -> Parser [String]
usersOption Nothing   = some userOption
usersOption (Just xs) = some userOption <|> pure xs

userOption :: Parser String
userOption = optionConcat [ long "user"
                          , short 'u'
                          , metavar "USER"
                          , help "A local user that we should try to authenticate using Github"
                          ]

dotfileOption :: Parser String
dotfileOption = optionConcat [ long "dotfile"
                             , short 'f'
                             , metavar "DOTFILE"
                             , help "File in 'dotfile' format specifying GITHUB_TOKEN"
                             , value "/etc/openssh-github-keys/github.creds"
                             ]

optionWithDefaultValue :: Maybe String -> [Mod OptionFields String] -> Parser String
optionWithDefaultValue Nothing  xs = optionConcat xs
optionWithDefaultValue (Just v) xs = strOption $ mconcat xs <> value v

optionConcat :: [Mod OptionFields String] -> Parser String
optionConcat = strOption . mconcat

readFileOptions :: FilePath -> IO FileOptions
readFileOptions fp = handle readHandler $ do
  content <- readFile fp
  case parseContent content of
    Left e  -> error $ concat ["Unable to parse file \"" , fp ,"\" cause: ",  show e]
    Right c -> return $ mapToFileOptions c

readHandler :: IOError -> IO FileOptions
readHandler e
      | isDoesNotExistError e = return emptyFileOptions
      | otherwise             = throw e

parseContent :: String -> Either ParseError [(String, [String])]
parseContent = parse configParser "(openssh-github-keys)"
