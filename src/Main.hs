module Main
where

import Prelude hiding (FilePath)
import Orwell.Analyse
import Orwell.Elastic
import Data.Maybe
import Shelly hiding (find)
import Control.Monad
import System.Console.GetOpt
import System.Environment
import Data.List as L
import Data.Text as T (Text, pack)

data Flag = TestOwner | TestContrib |
    ElasticUrlArg {
        url :: ElasticServerUrl
    } |
    Period {
        period :: String
    } |
    Repository {
        repo :: String
    }
    deriving (Show, Eq)

main = shelly $ silently $ do
    args <- liftIO $ getArgs
    opts <- liftIO $ orwellOpts args

    commits <- commitInfos (optionPeriod opts) (optionRepo opts)

    when (optionElastic opts) $ do
        liftIO $ mapM_ (submitCommitMetaData (optionElasticUrl opts)) commits

    when (optionContrib opts) $ do
        liftIO $ analyseTestContribution commits

    when (optionOwner opts) $ do
        analyseTestOwnership (optionRepo opts)

{-
Command-Line Options
-}
orwellOpts :: [String] -> IO [Flag]
orwellOpts args =
    case getOpt Permute options args of
          (o,[],[]  ) -> return o
          (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: orwell [OPTION...]"

options :: [OptDescr Flag]
options =
    [ Option ['p'] ["period"] (ReqArg Period "PERIOD") "Gibt den Zeitraum an, der analysiert werden soll (Git Syntax)"
    , Option ['r'] ["repo"] (ReqArg Repository "REPO") "Das zu analysierende Repository"
    , Option ['e'] ["elastic"] (ReqArg ElasticUrlArg "URL") "Überträgt die Commit-Daten zusätzlich an den angegebenen Elastic-Server"
    , Option ['o'] ["ownership"] (NoArg TestOwner) "Ermittelt die Anzahl Tests und deren Eigentümer im aktuellen HEAD"
    , Option ['c'] ["contribution"] (NoArg TestContrib) "Ermittelt die Beiträge pro Entwickler{Woche, Monat, Jahr} zum Testcode für den angegeben Zeitraum (PERIOD)"
    ]

optionOwner :: [Flag] -> Bool
optionOwner = any (== TestOwner)

optionContrib :: [Flag] -> Bool
optionContrib = any (== TestContrib)

optionElastic :: [Flag] -> Bool
optionElastic = any elasticUrlArgP

optionElasticUrl :: [Flag] -> String
optionElasticUrl = elasticUrlArg.fromJust.(L.find elasticUrlArgP)

optionPeriod :: [Flag] -> String
optionPeriod = period.fromJust.(L.find periodArgP)

optionRepo :: [Flag] -> FilePath
optionRepo = fromText.repoArg.fromJust.(L.find repoArgP)

periodArg :: Flag -> String
periodArg (Period p) = p

elasticUrlArgP :: Flag -> Bool
elasticUrlArgP (ElasticUrlArg _) = True
elasticUrlArgP _ = False

elasticUrlArg :: Flag -> ElasticServerUrl
elasticUrlArg (ElasticUrlArg d) = d

periodArgP :: Flag -> Bool
periodArgP (Period _) = True
periodArgP _ = False

repoArg :: Flag -> T.Text
repoArg (Repository r) = T.pack r

repoArgP :: Flag -> Bool
repoArgP (Repository _) = True
repoArgP _ = False

{-
END Command-Line Options
-}
