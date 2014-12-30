module Main
where

import Prelude hiding (FilePath)
import Orwell.Analyse
import Data.Maybe
import Shelly hiding (find)
import Control.Monad
import System.Console.GetOpt
import System.Environment
import Data.List as L
import Data.Text as T (Text, pack)

data Flag = TestOwner | TestContrib | 
    DumpCommits {
        dumpOut :: Maybe String
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
    
    when (optionDump opts) $ do
        liftIO $ dumpCommits commits (optionDumpOut opts)

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
    , Option ['d'] ["dump"] (OptArg DumpCommits "DIR") "Dumpt die commits als JSON in das aktuelle Verzeichnis"
    , Option ['o'] ["ownership"] (NoArg TestOwner) "Ermittelt die Anzahl Tests und deren Eigentümer im aktuellen HEAD"
    , Option ['c'] ["contribution"] (NoArg TestContrib) "Ermittelt die Beiträge pro Entwickler{Woche, Monat, Jahr} zum Testcode ab Zeitpunkt SINCE (Git Syntax)"
    ]
    
optionOwner :: [Flag] -> Bool
optionOwner = any (== TestOwner)

optionContrib :: [Flag] -> Bool
optionContrib = any (== TestContrib)

optionDump :: [Flag] -> Bool
optionDump = any dumpOutArgP

optionDumpOut :: [Flag] -> String
optionDumpOut = fromJust.dumpOut.fromJust.(L.find dumpOutArgP)

optionPeriod :: [Flag] -> String
optionPeriod = period.fromJust.(L.find periodArgP)

optionRepo :: [Flag] -> FilePath
optionRepo = fromText.repoArg.fromJust.(L.find repoArgP)

periodArg :: Flag -> String
periodArg (Period p) = p

dumpOutArgP :: Flag -> Bool
dumpOutArgP (DumpCommits _) = True
dumpOutArgP _ = False

dumpOutArg :: Flag -> String
dumpOutArg (DumpCommits (Just d)) = d

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

