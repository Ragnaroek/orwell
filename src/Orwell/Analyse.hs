{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
 
module Orwell.Analyse (
   analyseTestContribution,
   analyseTestOwnership,
   commitInfos,
   dumpCommits
) where

import Prelude as P (FilePath)
import Prelude hiding (FilePath)
import Shelly as S (FilePath)
import Shelly (Sh, cd, run, liftIO)
import Data.Text as T (Text, lines, isInfixOf, isPrefixOf, stripPrefix, pack, unpack, append, toLower, splitOn, drop, length, null)
import Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Text.Regex.PCRE.Light
import Control.Monad
import Data.Int
import Data.Time.Exts.Base
import Data.Time.Exts.Unix
import Data.Aeson

import Orwell.CommitMetaData


-- TODO Report Code in eigenes Modul auslagern
-- TODO Git-Code in eigenes Modul auslagern

dumpCommits :: [CommitMetaData] -> String -> IO ()
dumpCommits commits outDir = do 
  mapM (\c -> BL.writeFile (outDir ++ "/" ++ (T.unpack (commitHash c)) ++ ".json") (encode c)) commits
  return ()

commitInfos :: String -> S.FilePath -> Sh [CommitMetaData]
commitInfos period repositoryDir = do
    cd repositoryDir
    commits <- getRelevantCommits $ T.pack period
    mapM extractCommitInfo commits

analyseTestContribution :: [CommitMetaData] -> IO ()
analyseTestContribution infos = do    
    now <- getCurrentUnixDateTime
    let stats = statistics now infos
    -- Ausführlicher Report der letzten Woche
    putStrLn ""
    putStrLn $ "Commits analysiert: " ++ show (L.length infos)
    putStrLn ""
    detailedReport $ statLastWeek stats
    -- Ranking der vorletzten beiden Wochen wiederholen
    intervallReport "vorletzte Woche" $ IM.lookup 1 (statWeekly stats)
    intervallReport "vorvorletzte Woche" $ IM.lookup 2 (statWeekly stats)

    intervallReport "aktueller Monat" $ IM.lookup 0 (statMonthly stats)
    intervallReport "letzter Monat" $ IM.lookup 1 (statMonthly stats)
    
    intervallReport "aktuelles Jahr" $ IM.lookup 0 (statYearly stats)
    intervallReport "letztes Jahr" $ IM.lookup 1 (statYearly stats)

detailedReport :: Maybe DevStatMap -> IO ()
detailedReport m = do
    putStrLn "Report letzte Woche:"
    detailedReportData m

detailedReportData :: Maybe DevStatMap -> IO ()
detailedReportData Nothing = putStrLn "<Keine Commits in der letzten Woche>"
detailedReportData (Just m) = do
    printRankingTests m
    putStrLn ""
    printRankingLines m
    putStrLn ""
    printRankingCommitsWithTest m
    putStrLn ""
    printFeaturesWithoutTest m
    putStrLn ""
    printBugfixesWithoutTest m
    putStrLn ""
    printOtherWithoutTest m
    putStrLn ""
    printCommitsWithoutCode m
    putStrLn ""	
    putStrLn "DEBUG Alle Commits letzte Woche:"
    mapM_ (printCommits (const True)) (M.assocs m)

intervallReport :: String -> Maybe DevStatMap -> IO ()
intervallReport message m = do
    putStrLn ""
    putStrLn $ "Report " ++ message ++ ":"
    intervallReportData m

intervallReportData :: Maybe DevStatMap -> IO ()
intervallReportData Nothing = putStrLn "<Keine Daten vorhanden>"
intervallReportData (Just m) = do
    printRankingTests m
    printRankingLines m

printRankingLines :: DevStatMap -> IO ()
printRankingLines m = printRankingDevs "Ranking Entwickler Zeilen:" "Zeilen" (aggregateTestLines m)

printRankingTests :: DevStatMap -> IO ()
printRankingTests m = printRankingDevs "Ranking Entwickler Tests:" "Test(s)" (aggregateTests m)

analyseTestOwnership :: S.FilePath -> Sh ()
analyseTestOwnership repositoryDir = do
    cd repositoryDir
    cd "./src/test"
    
    javaTests <- grepTests "*.java"
    groovyTests <- grepTests "*.groovy"
    
    let parsed = map (parseToTestContrib.parseGrepOut) (javaTests ++ groovyTests)
    authored <- mapM gitAddLineAuthor parsed
    liftIO $ putStrLn ""
    liftIO $ printRankingDevs "Ranking Anzahl Tests Entwickler summiert:" "Tests" $ M.assocs $ aggregateTestCount authored

grepTests :: Text -> Sh [Text]
grepTests fileType = do
    grep ["-r", "-n", "--include", fileType, ".*@Test.*", "./"]

aggregateTestCount :: [TestContribution] -> M.Map Text Int
aggregateTestCount = foldl' incrementTestCount M.empty

incrementTestCount :: M.Map Text Int -> TestContribution -> M.Map Text Int
incrementTestCount m contrib = M.insert (testContribAuthor contrib) (count + 1) m
    where count = M.findWithDefault 0 (testContribAuthor contrib) m

gitAddLineAuthor :: TestContribution -> Sh TestContribution
gitAddLineAuthor contrib = do
    out <- git ["blame", "-p", "-L", blameLine, T.pack $ testContribFile contrib]
    return $ contrib {
        testContribAuthor = findAuthor out
    } 
    where line = T.pack $ show $ testContribLine contrib
          blameLine = (line `T.append` ",") `T.append` line

-- TODO Die Struktur sollte TestOwner heißen    
data TestContribution = TestContribution {
    testContribFile :: P.FilePath,
    testContribLine :: Int,
    testContribAuthor :: Text
} deriving (Show)

unknownAuthor :: Text
unknownAuthor = "<unknown>"

authorPrefix :: Text
authorPrefix = "author "

findAuthor :: [Text] -> Text
findAuthor = maybe unknownAuthor extractAuthor . L.find (T.isPrefixOf authorPrefix)

extractAuthor :: Text -> Text
extractAuthor = T.drop (T.length authorPrefix) 

parseGrepOut :: Text -> [B.ByteString]
parseGrepOut line = fromJust $ match regex (encodeUtf8 line) []
    where regex = compile "([^:]+):(\\d+):(.+)" []

parseToTestContrib :: [B.ByteString] -> TestContribution
parseToTestContrib raw = TestContribution file line unknownAuthor
    where file = L.drop 2 $ B.unpack $ raw !! 1
          line = read $ B.unpack $ raw !! 2

--
-- Kommandozeilentools
--

pushd :: Text -> Sh ()
pushd folder = do
    run "pushd" [folder]
    return ()

git :: [Text] -> Sh [Text]
git args = do
    out <- run "git" args
    return $ (T.lines out)

grep :: [Text] -> Sh [Text]
grep args = do
    out <- run "grep" args
    return $ (T.lines out)

data CommitType = BUGFIX | FEATURE | OTHER
    deriving (Show, Eq)


getRelevantCommits :: Text -> Sh [Text]
getRelevantCommits backInTime = git ["--no-pager", "log", "--no-merges", "--since=" `append` backInTime, "--format=%h"] 

classify :: CommitMetaData -> CommitType
classify commit | T.isInfixOf "TEC_" subject = FEATURE
                | T.isInfixOf "fix" $ T.toLower $ subject = BUGFIX
                | otherwise = OTHER
    where subject = commitSubject commit


gitLog :: Text -> Text -> Sh Text
gitLog commit format = do
    out <- git ["log", ("--format=" `append` format), "-n", "1", commit]
    return $ head out

gitDiffPrev :: Text -> Text -> Sh [Text]
gitDiffPrev commit filterRegex = git ["diff", ("-G" `append` filterRegex), (commit `append` "^"), commit]

gitHasParent :: Text -> Sh Bool
gitHasParent commit = do
    out <- git ["show", commit, "--format=%P", "--name-only"]
    return $ not $ T.null $ head out

author :: Text -> Sh Text    
author = (flip gitLog) "%an"

testsAdded :: Text -> Sh Int
testsAdded commit = do
   hasParent <- gitHasParent commit
   if hasParent
      then
        countTestsAdded commit
      else 
        return 0 

countTestsAdded :: Text -> Sh Int
countTestsAdded commit = do
    diff <- gitDiffPrev commit "@Test"
    return $ L.length $ filterDiffAddedWith "@Test" diff   

filterDiffAddedWith :: Text -> [Text] -> [Text]
filterDiffAddedWith str = filter (matchAdded str)

matchAdded :: Text -> Text -> Bool
matchAdded str line = T.isPrefixOf "+" line && T.isInfixOf str line

-- Datum des Commit, Unix-Timestamp. Hier wird explizit das committer date genommen
-- da ein author date u.U. weit in der Vergangenheit liegt. Für die zeitliche Einordnung
-- des commits is relevant, wann der Commit ins Repo gewandert ist.
authorDate :: Text -> Sh Text
authorDate = (flip gitLog) "%ct"

subject :: Text -> Sh Text
subject = (flip gitLog) "%s"

files :: Text -> Sh [P.FilePath]
files commit = do 
    out <- git ["show", "--pretty=format:", "--name-only", commit]
    return $ map T.unpack out

fileChanges :: Text -> Sh [FileChange]
fileChanges hash = do
    hasParent <- gitHasParent hash
    if hasParent
      then
        countFileChanges hash
      else
        return [] 

countFileChanges :: Text -> Sh [FileChange]
countFileChanges hash = do
    out <- git ["diff", "--numstat", hash `append` "^", hash ] --wo das ^ steht ist wichtig, sonst sind added und deleted vertauscht
    return $ map (parseToFileChange.parseChange) out

readGitLineChange :: B.ByteString -> Int
readGitLineChange "-" = 0
readGitLineChange s = read $ B.unpack s

parseToFileChange :: [B.ByteString] -> FileChange
parseToFileChange [_,add,del,file] = FileChange (B.unpack file) (readGitLineChange add) (readGitLineChange del)

parseChange :: Text -> [B.ByteString]
parseChange line = fromJust $ match regex (encodeUtf8 line) []
    where regex = compile "(\\d+|-)\\s(\\d+|-)\\s(.+)" []

testFiles :: [FileChange] -> [FileChange]
testFiles = filter ((L.isPrefixOf "src/test").changeFile)

codeFiles :: [FileChange] -> [FileChange]
codeFiles = filter (\file -> (L.isSuffixOf ".groovy" (changeFile file)) || (L.isSuffixOf ".java" (changeFile file)))

extractCommitInfo :: Text -> Sh CommitMetaData
extractCommitInfo hash = do
    a <- author hash
    s <- subject hash
    t <- authorDate hash
    c <- testsAdded hash 
    changes <- fileChanges hash
    return $ CommitMetaData hash s a (read (T.unpack t)) (codeFiles changes) (testFiles (codeFiles changes)) changes c

type DevStatMap = M.Map CommitAuthor [CommitStat]

data Stats = Stats {
    statWeekly :: IM.IntMap DevStatMap,
    statMonthly :: IM.IntMap DevStatMap,
    statYearly :: IM.IntMap DevStatMap
} deriving (Show)

statLastWeek :: Stats -> Maybe DevStatMap
statLastWeek = (IM.lookup 0) . statWeekly

-- TODO accessoren sollten alle mit ct geprefixt werden
data CommitStat = CommitStat {
    commit :: CommitMetaData,
    linesTestAdded :: Int,
    linesCodeAdded :: Int
} deriving (Eq, Ord, Show)

emptyStats = Stats IM.empty IM.empty IM.empty

statistics :: UnixDateTime -> [CommitMetaData] -> Stats
statistics now = foldl (accumulateStats now) emptyStats

accumulateStats :: UnixDateTime -> Stats -> CommitMetaData -> Stats
accumulateStats now s commit = s{statWeekly=appendAtIndex wi (statWeekly s) stat author,
                                 statMonthly=appendAtIndex mi (statMonthly s) stat author,
                                 statYearly=appendAtIndex yi (statYearly s) stat author}
    where wi=weekIndex time now
          mi=monthIndex time now
          yi=yearIndex time now
          stat=(commitStat commit)
          author=(commitAuthor commit)
          time=UnixDateTime $ fromIntegral $ commitTimestamp commit

weekIndex :: UnixDateTime -> UnixDateTime -> Int
weekIndex time now = ((fromEnum now) - (fromEnum time)) `div` (fromIntegral (60 * 60 * 24 * 7))

monthIndex :: UnixDateTime -> UnixDateTime -> Int
monthIndex time now = monthIndexYearOffset yearOffset monthTime monthNow
    where yearOffset = (yearIndex time now)
          monthNow = monthFromTimestamp now
          monthTime = monthFromTimestamp time

monthIndexYearOffset :: Int -> Int -> Int -> Int
monthIndexYearOffset yearOffset monthTime monthNow | yearOffset == 0 = monthNow - monthTime
                                                   | otherwise = (yearOffset * 12 - yearOffset * (monthTime + 1)) + monthNow

monthFromTimestamp :: UnixDateTime -> Int
monthFromTimestamp = fromEnum._dt_mon.toDateTimeStruct

yearIndex :: UnixDateTime -> UnixDateTime -> Int
yearIndex time now = yearNow - yearTime 
    where yearNow = yearFromTimestamp now
          yearTime = yearFromTimestamp time 

yearFromTimestamp :: UnixDateTime -> Int
yearFromTimestamp  = fromIntegral.getYear._dt_year.toDateTimeStruct

appendAtIndex :: Int ->  IM.IntMap DevStatMap -> CommitStat -> CommitAuthor -> IM.IntMap DevStatMap
appendAtIndex i m stat author | value == Nothing = IM.insert i (M.alter (appendStat stat) author M.empty) m
                              | otherwise = IM.insert i (M.alter (appendStat stat) author (fromJust value)) m
    where value=IM.lookup i m 


-- TODO Abstraktion für beiden Funktion, sind sehr ähnlich
aggregateTestLines :: DevStatMap -> [(CommitAuthor, Int)]
aggregateTestLines = M.foldWithKey aggregateAuthorTestLines []

aggregateAuthorTestLines :: CommitAuthor -> [CommitStat] -> [(CommitAuthor,Int)] -> [(CommitAuthor,Int)]
aggregateAuthorTestLines a ss = insert (a, sumTestLinesCommits ss)

aggregateTests :: DevStatMap -> [(CommitAuthor, Int)]
aggregateTests = M.foldWithKey aggregateAuthorTests []

aggregateAuthorTests :: CommitAuthor -> [CommitStat] -> [(CommitAuthor,Int)] -> [(CommitAuthor,Int)]
aggregateAuthorTests a ss = insert (a, sumTestsCommits ss)

sumTestsCommits :: [CommitStat] -> Int
sumTestsCommits = foldl (\i s -> i +  (commitTestsAdded (commit s))) 0

sumTestLinesCommits :: [CommitStat] -> Int
sumTestLinesCommits = foldl (\i s -> i + (linesTestAdded s)) 0

appendStat :: CommitStat -> Maybe [CommitStat] -> Maybe [CommitStat] 
appendStat s Nothing = Just [s]
appendStat s (Just l) = Just $ insert s l

commitStat :: CommitMetaData -> CommitStat
commitStat c = CommitStat c (sumTest c) (sumCode c)

sumTest :: CommitMetaData -> Int
sumTest = sumAdded commitTestFiles

sumCode :: CommitMetaData -> Int
sumCode = sumAdded commitCodeFiles

sumAdded :: (CommitMetaData -> [FileChange]) -> CommitMetaData -> Int
sumAdded g c = foldl (\s change-> s + (changeAdded change)) 0 (g c)

--- 
--- Reporting
--- 

maxHashesLineDiagram :: Int
maxHashesLineDiagram = 50

printRankingDevs :: Text -> Text -> [(Text, Int)] -> IO ()
printRankingDevs description datumDescr aggregate = do
    putTxt description
    putStrLn ""
    mapM_ (printRankingDev datumDescr (maxVal sorted)) sorted
    where sorted = sortBy compareNumVal aggregate

compareNumVal :: (Text, Int) -> (Text, Int) -> Ordering
compareNumVal a b = compare (snd b) (snd a)

-- Nimmt an, dass die übergebene Liste nach dem Int-Wert aufsteigend sortiert ist
maxVal :: [(Text, Int)] -> Int
maxVal (x:xs) = snd x

printRankingDev :: Text -> Int -> (Text, Int) -> IO ()
printRankingDev description m d = do
    printLineDiagram m $ snd d
    printLegend description d

printLegend :: Text -> (Text, Int) -> IO ()
printLegend datumDescription d = do
    putStr " "
    putStr $ show $ snd d
    putStr " "
    putTxt datumDescription
    putStr " "
    printAuthor $ fst d
    putStrLn ""

printAuthor :: Text -> IO ()
printAuthor a = do
    putStr "("
    putTxt a
    putStr ")"

printLineDiagram :: Int -> Int -> IO ()
printLineDiagram m p = do
    putStr "["
    putStr $ replicate hashes '#'
    putStr $ replicate (maxHashesLineDiagram - hashes) ' '
    putStr "]"
    where hashes = (round (((fromIntegral maxHashesLineDiagram) / (fromIntegral m)) * (fromIntegral p)))

printFeaturesWithoutTest = printWithoutTestOfType FEATURE
printBugfixesWithoutTest = printWithoutTestOfType BUGFIX
printOtherWithoutTest = printWithoutTestOfType OTHER

printWithoutTestOfType :: CommitType -> DevStatMap -> IO ()
printWithoutTestOfType t m = do
    putStr $ show t
    putStrLn " ohne Test:"
    mapM_ (printCommits (filterTypeNoTest t)) (M.assocs m) 

filterTypeNoTest :: CommitType -> CommitStat -> Bool
filterTypeNoTest t s = (classify c) == t && (commitTestFiles c) == [] && (commitCodeFiles c) /= []
    where c = (commit s)

printCommitsWithoutCode :: DevStatMap -> IO ()
printCommitsWithoutCode m = do
    putStrLn "Commits ohne Code:"
    mapM_ (printCommits filterNoCode) (M.assocs m) 

filterNoCode :: CommitStat -> Bool
filterNoCode s = (commitCodeFiles (commit s)) == []

filterWithTest :: CommitStat -> Bool
filterWithTest s = (commitTestFiles (commit s)) /= []

printCommits :: (CommitStat -> Bool) -> (CommitAuthor, [CommitStat]) -> IO ()
printCommits p d = do
    mapM_ printCommit $ filter p $ snd d
    putStr ""

printRankingCommitsWithTest :: DevStatMap -> IO ()
printRankingCommitsWithTest m = do
    putStrLn "Ranking Commits (Testzeilen absolut):"
    mapM_ (printRankingCommitAbsoluteTestLines $ maxTestLines rankedCommits) rankedCommits
    where rankedCommits = rankCommitsWithTest m

maxTestLines :: [CommitStat] -> Int
maxTestLines = linesTestAdded . head 
    
rankCommitsWithTest :: DevStatMap -> [CommitStat]
rankCommitsWithTest m = sortBy compareTestLinesAdded commitsWithTest
    where commitsWithTest = filter filterWithTest $ concat $ M.elems $ m

printRankingCommitAbsoluteTestLines :: Int -> CommitStat -> IO ()
printRankingCommitAbsoluteTestLines m s = do
    printLineDiagram m (linesTestAdded s)
    printLegendAbsoluteTestLines s
    putStrLn ""

printLegendAbsoluteTestLines :: CommitStat -> IO ()
printLegendAbsoluteTestLines c = do
    putStr " "
    putStr $ show $ linesTestAdded c
    putStr " Zeilen - "
    putTxt $ commitAuthor $ commit c
    putStr ": "
    putTxt $ commitSubject $ commit c

compareTestLinesAdded :: CommitStat -> CommitStat -> Ordering
compareTestLinesAdded a b = compare (linesTestAdded b) (linesTestAdded a) 

printCommit :: CommitStat -> IO ()
printCommit c = do
    putTxt $ commitHash $ commit c
    putStr " "
    putTxt $ commitSubject $ commit c
    printAuthor $ commitAuthor $ commit c
    putStrLn ""

putTxt :: Text -> IO ()
putTxt = putStr . T.unpack

{-
TODO: 
Ranking Commits (Anteil Testzeilen)
<commit>          [#############################] xxx %
<commit>          [#############                ] yyy %

TODO: Anzahl Tests über Scan der @Test-Annotation ermitteln
-}

