
module Orwell.Elastic (
  submitCommitMetaData,
  testCommit,
  ElasticServerUrl
) where


import Data.Text
import Orwell.CommitMetaData
import Database.Bloodhound.Types
import Database.Bloodhound.Client


type ElasticServerUrl = String

testCommit = CommitMetaData (pack "abchash") (pack "subject") (pack "Author") 1419958228 [] [] [] 42  

-- TODO Elastic-Server parametrisieren

submitTestDoc :: IO ()
submitTestDoc = do
   reply <- indexDocument (Server "http://statistics.toxine.lan:9200") (IndexName "orwell") (MappingName "commit") testCommit (DocId "abchash")
   print reply

submitCommitMetaData :: ElasticServerUrl -> CommitMetaData -> IO ()
submitCommitMetaData url commit = do
   reply <- indexDocument (Server url) (IndexName "orwell") (MappingName "commit") commit (DocId $ unpack $ commitHash commit)
   return ()
