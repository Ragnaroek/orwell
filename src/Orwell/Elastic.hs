
module Orwell.Elastic (
  submitCommitMetaData,
  testCommit,
  ElasticServerUrl
) where


import Data.Text
import Orwell.CommitMetaData
import Database.Bloodhound.Types
import Database.Bloodhound.Client
import Network.HTTP.Client


type ElasticServerUrl = String

testCommit = CommitMetaData (pack "abchash") (pack "subject") (pack "Author") 1419958228 [] [] [] 42

-- TODO Elastic-Server parametrisieren

--submitTestDoc :: IO ()
--submitTestDoc = do
--   withBH
--   reply <- indexDocument (Server "http://statistics.toxine.lan:9200") (IndexName "orwell") (MappingName "commit") testCommit (DocId "abchash")
--   print reply

submitCommitMetaData :: ElasticServerUrl -> CommitMetaData -> IO ()
submitCommitMetaData url commit = do
   reply <- withBH defaultManagerSettings (Server (pack url)) $ indexDocument (IndexName (pack "orwell")) (MappingName (pack "commit")) defaultIndexDocumentSettings commit (DocId $ commitHash commit)
   return ()
