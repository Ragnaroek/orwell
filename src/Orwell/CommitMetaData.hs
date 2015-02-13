{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Orwell.CommitMetaData (
  CommitMetaData(..),
  FileChange(..),
  CommitAuthor,
) where

import Data.Text
import Prelude as P (FilePath)
import Prelude hiding (FilePath)
import Data.Aeson

type CommitAuthor = Text

data FileChange = FileChange {
    changeFile :: P.FilePath,
    changeAdded :: Int,
    changeDeleted :: Int
} deriving (Eq, Ord, Show)

data CommitMetaData = CommitMetaData {
    commitHash :: Text,
    commitSubject :: Text,
    commitAuthor :: CommitAuthor,
    commitTimestamp :: Int, 
    commitCodeFiles :: [FileChange],
    commitTestFiles :: [FileChange],
    commitChanges :: [FileChange],
    commitTestsAdded :: Int
} deriving (Eq, Ord, Show)


instance ToJSON CommitMetaData where
   toJSON (CommitMetaData {..}) = object
    [ "commitAuthor" .= commitAuthor,
      "commitHash"   .= commitHash,
      "commitTimestamp" .= (commitTimestamp * 1000),
      "commitTestsAdded" .= commitTestsAdded
    ]

