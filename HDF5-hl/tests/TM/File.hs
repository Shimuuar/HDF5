{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module TM.File (tests) where

import Control.Monad
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp
import System.FilePath   ((</>))
import GHC.Generics      (Generic,Generically(..))
import HDF5.HL           qualified as H5
import HDF5.HL.Attribute qualified as H5

tests :: TestTree
tests = testGroup "Files"
  [ testCase "Create/open file" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \_ -> pure ()
      H5.withOpenFile   path H5.OpenRO      $ \_ -> pure ()
    --
  , testCase "Write/read dataset" $ withDir $ \dir -> do
      let path   = dir </> "test.h5"
          arr    = [1 .. 10] :: [Int]
          scalar = 123 :: Double
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.writeAllAt h5 "dset1" [] arr
        H5.writeAllAt h5 "dset2" [] scalar
      H5.withOpenFile   path H5.OpenRO      $ \h5 -> do
        assertEqual "Array"  arr    =<< H5.readAllAt h5 "dset1"
        assertEqual "Scalar" scalar =<< H5.readAllAt h5 "dset2"
    --
  , testCase "Write/read slabs" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
          arr  = [0..10] :: [Int]
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.writeAllAt h5 "dset1" [] arr
        H5.withOpenDataset h5 "dset1" $ \dset -> do
          assertEqual "Slice" [3,4,5] =<< H5.readSlab @[Int] dset 3 3
          H5.writeSlab dset 3 [30,40,50::Int]
          assertEqual "After write" [0,1,2,30,40,50,6,7,8,9,10] =<< H5.readAll @[Int] dset
    --
  , testCase "Write/read attribute (root)" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
          val  = 123.25 :: Double
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.writeAttr h5 "attr" val
      H5.withOpenFile   path H5.OpenRO      $ \h5 -> do
        assertEqual "attribute"   (Just val)        =<< H5.readAttrMay h5 "attr"
        assertEqual "attribute"   (Just (123::Int)) =<< H5.readAttrMay h5 "attr"
        assertEqual "Nonexistent" (Nothing @Int)    =<< H5.readAttrMay h5 "attr-X"
    --
  , testCase "Resize/chunking" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.withCreateEmptyDataset h5 "dset"
          (H5.typeH5 @Int)
          (H5.Growable (0::Int) H5.UNLIMITED)
          [H5.propDatasetChunking (10::Int)] $ \dset -> do
            -- 1-st resize
            H5.setDatasetExtent dset (10::Int)
            assertEqual "Zero" (replicate 10 0) =<< H5.readAll @[Int] dset
            -- 1-st write
            H5.writeSlab dset 0 (take 10 [0::Int ..])
            assertEqual "Zero" [0..9] =<< H5.readAll @[Int] dset
            -- 2-nd resize & write
            H5.setDatasetExtent dset (20::Int)
            H5.writeSlab dset 10 (take 10 [100::Int ..])
            assertEqual "Zero" ([0..9]++[100..109]) =<< H5.readAll @[Int] dset
    --
  , testCase "delete" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        let dset = [1..10]::[Int]
        H5.writeAllAt h5 "foo" [] dset
        assertEqual "dataset" dset =<< H5.readAllAt @[Int] h5 "foo"
        H5.delete h5 "foo"
        shouldThrowH5 "dataset should be deleted" $ void $ H5.readAllAt @[Int] h5 "foo"
    --
  , testCase "pathIsValid" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        assertEqual "" False =<< H5.pathIsValid h5 "foo"  True
        assertEqual "" False =<< H5.pathIsValid h5 "/foo" True
        H5.writeAllAt h5 "foo" [] ([1..10]::[Int])
        assertEqual "" True =<< H5.pathIsValid h5 "foo"  True
        assertEqual "" True =<< H5.pathIsValid h5 "/foo" True
    -- 
  , testCase "Attributes" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        let x = Foo{foo=11, bar=Bar 22 33}
        H5.writeAttributes h5 x
        x' <- H5.readAttributes h5
        assertEqual "" x x'
        assertEqual "foo"    (Just 11) =<< H5.readAttrMay @Int h5 "foo"
        assertEqual "bar/a1" (Just 22) =<< H5.readAttrMay @Int h5 "bar/a1"
        assertEqual "bar/a2" (Just 33) =<< H5.readAttrMay @Int h5 "bar/a2"
  ]

withDir :: (FilePath -> IO a) -> IO a
withDir = withSystemTempDirectory "HDF5"

shouldThrowH5 :: String -> IO () -> IO ()
shouldThrowH5 msg io
  = (io >> assertFailure ("Show raise exception: "++msg))
  `catch`
    (\(_::H5.Error) -> pure ())



data Foo = Foo
  { foo :: Int
  , bar :: Bar
  }
  deriving stock (Eq,Show,Generic)
  deriving H5.SerializeAttr via Generically Foo

data Bar = Bar
  { a1 :: Int
  , a2 :: Int
  }
  deriving stock (Eq,Show,Generic)
  deriving H5.SerializeAttr via Generically Bar
