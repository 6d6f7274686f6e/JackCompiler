module Main where

import qualified Jack.Types as Jack
import Jack.Parse

import qualified VM.Types as VM
import VM.ASM

import Jack2VM

import System.IO
import System.FilePath
import System.Environment
import System.Directory
import Control.Monad

main :: IO ()
main = putStrLn "This program is meant to run in GHCi for now."

compileFolder :: FilePath -> FilePath -> IO ()
compileFolder source dest = flip jackFiles2ASMFile dest =<< sources
  where sources = map (source ++) . filter (\s -> takeExtension s == ".jack") <$> getDirectoryContents source

compileFolder2VMFolder :: FilePath -> FilePath -> IO ()
compileFolder2VMFolder source dest = flip jackFiles2VMFolder dest =<< sources
  where sources = map (source ++) . filter (\s -> takeExtension s == ".jack") <$> getDirectoryContents source

jackFiles2ASMFile :: [FilePath] -> FilePath -> IO ()
jackFiles2ASMFile sources dest = writeFile dest =<< concat <$> (forM sources $ \source -> do
                                   s <- readFile source
                                   let vmresult = VM.programWrite . class2VM <$> parseJack s
                                       result   = vm2ASM (sanitizeFilename source) <$> vmresult
                                   case result of
                                     Left  err -> error $ "Parse error in file " ++ source ++ " : " ++ show err
                                     Right asm -> return asm)

jackFiles2VMFolder :: [FilePath] -> FilePath -> IO ()
jackFiles2VMFolder sources dest = forM_ sources $ \source -> 
  jackFile2VMFile source $ replaceExtension (replaceDirectory source dest) "vm"

jackFile2VMFile :: FilePath -> FilePath -> IO ()
jackFile2VMFile source dest = do s <- readFile source 
                                 let result = VM.programWrite . class2VM <$> parseJack s
                                 case result of
                                   Left err -> error $ "Parse error : " ++ show err
                                   Right vm -> writeFile dest vm
