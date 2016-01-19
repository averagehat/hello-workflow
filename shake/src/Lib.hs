{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( block
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Text.Regex.Posix
import Data.String.Here
import Control.Monad
import Control.Applicative
-- To run:
-- stack setup
-- stack exec shell-exe
block :: IO ()
-- FilePath is an alias for string

matching str strings = (filter (=~ str) strings) :: [String] 

groupFastqs fqs = (unp, fwd, rev) 
  where
    unp = matching "^((?!_R1_|_R2_).)+$"  fqs
    fwd = matching "_R1_" fqs
    rev = matching "_R2_" fqs


mapPairedUnpaired :: (FilePath -> FilePath -> Action ()) -> (FilePath -> Action ()) -> [FilePath] -> (Action (), Action ())
mapPairedUnpaired pF unpF fqs = ((zipWithM_ pF fwd rev), mapM_ unpF unp) 
  where (unp, fwd, rev) =  groupFastqs fqs

unpCutAdapt fq = unit $ cmd "cutadapt" ["-a", "AACCGGTT", "-o", fq <.> "cutadapt" , fq ] 
pCutAdapt fwd rev = unit $ cmd "cutadapt" ["-a", "ADAPTER_FWD", "-A", "ADAPTER_REV", "-o", outFwd, "-p", outRev,  fwd, rev]
  where (outFwd, outRev) = (fwd <.> "cutdapt", rev <.> "cutadapt")
 
allFqs = liftA2 (++) sffs fqs
  where 
    -- TODO: fix with -<.> "fastq" !
    sffs = liftM (map (-<.> "fastq")) $ getDirectoryFiles "" ["data/*.sff"]
    fqs =  getDirectoryFiles "" ["data/*.fastq"] 

runPython str = do 
  withTempFile $ \file -> do
    liftIO $ writeFile file fixedStr
    cmd "python" file
  where 
    --TODO: fix so this works even if the first line is empty
    indent = length $ takeWhile (== ' ') $ head $ lines str
    fixedStr = unlines $ map (drop indent) $ lines str

    --foldl (\x y -> x ++ "\n" ++ y) "" $ map (drop indent) $ lines str
    
block = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    want ["data/align/align.bam"] 

    -- align.bam is the target
    "data/align/align.bam" %> \out -> do 
       sffs <- getDirectoryFiles "" ["data/*.sff"]
       let fqs = [c -<.> "fastq" | c <- sffs]
       need fqs
       allFastqs <- getDirectoryFiles "" ["data/*.fastq"] 
       --allFastqs <- allFqs
       --need allFastqs -- fqs is the prerequisites
       putNormal $ show ("input sffs", allFastqs) -- logging
       cmd Shell "cat" allFastqs ">" [out] 

    "data/*.fastq" %> \out -> do 
         let src = out -<.> "sff" --replace extension
         need [src]
         runPython [i| 
         from Bio import SeqIO
         SeqIO.convert('${src}', 'sff', '${out}', 'fastq')
         |]

    "data/*.cutadapt" %> \out -> do 
        fqs <- allFqs
        need fqs
        () <- cmd "echo" fqs
        fst $ mapPairedUnpaired pCutAdapt unpCutAdapt fqs --return type must be Action (), not tuple

-- note that stack cannot tell if a rule has changed. so altering want/need and re-running won't work.
       -- () <-  is needed if cmd is not the last statement, e.g.
       -- () <- cmd Shell "cat" fqs ">" [out]  -- [out] = ["align.bam"]
         -- sffs <- getDirectoryFiles "" ["//*.sff"]
