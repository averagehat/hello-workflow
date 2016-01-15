
module Lib
    ( block
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
-- To run:
-- stack setup
-- stack exec shell-exe
block :: IO ()

block = shakeArgs shakeOptions{shakeFiles="_build"} $ do

    want ["data/align/align.bam"] 

    -- align.bam is the target
    "data/align/align.bam" %> \out -> do 
       sffs <- getDirectoryFiles "" ["data/*.sff"]
       let fqs = [c -<.> "fastq" | c <- sffs]
       need fqs -- fqs is the prerequisites

       putNormal $ show ("input sffs", sffs) -- logging
       cmd Shell "cat" fqs ">" [out] 


    "data/*.fastq" %> \out -> do 
         let src = out -<.> "sff" --replace extension
         need [src]
         cmd "cp" src out


-- note that stack cannot tell if a rule has changed. so altering want/need and re-running won't work.
       -- () <-  is needed if cmd is not the last statement, e.g.
       -- () <- cmd Shell "cat" fqs ">" [out]  -- [out] = ["align.bam"]
         -- sffs <- getDirectoryFiles "" ["//*.sff"]
