
module Lib
    ( block
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
-- stack exec shell-exe
block :: IO ()
--block = shakeArgs shakeOptions{shakeFiles="_build"} $ do
--    want ["_build/run" <.> exe]
--
--    phony "clean" $ do
--        putNormal "Cleaning files in _build"
--        removeFilesAfter "_build" ["//*"]
--
--    "_build/run" <.> exe %> \out -> do
--        cs <- getDirectoryFiles "" ["//*.c"]
----        let os = ["_build" </> c -<.> "o" | c <- cs]
----        need os
--        need ["_build"] -- actually, shake will build dirs automagically.
----        cmd "cat" os ">" [out]
--        --cmd "cat" cs ">" [out]
--        -- shell here allows for the ">" redirect operator
--        cmd Shell "cat" cs ">" [out]
--   
--    "_build" %> \out -> do
--        cmd "mkdir" "-p" out

candidates ext = do 
        cs <- getDirectoryFiles "" ["//*" <.> ext]
        return cs
-- note that stack cannot tell if a rule has changed. so altering
-- want/need and re-running won't work.
block = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_out/align/align.bam" <.> exe] 

    "_out/align/align.bam" %> \out -> do
       --let fqs = candidates "fastq"
--       fqs' <- getDirectoryFiles "" ["//*.fastq"]
           --Couldn't match expected type ‘Action ()’ with actual type ‘IO ()’  need to use ()
--       let fqs = fqs' 
--       need fqs                   --the number of slashes matters
       sffs <- getDirectoryFiles "" ["_out/*.sff"]
       --let fqs = ["_out" </> c -<.> "fastq" | c <- sffs]
       let fqs = [c -<.> "fastq" | c <- sffs]
       need fqs
       -- () <- cmd Shell "echo" fqs ">>" "shake-log"
       info <- mapM readFile' fqs
       putNormal $  show ("message", sffs)
       -- error $ show ("message", fqs) 
       putNormal $ show ("message", fqs) 
       () <- cmd Shell "echo" info ">>" "log"
       () <- cmd Shell "cat" fqs ">" [out] 
       cmd Shell "cat" sffs ">" [out] 

    "_out/*.fastq" %> \out -> do 
         let src = out -<.> "sff"
         need [src]
         -- sffs <- getDirectoryFiles "" ["//*.sff"]
         cmd "cp" src out


