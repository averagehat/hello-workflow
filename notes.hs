
       --let fqs = ["_out" </> c -<.> "fastq" | c <- sffs]
       --let fqs = candidates "fastq"
--       fqs' <- getDirectoryFiles "" ["//*.fastq"]
           --Couldn't match expected type ‘Action ()’ with actual type ‘IO ()’  need to use ()
--       let fqs = fqs' 
--       need fqs                   --the number of slashes matters
--
--
--commands get logged to stdout
candidates ext = do 
        cs <- getDirectoryFiles "" ["//*" <.> ext]
        return cs

info <- mapM readFile' fqs
() <- cmd Shell "echo" info ">>" "log"
