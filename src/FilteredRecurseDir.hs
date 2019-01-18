module FilteredRecurseDir where

import System.IO.HVFS
import System.IO.Unsafe
import System.FilePath (pathSeparator)

recurseFilterDir :: HVFS a => a -> FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
recurseFilterDir fs x pred = recurseFilterDirStat fs x pred >>= return . map fst

recurseFilterDirStat :: HVFS a => a -> FilePath -> (FilePath -> IO Bool) -> IO [(FilePath, HVFSStatEncap)]
recurseFilterDirStat h fn pred =
    do fs <- vGetSymbolicLinkStatus h fn
       validFile <- pred fn
       putStrLn ("path " ++ (show fn) ++ " is " ++ (if validFile then "valid" else "invalid" ))
       if validFile
          then if withStat fs vIsDirectory
              then do
                   dirc <- vGetDirectoryContents h fn
                   let contents = map ((++) (fn ++ [pathSeparator])) $
                                  filter (\x -> x /= "." && x /= "..") dirc
                   subdirs <- unsafeInterleaveIO $ mapM (\fn -> recurseFilterDirStat h fn pred) contents
                   return $ (concat subdirs) ++ [(fn, fs)]
              else return [(fn, fs)]
          else return []
