{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Monad    ((>=>))
import qualified Data.List        as List
import           Data.Ord         (Down (..))
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TIO
import qualified Data.Text.Lazy   as Lazy
import           Exceptions
import           Lib              (Body (..), Slug (..), extractSlugs, makePage,
                                   makePost, makeToc, _date, _frontmatter)
import           Path             (Dir, Path, Rel, reldir, relfile, (</>))
import qualified Path
import qualified System.Directory as Dir
import qualified System.Directory as Directory

-- TODO: Maybe handle IO errors? Not sure with all the writeFile and readFile
-- TODO: Write tests for extractSlugs and the exceptions
-- TODO: Copy assets to /public for each post
postsDir :: Path Rel Dir
postsDir = [reldir|./content/blog/|]

main :: IO ()
main = do
  posts <- Directory.listDirectory $ Path.toFilePath postsDir
  -- ^ Each post is a directory
  sequence <$> traverse (Path.parseRelDir >=> extractSlugs postsDir) posts >>= \case
    Left e ->
      case e of
        ExtractSlugE file msg ->
          putStrLn ("Could not extract slug information from " <> show file) >>
          print msg
        _ -> putStrLn "Unknown exception"
    Right slugs -> do
      let outDir = [reldir|./public/|]
          sortedSlugs = List.sortOn (Down . _date . _frontmatter) slugs
      mapM_ (createAndWritePost outDir) sortedSlugs
      -- ^ Transform markdown to html with cmark and write to output folder
      -- | Generate index.html with links to posts and also write to output
      -- folder
      makeToc sortedSlugs >>= makePage . Body >>=
        TIO.writeFile (Path.toFilePath $ outDir </> [relfile|index.html|]) .
        Lazy.toStrict
      putStrLn "Done!"
  where
    createAndWritePost dir slug =
      Path.parseRelDir (Text.unpack $ _postDir slug) >>= \postDir ->
        Dir.createDirectoryIfMissing True (Path.toFilePath $ dir </> postDir) >>
        let fpath = Path.toFilePath $ dir </> postDir </> [relfile|index.html|]
         in makePost slug >>= makePage . Body >>=
            TIO.writeFile fpath . Lazy.toStrict
