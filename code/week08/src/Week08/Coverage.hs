{-# LANGUAGE TemplateHaskell #-}

module Week08.Coverage
    ( renderCoverageReport
    ) where

import           Control.Lens
import           Control.Monad       (foldM, forM_)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.List           (foldl')
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.String         (IsString (..))
import           PlutusTx.Coverage   (CoverageAnnotation (..), CoverageReport (..), CovLoc (..))
import           System.Console.ANSI

data Character = Character
    { _cChar :: !Char
    , _cAnn  :: !(Maybe (Maybe Bool))
    } deriving (Show, Eq, Ord)

makeLenses ''Character

data Line = Line
    { _lLen   :: !Int
    , _lChars :: !(IntMap Character)
    } deriving (Show, Eq, Ord)

makeLenses ''Line

newtype File = File {_fLines :: IntMap Line}
    deriving (Show, Eq, Ord)

makeLenses ''File

newtype Project = Project {_pFiles :: Map FilePath File}
    deriving (Show, Eq, Ord)

makeLenses ''Project

emptyProject :: Project
emptyProject = Project Map.empty

instance IsString Line where
    fromString s =
      let
        (m, n) = foldl' f (IM.empty, 0) s
      in
        Line
            { _lLen   = n
            , _lChars = m
            }
      where
        f :: (IntMap Character, Int) -> Char -> (IntMap Character, Int)
        f (m, n) c =
          let
            n' = succ n
            c' = Character
                    { _cChar = c
                    , _cAnn  = Nothing
                    }
            m' = IM.insert n' c' m
          in
            (m', n')

readFile' :: FilePath -> IO File
readFile' fp = do
    ls <- lines <$> readFile fp
    let m = fst $ foldl' f (IM.empty, 0) ls
    return $ File m
  where
    f :: (IntMap Line, Int) -> String -> (IntMap Line, Int)
    f (m, n) s =
      let
        n' = succ n
        m' = IM.insert n' (fromString s) m
      in
        (m', n')

renderChar :: Character -> IO ()
renderChar c = case c ^. cAnn of
    Nothing           -> putChar $ c ^. cChar
    Just Nothing      -> f Yellow
    Just (Just True)  -> f Blue
    Just (Just False) -> f Red
  where
    f :: Color -> IO ()
    f bg = setSGR [SetColor Background Dull bg, SetColor Foreground Dull Black] >> putChar (c ^. cChar) >> setSGR []

renderLine :: Line -> IO ()
renderLine l = mapM_ renderChar (l ^. lChars) >> putChar '\n'

renderFile :: File -> IO ()
renderFile f = mapM_ renderLine $ f ^. fLines

renderProject :: Project -> IO ()
renderProject p = forM_ (Map.toList $ p ^. pFiles) $ \(n, f) -> do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn n
    setSGR []
    putStrLn ""
    renderFile f
    putStrLn ""
    putStrLn ""

applyToChar :: Maybe Bool -> Character -> Character
applyToChar mb = over cAnn f
  where
    f :: Maybe (Maybe Bool) -> Maybe (Maybe Bool)
    f Nothing        = Just mb
    f (Just Nothing) = Just mb
    f mb'            = mb'

applyToLine :: Int -> Int -> Maybe Bool -> Line -> Line
applyToLine col1 col2 mb l = foldl' (\l' i -> l' & lChars . ix i %~ applyToChar mb) l [col1 .. col2]

applyToLineFrom :: Int -> Maybe Bool -> Line -> Line
applyToLineFrom col mb l = applyToLine col (l ^. lLen) mb l

applyToLineTo :: Int -> Maybe Bool -> Line -> Line
applyToLineTo col = applyToLine 1 col

applyCovLoc :: Int -> Int -> Int -> Int -> Maybe Bool -> File -> File
applyCovLoc row1 row2 col1 col2 mb f
    | row2 <= row1 = f & fLines . ix row1 %~ applyToLine col1 col2 mb
    | otherwise    = foldl' (\f'' row -> f'' & fLines . ix row %~ applyToLineFrom 1 mb) f' [succ row1 .. pred row2]
  where
    f' = f & fLines . ix row1 %~ applyToLineFrom col1 mb
           & fLines . ix row2 %~ applyToLineTo   col2 mb

renderCoverageReport :: CoverageReport -> IO ()
renderCoverageReport report = do
    p <- foldM f emptyProject $ _coveredAnnotations report
    renderProject p
  where
    f :: Project -> CoverageAnnotation -> IO Project
    f p c = case c of
        CoverLocation covLoc -> g covLoc Nothing
        CoverBool covLoc b   -> g covLoc $ Just b
      where
        g :: CovLoc -> Maybe Bool -> IO Project
        g covLoc mb = do
            let fp = _covLocFile covLoc
            p' <- case p ^. pFiles . at fp of
                Nothing -> do
                    file <- readFile' fp
                    return $ p & pFiles . at fp .~ Just file
                Just _  -> return p
            return $ p' & pFiles . ix fp %~ applyCovLoc
                (_covLocStartLine covLoc)
                (_covLocEndLine covLoc)
                (_covLocStartCol covLoc)
                (_covLocEndCol covLoc)
                mb
