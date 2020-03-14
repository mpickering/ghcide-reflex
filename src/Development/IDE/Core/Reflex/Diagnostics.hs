{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Core.Reflex.Diagnostics where

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NL
--import {-# SOURCE #-} Language.Haskell.Core.FileStore (getModificationTime)
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Diagnostics
import qualified Data.SortedList as SL
import qualified Data.Text as T
import Data.List
import qualified Data.HashMap.Strict as HMap
import Control.Monad.Fix
import Data.Functor
import Data.Functor.Barbie
import Data.Functor.Product
import Language.Haskell.LSP.Core
import Data.Kind
import Reflex.Host.Class
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Core.RuleTypes
import Control.Monad.Extra
import Control.Monad.Reader
import Data.GADT.Show
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Control.Error.Util
import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DMap, DSum(..))
import Data.These(These(..))
import Reflex.Time
import Reflex.Network
import System.Directory
import Reflex
import GHC hiding (parseModule, mkModule, typecheckModule, getSession)
import qualified GHC
import Reflex.PerformEvent.Class
import Development.IDE.Core.Compile
import Data.Default
import Control.Monad.IO.Class
import Development.IDE.Types.Location
import StringBuffer
import Development.IDE.Types.Options
import Data.Dependent.Map (GCompare)
import Data.GADT.Compare
import qualified Data.Map as M
import Unsafe.Coerce
import Reflex.Host.Basic
import Development.IDE.Import.FindImports
import Control.Monad
import HscTypes
import Data.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Module hiding (mkModule)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Strict
import Development.IDE.Types.Diagnostics
import Development.IDE.Import.DependencyInformation
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Coerce
import Data.Traversable
import qualified GHC.LanguageExtensions as LangExt
import DynFlags
import Development.IDE.Spans.Type
import Development.IDE.Spans.Calculate
--import HIE.Bios
--import HIE.Bios.Environment
import System.Environment
import System.IO
import Linker
--import qualified GHC.Paths
import Control.Concurrent
import Reflex.Profiled
import Debug.Trace
import Control.Monad.Ref
import Reflex.Host.Class
import Data.Time.Clock

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Development.IDE.Types.Logger
import Development.IDE.Core.Debouncer

import Development.IDE.Core.Reflex.Thunk
import Development.IDE.Core.Reflex.Early
import Development.IDE.Core.Reflex.Constraints

{- Diagnostics -}

type Key = D.Some RuleType

type DiagsInfo = (Maybe Int, NormalizedFilePath, Key, [FileDiagnostic])



updateFileDiagnostics ::
  (Reflex t, MonadHold t m, MonadFix m) => Event t (NL.NonEmpty DiagsInfo)
  -> m (Event t (DiagnosticStore, [(NormalizedUri, [Diagnostic])])
       , Dynamic t DiagnosticStore)
updateFileDiagnostics diags = do
    let split es = unzip (map split_one (NL.toList es))
        split_one (mt, fp, k, es) =
            let es' = map (\(_, b, c) -> (b, c)) es
                (currentShown, currentHidden) =
                  partition (\(b, c) -> b == ShowDiag) es'
            in ((mt, fp, k, currentShown), (mt, fp, k, currentHidden))
    let (shown_e, hidden_e) = splitE (split <$> diags)
    newDiags <- foldDynMaybe (\a b -> checkNew (foldr updNewDiags (fst b, []) a) b)
                  (HMap.empty, []) shown_e
    hiddenDiags <- foldDyn (\a b -> foldr updHiddenDiags b a) HMap.empty hidden_e
    return (updated newDiags, hiddenDiags)


    where
      checkNew (newDiagsStore, newDiags) (old, old_newDiags)
        = if old_newDiags == newDiags
            then Nothing
            else Just (newDiagsStore, newDiags)
      updNewDiags (ver, fp, k, currentShown) (c, cds) =
            let newDiagsStore = setStageDiagnostics fp ver
                                  (T.pack $ show k) (map snd currentShown) c
                newDiags = getFileDiagnostics fp newDiagsStore
                uri = filePathToUri' fp
            in (newDiagsStore, (uri, newDiags) : cds)

      updHiddenDiags (ver, fp, k, currentHidden) old =
            let newDiagsStore = setStageDiagnostics fp ver
                                  (T.pack $ show k) (map snd currentHidden) old
                newDiags = getFileDiagnostics fp newDiagsStore
            in newDiagsStore


{-
                     -}

--publish

publishDiagnosticsNotification :: Uri -> [Diagnostic] -> LSP.FromServerMessage
publishDiagnosticsNotification uri diags =
    LSP.NotPublishDiagnostics $
    LSP.NotificationMessage "2.0" LSP.TextDocumentPublishDiagnostics $
    LSP.PublishDiagnosticsParams uri (List diags)



-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics
    :: NormalizedFilePath
    -> LSP.TextDocumentVersion -- ^ the time that the file these diagnostics originate from was last edited
    -> T.Text
    -> [LSP.Diagnostic]
    -> DiagnosticStore
    -> DiagnosticStore
setStageDiagnostics fp timeM stage diags ds  =
    updateDiagnostics ds uri timeM diagsBySource
    where
        diagsBySource = M.singleton (Just stage) (SL.toSortedList diags)
        uri = filePathToUri' fp

getAllDiagnostics ::
    DiagnosticStore ->
    [FileDiagnostic]
getAllDiagnostics =
    concatMap (\(k,v) -> map (fromUri k,ShowDiag,) $ getDiagnosticsFromStore v) . HMap.toList

getFileDiagnostics ::
    NormalizedFilePath ->
    DiagnosticStore ->
    [LSP.Diagnostic]
getFileDiagnostics fp ds =
    maybe [] getDiagnosticsFromStore $
    HMap.lookup (filePathToUri' fp) ds

filterDiagnostics ::
    (NormalizedFilePath -> Bool) ->
    DiagnosticStore ->
    DiagnosticStore
filterDiagnostics keep =
    HMap.filterWithKey (\uri _ -> maybe True (keep . toNormalizedFilePath) $ uriToFilePath' $ fromNormalizedUri uri)

filterVersionMap
    :: HMap.HashMap NormalizedUri (Set.Set LSP.TextDocumentVersion)
    -> HMap.HashMap NormalizedUri (M.Map LSP.TextDocumentVersion a)
    -> HMap.HashMap NormalizedUri (M.Map LSP.TextDocumentVersion a)
filterVersionMap =
    HMap.intersectionWith $ \versionsToKeep versionMap -> M.restrictKeys versionMap versionsToKeep

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) = concatMap SL.fromSortedList $ M.elems diags

