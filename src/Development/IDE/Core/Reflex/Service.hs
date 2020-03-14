{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.Reflex.Service( reflexOpen ) where


import qualified Data.List.NonEmpty as NL
--import {-# SOURCE #-} Language.Haskell.Core.FileStore (getModificationTime)
import Language.Haskell.LSP.VFS
import Control.Monad.Fix
import Data.Functor
import Data.Functor.Barbie
import Data.Functor.Product
import Language.Haskell.LSP.Core
import Development.IDE.Core.RuleTypes
import Control.Monad.Extra
import Control.Monad.Reader
import Data.GADT.Show
import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DSum(..))
import Reflex.Time
import Reflex
import Data.Default
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import qualified Data.Map as M
import Reflex.Host.Basic
import Debug.Trace

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Development.IDE.Types.Logger
import Development.IDE.Core.Debouncer

import Development.IDE.Core.Reflex.Thunk
import Development.IDE.Core.Reflex.Early
import Development.IDE.Core.Reflex.Diagnostics
import Development.IDE.Core.Reflex.Rules



-- Start the network, the main entry point
reflexOpen :: Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> (Handlers ->
                ((VFSHandle, LSP.ClientCapabilities, IO LSP.LspId, (LSP.FromServerMessage -> IO ())) -> IO ())
              -> ((D.Some RuleType, NormalizedFilePath) -> IO ())
              -> ForallBasic ())
           -> Rules
           -> IO ()
reflexOpen logger _debouncer _opts startServer init_rules = do


  basicHostWithQuit $ mdo
    let (mod_rules, global_rules, unitActions) = partitionRules init_rules
    pb <- getPostBuild
    (input, input_trigger) <- newTriggerEvent

    -- Doesn't typecheck if you have the signature (haha)
    let --rule :: Event t () -> GlobalDefinition (Performable m) t -> m (DSum GlobalType (Dynamic t))
        rule _trigger (name :=> (WrappedDynamicM act)) = mdo
          (upd_e, upd) <- newTriggerEvent
          d <- unwrapBG (flip runReaderT renv act)
          d' <- dynApplyEvent upd_e d
          return (name :=> GlobalVar d' upd)

    let renv = REnv genv mmap

    genv_rules <- mapM (rule pb) global_rules

    let genv = GlobalEnv (D.fromList genv_rules) hs_es init logger

    -- Less important to incrementally build the diagnostics map I think

    mmap <- unwrapBG $ mkModuleMap genv mod_rules input


    runReaderT triggerTypecheck renv

    (hs, hs_es) <- mkHandlers


    -- Start reporting diagnostics
    all_diags <- switchHoldPromptly never
                   (collectDiags <$> updated (incrementalToDynamic (getMap mmap)))
    (output_diags, _) <- updateFileDiagnostics all_diags
    reportDiags renv (fmap snd output_diags)

    -- Start running the user supplied functions
    (init, init_trigger) <- newTriggerEvent
    let ForallBasic start = startServer hs init_trigger input_trigger
    unwrapBG $ flip runReaderT renv $ do
                start
                sequence unitActions

    return never


-- TODO: This is broken
dynApplyEvent
  :: (Adjustable t m, MonadHold t m, MonadFix m)
  => Event t (a -> a)
  -> Dynamic t a
  -> m (Dynamic t a)
dynApplyEvent e d = do
  (a, b) <- runWithReplace (pure d) (updated d <&> \i -> foldDyn id i e)
  dd <- holdDyn a b
  pure (join dd)

-- Make the main module map
mkModuleMap :: forall t m . (Adjustable t m
                            , Reflex t
                            , PerformEvent t m
                            , Monad m
                            , TriggerEvent t m
                            , MonadIO m
                            , MonadFix m
                            , MonadIO (Performable m)
                            , MonadHold t m
                            ) => GlobalEnv t
                 -> [ShakeDefinition m t]
                 -> Event t (D.Some RuleType, NormalizedFilePath)
                 -> m (ModuleMapWithUpdater t)
mkModuleMap genv rules_raw input = mdo

  -- An event which is triggered to update the incremental
  (map_update, update_trigger) <- newTriggerEvent
  -- The batchOccurences helps to group together updates, especially at the
  -- start of the process where a lot of new files are added at once.
  map_update' <- fmap concat <$> batchOccurrences 0.1 map_update
  let -- (k, v) pair for one element in the map
      mkOne (sel, fp) = (fp, Just (mkModule genv rules_raw mmu sel fp))
      sing fp = [fp]
      -- The updates needed to be made to the map
      inp = M.fromList . map mkOne <$> mergeWith (++) [sing <$> input, map_update']

  mod_map <- listWithKeyShallowDiff
                M.empty  -- Map starts empty
                inp      -- Event which fires with new things to add to the map
                (\_ act _ -> snd <$> act)
  let mmu = (MMU mod_map update_trigger)
  return mmu

-- Create the state for one module, this is the key function in the whole
-- implementation.
mkModule :: forall t m . (MonadIO m, PerformEvent t m, TriggerEvent t m, Monad m, Reflex t, MonadFix m, _) =>
                 GlobalEnv t
              -> [ShakeDefinition m t]
              -> ModuleMapWithUpdater t
              -> D.Some RuleType
              -> NormalizedFilePath
              -> m (NormalizedFilePath, ModuleState t)
mkModule genv rules_raw mm (D.Some sel) f = do

  -- Make dynamics for all the rules in the program
  (rule_dyns, diags_e) <- unzip <$> mapM rule rules_raw
  let rule_dyns_map = D.fromList rule_dyns
  -- Run the specific rule which triggered the state for this module to be
  -- initialised.
  case D.lookup sel rule_dyns_map of
    Just (MDynamic d) -> forceThunk (unearly <$> d)
    Nothing -> return ()

  -- Combine together diagnostics for all the rules
  let diags_with_mod = mergeList diags_e

  let m = ModuleState { rules = D.fromList rule_dyns
                      , diags = diags_with_mod }
  return (f, m)

  where
    -- Interpret a rule
    -- There are three different ways to trigger a rule
    -- * Each rule has a specific trigger the user can call to fire the
    -- rule
    -- * The "start" trigger is called when the thunk is initially forced
    -- * If any of the dependencies change
    rule :: _ => ShakeDefinition m t
         -> m (DSum RuleType (MDynamic t), Event t DiagsInfo)
    rule (name :=> (WrappedEarlyActionWithTrigger act user_trig)) = mdo
        --
        user_trig' <- ([UserTrigger] <$) <$> runReaderT (user_trig f) renv
        let rebuild_trigger = (fmap (\e -> leftmost [user_trig', start_trigger, e]) deps')
--        act_trig <- Reflex.traceEvent ident <$> switchHoldPromptly start_trigger rebuild_trigger
        -- switchHoldPromptly is important here so that updats to dynamics
        -- in the same frame will immediately trigger a rebuild.
        act_trig <- switchHoldPromptly start_trigger rebuild_trigger

        -- When the trigger fires, run the rule
        pm <- performAction renv (act f) act_trig
        -- Separate the dependencies from the actual result
        let (act_res, deps) = splitE pm
        let deps' = pushAlways mkDepTrigger deps
        -- Create a thunk from the actual result, this also returns the
        -- start trigger which can be used to force the thunk.
        (act_des_d, trigger) <- thunk act_res
        let start_trigger = [StartTrigger] <$ trigger

        let swap_tup ((a, (b, c))) = (b, (a, c))
        let d = swap_tup . fmap (splitThunk []) . splitThunk Nothing <$> act_des_d
        let (pm_diags, res) = splitDynPure d
        -- Enable early cut-off
        early_res <- early (fmap joinThunk <$> res)
        -- Attach the file version and rule name to the diagnostics
        diags_with_mod <- performEvent (get_mod <$> attach vfs_b (updated pm_diags))
        let ident = show f ++ ": " ++ gshow name
--        return (name :=> (MDynamic $ traceDynE ("D:" ++ ident) early_res), diags_with_mod)
        return ((name :=> MDynamic early_res), diags_with_mod)
--
      where
        get_mod (vfs, ds) = do
          mbVirtual <- liftIO $ getVirtualFile vfs $ filePathToUri' f
          return (virtualFileVersion <$> mbVirtual, f, D.Some name, ds)

    vfs_b = (current . dyn $ D.findWithDefault (error "error") GetVFSHandle (globalEnv genv))

    renv = REnv genv mm

traceDynE p d = traceDynWith (const $ Debug.Trace.traceEvent p p) d



instance TraversableB HandlersG where
instance ApplicativeB HandlersG where
instance ConstraintsB HandlersG where
instance FunctorB HandlersG where


-- Make the handlers and events which fire when a handler fires
mkHandlers :: forall m t . _ => m (Handlers, GlobalHandlers t)
mkHandlers = do
  res <- btraverseC @Show go h
  let (hs, gs) = bunzip res
  -- Unset this handler as haskell-lsp complains about it
  return (hs { documentOnTypeFormattingHandler = MNothing }, gs)
  where
    h :: Handlers
    h = def

    go :: Show a => f a -> m ((MHandler `Product` (Event t)) a)
    go _ = do
      (input, trig) <- newTriggerEvent
      let input' = Reflex.traceEvent "handler" input
      return (Pair (MJust trig) (input'))


-- This use of (++) could be really inefficient, probably a better way to
-- do it
collectDiags :: _ => (M.Map NormalizedFilePath (ModuleState t)) -> Event t (NL.NonEmpty DiagsInfo)
collectDiags m = mergeWith (<>) $ map diags (M.elems m)

-- | Output the diagnostics, with a 0.1s debouncer
reportDiags :: _ => _ -> Event t _ -> m ()
reportDiags renv e = do
  e' <- debounce 0.1 e
  performEvent_ (mapM_ (flip runReaderT renv . putEvent . render) <$> e')
  where
    render (uri, newDiags)
      = publishDiagnosticsNotification (fromNormalizedUri uri) newDiags

    putEvent e = do
      eventer <- askEventer
      liftIO $ eventer e

-- Trigger a typecheck of all the files of interest when the OfInterest
-- var changes.
triggerTypecheck :: _ => BasicM t m ()
triggerTypecheck = do
  ofInterest <- getGlobalVar OfInterestVar
  env <- ask
  lift $ performEvent_ $ mapM_ (flip runReaderT env . trigger) <$> (updated (dyn ofInterest))
  where
    trigger nfp = do
      noTrack (use GetTypecheckedModule nfp)


