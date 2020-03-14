-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ConstraintKinds               #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE FlexibleContexts               #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.RuleTypes(
    module Development.IDE.Core.RuleTypes
    ) where

import qualified Data.Map as M
import           Control.DeepSeq
import Data.Binary
import           Development.IDE.Import.DependencyInformation
import Development.IDE.GHC.Util
import           Data.Hashable
import           Data.Typeable
import qualified Data.Set as S
import           GHC.Generics                             (Generic)
import Development.IDE.Core.PositionMapping

import           GHC
import Module (InstalledUnitId)
import HscTypes (CgGuts, Linkable, HomeModInfo, ModDetails)

import           Development.IDE.Spans.Type
import           Development.IDE.Import.FindImports (ArtifactsLocation)
import Data.GADT.Compare.TH
import Development.IDE.Types.Options
import Data.GADT.Show.TH

import Language.Haskell.LSP.VFS
import qualified Data.Text as T
import Development.IDE.Types.Location
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import {-# SOURCE #-} Development.IDE.Core.Reflex.Rules

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

type LocatedImports = ([(Located ModuleName, Maybe ArtifactsLocation)], S.Set InstalledUnitId)

-- Per module rules
data RuleType a where
--  GetFileExists :: RuleType Bool
  GetFileContents :: RuleType (FileVersion, Maybe T.Text)
  GetParsedModule :: RuleType ParsedModule
  GetLocatedImports :: RuleType LocatedImports
  GetSpanInfo :: RuleType SpansInfo
  GetDependencyInformation :: RuleType DependencyInformation
  GetDependencies :: RuleType TransitiveDependencies
  GetTypecheckedModule :: RuleType TcModuleResult
  ReportImportCycles :: RuleType ()
  GenerateCore :: RuleType (SafeHaskellMode, CgGuts, ModDetails)
  GenerateByteCode :: RuleType Linkable
  GhcSession :: RuleType HscEnvEq
  GetHiFile :: RuleType HiFileResult
  GetModIface :: RuleType HiFileResult
  IsFileOfInterest :: RuleType Bool

data GlobalType a where
  GetHscEnv :: GlobalType SessionMap
  GhcSessionIO :: GlobalType GhcSessionFun
  GetEnv :: GlobalType HscEnv
  GetIdeOptions :: GlobalType IdeOptions
  OfInterestVar :: GlobalType (HashSet NormalizedFilePath)
  FileExistsMapVar :: GlobalType FileExistsMap
  GetVFSHandle :: GlobalType VFSHandle
  GetInitFuncs :: GlobalType InitParams
  IdeConfigurationVar :: GlobalType IdeConfiguration
  GetPositionMap :: GlobalType PositionMap

type PositionMap = (HashMap NormalizedUri (M.Map LSP.TextDocumentVersion PositionMapping))

type InitParams = (VFSHandle, LSP.ClientCapabilities, IO LSP.LspId, (LSP.FromServerMessage -> IO ()))

-- | Lsp client relevant configuration details
data IdeConfiguration = IdeConfiguration
  { workspaceFolders :: HashSet NormalizedUri
  }
  deriving (Show)

data SessionMap = SessionMap { sessionMap :: M.Map FilePath HscEnvEq
                             , cradleLoc :: FilePath -> IO (Maybe FilePath)
                             , update :: (FilePath, HscEnvEq) -> IO () }

data GetHscEnvArgs = GetHscEnvArgs
    { hscenvOptions :: [String]        -- componentOptions from hie-bios
    , hscenvDependencies :: [FilePath] -- componentDependencies from hie-bios
    }
    deriving (Eq, Show, Typeable, Generic, Ord)
instance Hashable GetHscEnvArgs
instance NFData   GetHscEnvArgs
instance Binary   GetHscEnvArgs

-- | A map for tracking the file existence
type FileExistsMap = (HashMap NormalizedFilePath Bool)

-- | haskell-lsp manages the VFS internally and automatically so we cannot use
-- the builtin VFS without spawning up an LSP server. To be able to test things
-- like `setBufferModified` we abstract over the VFS implementation.
data VFSHandle = VFSHandle
    { getVirtualFile :: NormalizedUri -> IO (Maybe VirtualFile)
        -- ^ get the contents of a virtual file
    , setVirtualFileContents :: Maybe (NormalizedUri -> Maybe T.Text -> IO ())
        -- ^ set a specific file to a value. If Nothing then we are ignoring these
        --   signals anyway so can just say something was modified
    }



newtype GhcSessionFun = GhcSessionFun (FilePath -> ForallAction HscEnvEq)
instance Show GhcSessionFun where show _ = "GhcSessionFun"
instance NFData GhcSessionFun where rnf !_ = ()

data FileVersion
    = VFSVersion Int
    | ModificationTime
      !Int   -- ^ Large unit (platform dependent, do not make assumptions)
      !Int   -- ^ Small unit (platform dependent, do not make assumptions)
    deriving (Show, Generic)

instance NFData FileVersion

vfsVersion :: FileVersion -> Maybe Int
vfsVersion (VFSVersion i) = Just i
vfsVersion ModificationTime{} = Nothing

modificationTime :: FileVersion -> Maybe (Int, Int)
modificationTime VFSVersion{} = Nothing
modificationTime (ModificationTime large small) = Just (large, small)

-- | Contains the typechecked module and the OrigNameCache entry for
-- that module.
data TcModuleResult = TcModuleResult
    { tmrModule     :: TypecheckedModule
    , tmrModInfo    :: HomeModInfo
    }
instance Show TcModuleResult where
    show = show . pm_mod_summary . tm_parsed_module . tmrModule

instance NFData TcModuleResult where
    rnf = rwhnf

tmrModSummary :: TcModuleResult -> ModSummary
tmrModSummary = pm_mod_summary . tm_parsed_module . tmrModule

data HiFileResult = HiFileResult
    { hirModSummary :: !ModSummary
    -- Bang patterns here are important to stop the result retaining
    -- a reference to a typechecked module
    , hirModIface :: !ModIface
    }

instance NFData HiFileResult where
    rnf = rwhnf

instance Show HiFileResult where
    show = show . hirModSummary

concat <$> sequence [deriveGEq ''RuleType, deriveGCompare ''RuleType, deriveGShow ''RuleType]
concat <$> sequence [deriveGEq ''GlobalType, deriveGCompare ''GlobalType, deriveGShow ''GlobalType]

{-
-- NOTATION
--   Foo+ means Foo for the dependencies
--   Foo* means Foo for me and Foo+

-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = ParsedModule

-- | The dependency information produced by following the imports recursively.
-- This rule will succeed even if there is an error, e.g., a module could not be located,
-- a module could not be parsed or an import cycle.
type instance RuleResult GetDependencyInformation = DependencyInformation

-- | Transitive module and pkg dependencies based on the information produced by GetDependencyInformation.
-- This rule is also responsible for calling ReportImportCycles for each file in the transitive closure.
type instance RuleResult GetDependencies = TransitiveDependencies


-- | The type checked version of this file, requires TypeCheck+
type instance RuleResult TypeCheck = TcModuleResult

-- | Information about what spans occur where, requires TypeCheck
type instance RuleResult GetSpanInfo = SpansInfo

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = (SafeHaskellMode, CgGuts, ModDetails)

-- | Generate byte code for template haskell.
type instance RuleResult GenerateByteCode = Linkable

-- | A GHC session that we reuse.
type instance RuleResult GhcSession = HscEnvEq

-- | Resolve the imports in a module to the file path of a module
-- in the same package or the package id of another package.
type instance RuleResult GetLocatedImports = ([(Located ModuleName, Maybe ArtifactsLocation)], S.Set InstalledUnitId)

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = ()

-- | Read the module interface file
type instance RuleResult GetHiFile = HiFileResult

-- | Get a module interface, either from an interface file or a typechecked module
type instance RuleResult GetModIface = HiFileResult

type instance RuleResult IsFileOfInterest = Bool

data GetParsedModule = GetParsedModule
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetParsedModule
instance NFData   GetParsedModule
instance Binary   GetParsedModule

data GetLocatedImports = GetLocatedImports
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetLocatedImports
instance NFData   GetLocatedImports
instance Binary   GetLocatedImports

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation
instance Binary   GetDependencyInformation

data ReportImportCycles = ReportImportCycles
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ReportImportCycles
instance NFData   ReportImportCycles
instance Binary   ReportImportCycles

data GetDependencies = GetDependencies
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencies
instance NFData   GetDependencies
instance Binary   GetDependencies

data TypeCheck = TypeCheck
    deriving (Eq, Show, Typeable, Generic)
instance Hashable TypeCheck
instance NFData   TypeCheck
instance Binary   TypeCheck

data GetSpanInfo = GetSpanInfo
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetSpanInfo
instance NFData   GetSpanInfo
instance Binary   GetSpanInfo

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateCore
instance NFData   GenerateCore
instance Binary   GenerateCore

data GenerateByteCode = GenerateByteCode
   deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateByteCode
instance NFData   GenerateByteCode
instance Binary   GenerateByteCode

data GhcSession = GhcSession
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSession
instance NFData   GhcSession
instance Binary   GhcSession

data GetHiFile = GetHiFile
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHiFile
instance NFData   GetHiFile
instance Binary   GetHiFile

data GetModIface = GetModIface
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIface
instance NFData   GetModIface
instance Binary   GetModIface


data IsFileOfInterest = IsFileOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable IsFileOfInterest
instance NFData   IsFileOfInterest
instance Binary   IsFileOfInterest
-}
