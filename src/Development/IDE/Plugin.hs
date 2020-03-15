
module Development.IDE.Plugin(Plugin(..)) where

import Data.Default
--import Development.IDE.LSP.Server
import Development.IDE.Core.Reflex

--import           Language.Haskell.LSP.Types
--import Development.IDE.Core.Rules
--import qualified Language.Haskell.LSP.Core as LSP
--import Language.Haskell.LSP.Messages


data Plugin c = Plugin
    {pluginRules :: Rules
    }

instance Default (Plugin c) where
    def = Plugin mempty

instance Semigroup (Plugin c) where
    Plugin x1 <> Plugin x2 = Plugin (x1<>x2)

instance Monoid (Plugin c) where
    mempty = def

{-
codeActionPlugin :: (LSP.LspFuncs c -> IdeState -> TextDocumentIdentifier -> Range -> CodeActionContext -> IO (Either ResponseError [CAResult])) -> Plugin c
codeActionPlugin f = Plugin mempty $ PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction g
    }
    where
      g lsp state (CodeActionParams a b c _) = fmap List <$> f lsp state a b c
      -}
