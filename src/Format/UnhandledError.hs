module Format.UnhandledError where

import           Data.Text.Prettyprint.Doc

class UnhandledError a where
    unhandledError :: a -> Doc ann
