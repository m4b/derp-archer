\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Filterable(Filterable(..)) where

import qualified Prelude as P

class Filterable b a where
  filter :: b -> a -> a

instance Filterable (a -> P.Bool) [a] where
  filter = P.filter

\end{code}