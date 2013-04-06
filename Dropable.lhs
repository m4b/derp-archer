\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dropable(Dropable(..)) where

class Dropable b a where
  drop :: b -> a -> a

\end{code}