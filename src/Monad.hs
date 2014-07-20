{-# LANGUAGE FlexibleInstances #-}

module Monad where

import Error

type HlamM a = Either HlamError a
