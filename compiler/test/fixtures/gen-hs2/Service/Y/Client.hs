-----------------------------------------------------------------
-- Autogenerated by Thrift
--
-- DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
--  @generated
-----------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Service.Y.Client (Y) where
import qualified Service.X.Client as X
import qualified Thrift.Codegen as Thrift
import Service.Types

data Y

type instance Thrift.Super Y = X.X