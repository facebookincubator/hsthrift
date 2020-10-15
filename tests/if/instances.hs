-- @lint-ignore HLINT1 T25377293 Grandfathered in
--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--
{-# LANGUAGE RecordWildCards #-}
import Data.Semigroup (Semigroup(..))
import Prelude ((++), Monoid(..))

instance Semigroup X where
  (<>) = mappend

instance Prelude.Monoid X where
  mempty = X [] []
  mappend X{..} (X y1 y2) = X (x_intList ++ y1) (x_stringList ++ y2)
