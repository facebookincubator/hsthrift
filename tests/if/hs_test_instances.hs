-- Copyright (c) Facebook, Inc. and its affiliates.

-- @nolint
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import Prelude ((/=), ($))

instance QuickCheck.Arbitrary Foo where
  arbitrary = Foo <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary TestStruct where
  arbitrary = TestStruct
              <$> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> (Map.fromList <$> QuickCheck.arbitrary)
              <*> arbitraryText
              <*> (Set.fromList <$> QuickCheck.arbitrary)
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> (HashMap.fromList <$> QuickCheck.arbitrary)
              <*> ((Z . X) <$> QuickCheck.arbitrary)
              <*> QuickCheck.arbitrary
              <*> arbitraryString
              <*> arbitraryBS
              <*> (Prelude.fmap X <$> QuickCheck.arbitrary)
              <*> QuickCheck.arbitrary
              <*> QuickCheck.arbitrary
              <*> (Vector.fromList <$> QuickCheck.arbitrary)
              <*> (VectorStorable.fromList <$> QuickCheck.arbitrary)
              <*> arbitraryBSMap
              <*> (Text.encodeUtf8 <$> arbitraryText)

instance QuickCheck.Arbitrary Number where
  arbitrary = QuickCheck.oneof $ Prelude.map Prelude.pure
              [Number_One, Number_Two, Number_Three]

instance QuickCheck.Arbitrary TUnion where
  arbitrary = QuickCheck.oneof
              [ TUnion_StringOption <$> arbitraryText
              , TUnion_I64Option <$> QuickCheck.arbitrary
              , TUnion_FooOption <$> QuickCheck.arbitrary
              , Prelude.pure TUnion_EMPTY
              ]

arbitraryString :: QuickCheck.Gen Prelude.String
arbitraryString = Prelude.filter (/= '\NUL') <$> QuickCheck.arbitrary

arbitraryText :: QuickCheck.Gen Text.Text
arbitraryText = Text.pack <$> arbitraryString

arbitraryBS :: QuickCheck.Gen ByteString.ByteString
arbitraryBS = ByteString.pack <$> QuickCheck.arbitrary

arbitraryBSMap
  :: QuickCheck.Arbitrary a => QuickCheck.Gen (Map.Map ByteString.ByteString a)
arbitraryBSMap =
  Map.fromList . Prelude.map (\(k,v) -> (ByteString.pack k, v)) <$>
  QuickCheck.arbitrary
