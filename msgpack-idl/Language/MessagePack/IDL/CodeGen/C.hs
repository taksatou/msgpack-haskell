{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.C (
  Config(..),
  generate,
  ) where

import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.FilePath
import Text.Shakespeare.Text
import System.Directory

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    , configPrefix :: String    
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let prefix = configPrefix
  
  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile (prefix ++ "_types.h") $ templ configFilePath [lt|
#ifndef _A_H_
#define _A_H_

#{LT.concat $ map (genTypeDecl prefix) spec }

#endif /* _A_H_ */
|]

  LT.writeFile (prefix ++ "_types.c") $ templ configFilePath [lt|
#include <string.h>
#include <msgpack.h>
#include "#{prefix}_types.h"

#{LT.concat $ map (genTypeImpl (Config configFilePath configPrefix)) spec }

|]

genTypeDecl :: String -> Decl -> LT.Text
-- genTypeDecl prefix MPType {..} = [lt|
-- typedef #{tyType} #{tyName};
-- int #{prefix}_#{tyName}_to_msgpack(msgpack_packer *mpk, #{tyName} *arg);
-- int #{prefix}_#{tyName}_from_msgpack(msgpack_object *mobj, #{tyName} *arg);
-- |]

genTypeDecl prefix MPMessage {..} = [lt|
int #{prefix}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg);
int #{prefix}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg);
|]

genTypeDecl _ _ = ""


genTypeImpl :: Config -> Decl -> LT.Text

-- genTypeImpl _ MPType {..} = [lt|
-- class #{tyName}:
--   @staticmethod
--   def from_msgpack(arg):
--     return #{fromMsgpack tyType "arg"}
-- |]

genTypeImpl Config {..} MPMessage {..} = [lt|
int #{configPrefix}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg) {
    int res = 0;

    res |= msgpack_pack_array(pk, #{length msgFields});
#{LT.concat $ map pk msgFields}
    return res;
}

int #{configPrefix}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg) {
    int res = 0;

    return res;
}
|]
  where
  indent4 s =  LT.unlines $ map (LT.append (LT.pack "    ")) $ LT.split (\ c -> c == '\n') s
  pk Field {..} = indent4 [lt|#{toMsgpack (Config configFilePath configPrefix) fldType (T.append (T.pack "arg->") fldName)}|]

genTypeImpl _ _ = ""



sortField :: [Field] -> [Maybe Field]
sortField flds =
  flip map [0 .. maximum $ [-1] ++ map fldId flds] $ \ix ->
  find ((==ix). fldId) flds

genClient _ = ""

sanitize :: Char -> Char
sanitize '[' = '_'
sanitize ']' = '_'
sanitize c = c

toMsgpack :: Config -> Type -> T.Text -> LT.Text
toMsgpack _ (TInt _ _) arg = [lt|msgpack_pack_int(pk, #{arg});|]
toMsgpack _ (TFloat _) arg = [lt|msgpack_pack_double(pk, #{arg});|]
toMsgpack _ TBool arg = [lt|(arg)?msgpack_pack_true(pk):msgpack_pack_false(pk)});|]
toMsgpack _ TString arg = [lt|do {
    size_t _l = strlen(#{arg});
    msgpack_pack_raw(pk, _l);
    msgpack_pack_raw_body(pk, #{arg}, _l);
} while(0);|]

toMsgpack c (TList t) arg = [lt|msgpack_pack_array(pk, #{lis_len});
for (int _i = 0; i < #{lis_len}; ++_i) {
    #{toMsgpack c t arg};
}|]
  where
    lis_len = T.append arg (T.pack "_len")

toMsgpack c (TMap t1 t2) arg = [lt|msgpack_pack_map(pk, #{map_len});
for (int _i = 0; i < #{map_len}; ++_i) {
    #{toMsgpack c t1 ith_key};
    #{toMsgpack c t2 ith_val};
}|]
  where
    map_len = T.append arg (T.pack "_len")
    ith_key = T.append (T.cons '&' arg) (T.pack "_keys[_i]")
    ith_val = T.append (T.cons '&' arg) (T.pack "_vals[_i]")

toMsgpack c (TNullable t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
    #{toMsgpack c t arg};
}|]

toMsgpack c (TPointer t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
    #{toMsgpack' c t arg};
}|]
  where
    toMsgpack' Config {..} (TUserDef name _) arg = [lt|#{configPrefix}_#{name}_to_msgpack(pk, #{arg});|]
    toMsgpack' c t arg = toMsgpack c t (T.cons '*' arg)
    

toMsgpack Config {..} (TUserDef name _) arg = [lt|#{configPrefix}_#{name}_to_msgpack(pk, &#{arg});|]
toMsgpack _ t _ = [lt|/* #{show t} unsupported */|]

fromMsgpack :: Type -> T.Text -> LT.Text
fromMsgpack (TNullable t) name = fromMsgpack t name
fromMsgpack (TInt _ _) name = [lt|#{name}|]
fromMsgpack (TFloat False) name = [lt|#{name}|]
fromMsgpack (TFloat True) name = [lt|#{name}|]
fromMsgpack TBool name = [lt|#{name}|]
fromMsgpack TRaw name = [lt|#{name}|]
fromMsgpack TString name = [lt|#{name}|]
fromMsgpack (TList typ) name =
  let
    varname = T.append (T.pack "elem_") (T.map sanitize name) in
  [lt|[#{fromMsgpack typ varname} for #{varname} in #{name}]|]

fromMsgpack (TMap typ1 typ2) name =
  let
    keyname = T.append (T.pack "k_" ) $ T.map sanitize name
    valname = T.append (T.pack "v_" ) $ T.map sanitize name
  in
  [lt|{#{fromMsgpack typ1 keyname} : #{fromMsgpack typ2 valname} for #{keyname},#{valname} in #{name}.items()}|]

fromMsgpack (TUserDef className _) name = [lt|#{className}.from_msgpack(#{name})|]
            
fromMsgpack (TTuple ts) name =
            let elems = map (f name) (zip [0..] ts) in
            [lt| (#{LT.intercalate ", " elems}) |]
            where
              f :: T.Text -> (Integer, Type) -> LT.Text
              f n (i, (TUserDef className _ )) = [lt|#{className}.from_msgpack(#{n}[#{show i}]) |]
              f n (i, _) = [lt|#{n}[#{show i}]|]

fromMsgpack TObject name = [lt|#{name}|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
// This file is auto-generated from #{filepath}
// *** DO NOT EDIT ***

#{content}
|]
