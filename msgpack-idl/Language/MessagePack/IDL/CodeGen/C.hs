{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.C (
  Config(..),
  generate,
  ) where

import Data.List
import Data.Monoid
import Text.Printf
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
  let c = (Config configFilePath configPrefix)

  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile (configPrefix ++ "_types.h") $ templ configFilePath [lt|
#ifndef _A_H_
#define _A_H_

#include <msgpack.h>

#{LT.concat $ map (genTypeDecl c) spec }

#endif /* _A_H_ */
|]

  LT.writeFile (configPrefix ++ "_types.c") $ templ configFilePath [lt|
#include <string.h>
#include <msgpack.h>
#include "#{configPrefix}_types.h"

#{LT.concat $ map (genTypeImpl c) spec }

|]

genTypeDecl :: Config -> Decl -> LT.Text
genTypeDecl c MPMessage {..} = [lt|
int #{configPrefix c}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg);
int #{configPrefix c}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg);|]

genTypeDecl _ _ = ""

-- TODO: error check
genTypeImpl :: Config -> Decl -> LT.Text
genTypeImpl c MPMessage {..} = [lt|
int #{configPrefix c}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg) {
    msgpack_pack_array(pk, #{length msgFields});
#{LT.concat $ map pk msgFields}
    return 0;
}

int #{configPrefix c}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size == #{length msgFields}) {
        return -1;
    }

    msgpack_object *o = obj->via.array.ptr;
#{LT.concat $ map unpk $ zip nums msgFields}
    return 0;
}
|]
  where
  pk Field {..} = indent4
    [lt|#{toMsgpack c fldType (T.append (T.pack "arg->") fldName)}|]

  unpk (idx, Field {..}) = indent4
    [lt|#{fromMsgpack c (T.pack $ printf "o[%d]" idx) fldType fldDefault (T.append (T.pack "arg->") fldName)}|]

  nums :: [Integer]
  nums = [0..]

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
#{indent4 $ toMsgpack c t ith_val}}|]
  where
    lis_len = T.append arg (T.pack "_len")
    ith_val = T.append arg (T.pack "[_i]")


toMsgpack c (TMap t1 t2) arg = [lt|msgpack_pack_map(pk, #{map_len});
for (int _i = 0; i < #{map_len}; ++_i) {
#{indent4 $ toMsgpack c t1 ith_key}
#{indent4 $ toMsgpack c t2 ith_val}}|]
  where
    map_len = T.append arg (T.pack "_len")
    ith_key = T.append (T.cons '&' arg) (T.pack "_keys[_i]")
    ith_val = T.append (T.cons '&' arg) (T.pack "_vals[_i]")

toMsgpack c (TNullable t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
#{indent4 $ toMsgpack c t arg}}|]

toMsgpack c (TPointer t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
#{indent4 $ toMsgpack' t arg}}|]
  where
    toMsgpack' (TUserDef name _) arg = [lt|#{configPrefix c}_#{name}_to_msgpack(pk, #{arg});|]
    toMsgpack' t arg = toMsgpack c t (T.cons '*' arg)

toMsgpack c (TUserDef name _) arg = [lt|#{configPrefix c}_#{name}_to_msgpack(pk, &#{arg});|]
toMsgpack _ t _ = [lt|/* #{show t} unsupported */|]

-- TODO:
--   * set default
--   * malloc for pointer?

fromMsgpack :: Config -> T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
fromMsgpack _ o (TInt _ _) _ arg = [lt|#{arg} = #{o}.via.i64;|]
fromMsgpack _ o (TFloat _) _ arg = [lt|#{arg} = #{o}.via.dec;|]
fromMsgpack c o (TNullable t) d arg = fromMsgpack c o t d arg
fromMsgpack _ o TBool _ arg = [lt|#{arg} = #{o}.via.boolean;|]
fromMsgpack _ o TString _ arg = [lt|#{arg} = #{o}.via.raw.ptr;|]
fromMsgpack c o (TUserDef name _) _ arg = [lt|#{configPrefix c}_#{name}_from_msgpack(&#{o}, &#{arg});|]

fromMsgpack c o (TList t) d arg = [lt|#{lis_len} = #{o}.via.array.size;
for (int _i = 0; _i < #{o}.via.array.size; ++_i) {
#{indent4 $ fromMsgpack c o t d ith_val}}|]
  where
    lis_len = T.append arg (T.pack "_len")
    ith_val = T.append arg (T.pack "[_i]")


fromMsgpack c o (TPointer t) d arg = [lt|if (#{o}.type == MSGPACK_OBJECT_NIL) {
    #{arg} = NULL;
} else {
#{indent4 $ fromMsgpack' t}}|]
  where
    fromMsgpack' (TUserDef name _) = [lt|#{configPrefix c}_#{name}_from_msgpack(&#{o}, #{arg});|]
    fromMsgpack' t = fromMsgpack c o t d (T.cons '*' arg)
    valsize (TInt _ _) = [lt|sizeof(#{o}.via.i64)|]
    valsize (TFloat _) = [lt|sizeof(#{o}.via.dec)|]
    valsize TBool = [lt|sizeof(#{o}.via.boolean)|]
    valsize TString = [lt|#{o}.via.raw.size|]
    valsize TRaw = [lt|#{o}.via.raw.size|]
    valsize (TUserDef name _) = [lt|sizeof(#{name})|]
    valsize _ = LT.pack "0"


fromMsgpack _ _ t _ _ = [lt|/* #{show t} unsupported */|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
// This file is auto-generated from #{filepath}

#{content}
|]

indent4 :: LT.Text -> LT.Text
indent4 s =  LT.unlines $ map (LT.append (LT.pack "    ")) $ LT.split (\ c -> c == '\n') s
