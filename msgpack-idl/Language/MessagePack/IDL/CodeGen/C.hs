{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.C (
  Config(..),
  generate,
  ) where

import Text.Printf
import Data.List (sortBy)
import Data.Function
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
      tag = T.toUpper $ T.pack configPrefix
      ss = sortSpec spec

  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile (configPrefix ++ "_types.h") $ templ configFilePath [lt|
#ifndef _#{tag}_H_
#define _#{tag}_H_

#include <msgpack.h>

#{LT.concat $ map (genTypeDecl c) ss }

#{LT.concat $ map (genFuncDecl c) ss }

#endif /* _#{tag}_H_ */
|]

  LT.writeFile (configPrefix ++ "_types.c") $ templ configFilePath [lt|
#include <string.h>
#include <msgpack.h>
#include "#{configPrefix}_types.h"

#{LT.concat $ map (genFuncImpl c) ss }

|]

genTypeDecl :: Config -> Decl -> LT.Text
genTypeDecl c MPMessage {..} = [lt|
typedef struct {
#{LT.concat $ map (indent 4 . genField c) msgFields }} #{msgName};
|]

genTypeDecl c MPEnum {..} = [lt|
typedef enum {
#{LT.concat $ map (indent 4 . genEnumField c) $ sortedEnumMem }} #{enumName};
|]
  where
    sortedEnumMem = sortBy (compare `on` fst) enumMem

genTypeDecl _ s = [lt|
/* #{show s} unsupported */
|]

genField :: Config -> Field -> LT.Text
genField _ Field {..} = [lt|#{genField' fldType fldName}|]

genField' :: Type -> T.Text -> LT.Text
genField' (TInt _ _) name = [lt|int64_t #{name};|]
genField' (TFloat _) name = [lt|double #{name};|]
genField' TBool name = [lt|bool #{name};|]
genField' TString name = [lt|char *#{name};|]
genField' (TUserDef typeName _) name = [lt|#{typeName} #{name};|]
genField' TRaw name = [lt|size_t #{name}_size;
void *#{name};|]

genField' (TList t) name = [lt|size_t #{name}_size;
#{genField' t name}|]

genField' (TMap t1 t2) name = [lt|size_t #{name}_size;
#{genField' t1 keys_field }
#{genField' t2 vals_field }|]
  where
    keys_field = T.cons '*' (T.append name $ T.pack "_keys")
    vals_field = T.cons '*' (T.append name $ T.pack "_vals")

genField' t name = [lt|/* #{show t}: #{name} unsupported */|]

genEnumField :: Config -> (Int, T.Text) -> LT.Text
genEnumField c (num, name) = [lt|#{name} = #{num},|]


genFuncDecl :: Config -> Decl -> LT.Text
genFuncDecl c MPMessage {..} = [lt|
int #{configPrefix c}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg);
int #{configPrefix c}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg);|]

genFuncDecl c MPEnum {..} = [lt|

inline int #{configPrefix c}_#{enumName}_to_msgpack(msgpack_packer *pk, #{enumName} arg) {
    return msgpack_pack_int(pk, arg);
}

inline int #{configPrefix c}_#{enumName}_from_msgpack(msgpack_object *obj, #{enumName} *arg) {
    *arg = obj->via.i64;
    return 0;
}
|]

genFuncDecl _ _ = ""

-- TODO: error check
genFuncImpl :: Config -> Decl -> LT.Text
genFuncImpl c MPMessage {..} = [lt|
int #{configPrefix c}_#{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg) {
    msgpack_pack_array(pk, #{length msgFields});
#{LT.concat $ map pk msgFields}
    return 0;
}

int #{configPrefix c}_#{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size != #{length msgFields}) {
        return -1;
    }

    msgpack_object *o = obj->via.array.ptr;
#{LT.concat $ map unpk $ zip nums msgFields}
    return 0;
}
|]
  where
  pk Field {..} = indent 4
    [lt|#{toMsgpack c fldType (T.append (T.pack "arg->") fldName)}|]

  unpk (idx, Field {..}) = indent 4
    [lt|#{fromMsgpack c (T.pack $ printf "o[%d]" idx) fldType fldDefault (T.append (T.pack "arg->") fldName)}|]

  nums :: [Integer]
  nums = [0..]

genFuncImpl _ MPEnum {..} = ""  -- enum is inline
genFuncImpl _ d = "/* #{show d} unsupported */"


toMsgpack :: Config -> Type -> T.Text -> LT.Text
toMsgpack _ (TInt _ _) arg = [lt|msgpack_pack_int(pk, #{arg});|]
toMsgpack _ (TFloat _) arg = [lt|msgpack_pack_double(pk, #{arg});|]
toMsgpack _ TBool arg = [lt|#{arg} ? msgpack_pack_true(pk) : msgpack_pack_false(pk)});|]
toMsgpack _ TString arg = [lt|do {
    size_t _l = strlen(#{arg});
    msgpack_pack_raw(pk, _l);
    msgpack_pack_raw_body(pk, #{arg}, _l);
} while(0);|]

toMsgpack c (TList t) arg = [lt|msgpack_pack_array(pk, #{lis_size});
for (int _i = 0; i < #{lis_size}; ++_i) {
#{indent 4 $ toMsgpack c t ith_val}}|]
  where
    lis_size = T.append arg (T.pack "_size")
    ith_val = T.append arg (T.pack "[_i]")


toMsgpack c (TMap t1 t2) arg = [lt|msgpack_pack_map(pk, #{map_size});
for (int _i = 0; i < #{map_size}; ++_i) {
#{indent 4 $ toMsgpack c t1 ith_key}#{indent 4 $ toMsgpack c t2 ith_val}}|]
  where
    map_size = T.append arg (T.pack "_size")
    ith_key = T.append (T.cons '&' arg) (T.pack "_keys[_i]")
    ith_val = T.append (T.cons '&' arg) (T.pack "_vals[_i]")

toMsgpack c (TNullable t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
#{indent 4 $ toMsgpack c t arg}}|]

-- toMsgpack c (TPointer t) arg = [lt|if (!#{arg}) {
--     msgpack_pack_null(pk);
-- } else {
-- #{indent 4 $ toMsgpack' t}}|]
--   where
--     toMsgpack' (TUserDef name _) = [lt|#{configPrefix c}_#{name}_to_msgpack(pk, #{arg});|]
--     toMsgpack' t' = toMsgpack c t' (T.cons '*' arg)

toMsgpack c (TUserDef name _) arg = [lt|#{configPrefix c}_#{name}_to_msgpack(pk, &#{arg});|]
toMsgpack _ t _ = [lt|msgpack_pack_nil(pk); /* #{show t} unsupported */|]

-- TODO:
--   * set default
--   * malloc for pointer?

fromMsgpack :: Config -> T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
fromMsgpack _ o (TInt _ _) _ arg = [lt|#{arg} = #{o}.via.i64;|]
fromMsgpack _ o (TFloat _) _ arg = [lt|#{arg} = #{o}.via.dec;|]
fromMsgpack c o (TNullable t) d arg = fromMsgpack c o t d arg
fromMsgpack _ o TBool _ arg = [lt|#{arg} = #{o}.via.boolean;|]
fromMsgpack _ o TString _ arg = [lt|#{arg} = strdup(#{o}.via.raw.ptr);|]
fromMsgpack c o (TUserDef name _) _ arg = [lt|#{configPrefix c}_#{name}_from_msgpack(&#{o}, &#{arg});|]

fromMsgpack _ o TRaw _ arg = [lt|#{arg} = malloc(#{o}.via.raw.size);
if (#{arg} == NULL) {
    return -1;
}
#{raw_size} = #{o}.via.raw.size;
memcpy(#{arg}, #{o}.via.raw.ptr, #{o}.via.raw.size);|]
  where
    raw_size = T.append arg (T.pack "_size")

fromMsgpack c o (TList t) d arg = [lt|#{lis_size} = #{o}.via.array.size;
for (int _i = 0; _i < #{o}.via.array.size; ++_i) {
#{indent 4 $ fromMsgpack c (T.pack $ printf "%s.via.array.ptr[_i]" $ T.unpack o) t d ith_val}}|]
  where
    lis_size = T.append arg (T.pack "_size")
    ith_val = T.append arg (T.pack "[_i]")

fromMsgpack c o (TMap t1 t2) d arg = [lt|#{map_size} = #{o}.via.map.size;
for (int _i = 0; _i < #{o}.via.map.size; ++_i) {
#{indent 4 $ fromMsgpack c (T.pack $ printf "%s.via.map.ptr[_i].key" $ T.unpack o) t1 d ith_key}#{indent 4 $ fromMsgpack c (T.pack $ printf "%s.via.map.ptr[_i].val" $ T.unpack o) t2 d ith_val}}|]
  where
    map_size = T.append arg (T.pack "_size")
    ith_key = T.append arg (T.pack "_keys[_i]")
    ith_val = T.append arg (T.pack "_vals[_i]")


-- fromMsgpack c o (TPointer t) d arg = [lt|if (#{o}.type == MSGPACK_OBJECT_NIL) {
--     #{arg} = NULL;
-- } else {
-- #{indent 4 $ fromMsgpack' t}}|]
--   where
--     fromMsgpack' (TUserDef name _) = [lt|#{configPrefix c}_#{name}_from_msgpack(&#{o}, #{arg});|]
--     fromMsgpack' t' = fromMsgpack c o t' d (T.cons '*' arg)
--     --   for malloc
--     -- valsize (TInt _ _) = [lt|sizeof(#{o}.via.i64)|]
--     -- valsize (TFloat _) = [lt|sizeof(#{o}.via.dec)|]
--     -- valsize TBool = [lt|sizeof(#{o}.via.boolean)|]
--     -- valsize TString = [lt|#{o}.via.raw.size|]
--     -- valsize TRaw = [lt|#{o}.via.raw.size|]
--     -- valsize (TUserDef name _) = [lt|sizeof(#{name})|]
--     -- valsize _ = LT.pack "0"


fromMsgpack _ _ t _ _ = [lt|/* #{show t} unsupported */|]

templ :: FilePath -> LT.Text -> LT.Text
templ filepath content = [lt|
// This file is auto-generated from #{filepath}

#{content}
|]

indent :: Int -> LT.Text -> LT.Text
indent i s =  LT.unlines $ map (LT.append (LT.pack $ take i $ repeat ' ')) $ LT.split (\ c -> c == '\n') s

sortSpec :: Spec -> Spec
sortSpec d =
  sortBy compareSpec d
  where
    compareSpec MPEnum {..} _ = GT
    compareSpec _ MPEnum {..} = LT
    compareSpec _ _ = EQ
    
