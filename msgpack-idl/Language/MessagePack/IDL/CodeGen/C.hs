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


-- TODO
--  * destructor

data Config
  = Config
    { configFilePath :: FilePath
    }
  deriving (Show, Eq)

generate:: Config -> Spec -> IO ()
generate Config {..} spec = do
  let name = takeBaseName configFilePath
      tag = T.toUpper $ T.pack name
      ss = sortSpec spec

  createDirectoryIfMissing True (takeBaseName configFilePath);
  setCurrentDirectory (takeBaseName configFilePath);
  LT.writeFile (name ++ "_types.h") $ templ configFilePath [lt|
#ifndef _#{tag}_H_
#define _#{tag}_H_

#include <msgpack.h>

#{LT.concat $ map genTypeDecl ss }

#{LT.concat $ map genMsgCDtorDecl ss }

#{LT.concat $ map genFuncDecl ss }

#endif /* _#{tag}_H_ */
|]

  LT.writeFile (name ++ "_types.c") $ templ configFilePath [lt|
#include <string.h>
#include <msgpack.h>
#include "#{name}_types.h"

#{LT.concat $ map (genMsgCDtorImpl spec) ss }

#{LT.concat $ map (genFuncImpl spec) ss }

|]

genTypeDecl :: Decl -> LT.Text
genTypeDecl MPMessage {..} = [lt|
typedef struct {
#{LT.concat $ map (indent 4 . genField) msgFields }} #{msgName};
|]

genTypeDecl MPEnum {..} = [lt|
typedef enum {
#{LT.concat $ map (indent 4 . genEnumField) $ sortedEnumMem }} #{enumName};
|]
  where
    sortedEnumMem = sortBy (compare `on` fst) enumMem

genTypeDecl s = [lt|
/* #{show s} unsupported */
|]

genField :: Field -> LT.Text
genField Field {..} = [lt|#{genField' fldType fldName}|]

genField' :: Type -> T.Text -> LT.Text
genField' (TInt _ _) name = [lt|int64_t #{name};|]
genField' (TFloat _) name = [lt|double #{name};|]
genField' TBool name = [lt|bool #{name};|]
genField' TString name = [lt|char *#{name};|]
genField' (TUserDef typeName _) name = [lt|#{typeName} #{name};|]
genField' (TNullable t) name = genField' t name
genField' TRaw name = [lt|size_t #{name}_size;
void *#{name};|]

genField' (TList t) name = [lt|size_t #{name}_size;
#{genField' t vals_field}|]
  where
    vals_field = T.cons '*' name

genField' (TMap t1 t2) name = [lt|size_t #{name}_size;
#{genField' t1 keys_field }
#{genField' t2 vals_field }|]
  where
    keys_field = T.cons '*' (T.append name $ T.pack "_keys")
    vals_field = T.cons '*' (T.append name $ T.pack "_vals")

genField' t name = [lt|/* #{show t}: #{name} unsupported */|]

genEnumField :: (Int, T.Text) -> LT.Text
genEnumField (num, name) = [lt|#{name} = #{num},|]

genMsgCDtorDecl :: Decl -> LT.Text
genMsgCDtorDecl MPMessage {..} = [lt|
#{msgName}* #{msgName}_init(#{msgName}* arg);
void #{msgName}_destroy(#{msgName} *arg);|]

genMsgCDtorDecl _ = ""

genFuncDecl :: Decl -> LT.Text
genFuncDecl MPMessage {..} = [lt|
int #{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg);
int #{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg);|]

-- genFuncDecl MPEnum {..} = [lt|

-- inline int #{enumName}_to_msgpack(msgpack_packer *pk, #{enumName} arg) {
--     return msgpack_pack_int(pk, arg);
-- }

-- inline int #{enumName}_from_msgpack(msgpack_object *obj, #{enumName} *arg) {
--     *arg = obj->via.i64;
--     return 0;
-- }
-- |]

genFuncDecl _ = ""

-- TODO: error check
genFuncImpl :: Spec -> Decl -> LT.Text
genFuncImpl spec MPMessage {..} = [lt|
int #{msgName}_to_msgpack(msgpack_packer *pk, #{msgName} *arg) {
    msgpack_pack_array(pk, #{length msgFields});
#{LT.concat $ map pk msgFields}
    return 0;
}

int #{msgName}_from_msgpack(msgpack_object *obj, #{msgName} *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size != #{length msgFields}) {
        return -1;
    }

    #{msgName}_init(arg);
    msgpack_object *o = obj->via.array.ptr;
#{LT.concat $ map unpk $ zip nums msgFields}
    return 0;
}
|]
  where
  pk Field {..} = indent 4 $
    [lt|#{toMsgpack spec fldType (T.append (T.pack "arg->") fldName)}|]

  unpk (idx, Field {..}) = indent 4 $
    [lt|#{fromMsgpack spec (T.pack $ printf "o[%d]" idx) fldType fldDefault (T.append (T.pack "arg->") fldName)}|]

  nums :: [Integer]
  nums = [0..]

genFuncImpl _ MPEnum {..} = ""  -- enum is inline
genFuncImpl _ x = [lt|/* #{show x} unsupported */|]


genMsgCDtorImpl :: Spec -> Decl -> LT.Text
genMsgCDtorImpl spec MPMessage {..} = [lt|
#{msgName}* #{msgName}_init(#{msgName}* arg) {
    memset(arg, 0, sizeof(#{msgName}));
#{LT.concat $ map (indent 4 . genInitField) initFields}}

void #{msgName}_destroy(#{msgName} *arg) {
#{LT.concat $ map (indent 4 . genDtorCall) dtorFields}}
|]
  where
    genDtorCall Field {..} = genDtorCall' (T.pack "arg") fldType fldName
    genInitField Field {..} = genInitField' (T.pack "arg") fldType fldDefault fldName
    dtorFields = filter byUserDef msgFields
    initFields = filter bySpec msgFields
    bySpec Field {..}
      | fldDefault /= Nothing = True
      | otherwise             = case fldType of
        (TUserDef _ _) -> not $ isEnumType spec fldType
        _ -> False
    byUserDef Field {..} = case fldType of
      (TUserDef _ _) -> not $ isEnumType spec fldType
      _ -> False
    
genMsgCDtorImpl _ _ = ""

genInitField' :: T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
genInitField' arg _ (Just dflt) fldName = [lt|#{arg}->#{fldName} = #{genLiteral dflt};|]
genInitField' arg (TUserDef msgName _) _ fldName = [lt|#{msgName}_init(&#{arg}->#{fldName});|]
genInitField' _ _ _ _ = ""

genDtorCall' :: T.Text -> Type -> T.Text -> LT.Text
genDtorCall' arg (TUserDef name _) fldName = [lt|#{name}_destroy(&#{arg}->#{fldName});|]
-- genDtorCall' arg (TList t) fldName = [lt|if (#{arg}->#{fldName}
-- |]
genDtorCall' _ _ _ = ""

genLiteral :: Literal -> LT.Text
genLiteral (LInt i) = [lt|#{show i}|]
genLiteral (LFloat d) = [lt|#{show d}|]
genLiteral (LBool b) = [lt|#{show b}|]
genLiteral LNull = [lt|NULL|]
genLiteral (LString s) = [lt|#{show s}|]

toMsgpack :: Spec -> Type -> T.Text -> LT.Text
toMsgpack _ (TInt _ _) arg = [lt|msgpack_pack_int(pk, #{arg});|]
toMsgpack _ (TFloat _) arg = [lt|msgpack_pack_double(pk, #{arg});|]
toMsgpack _ TBool arg = [lt|#{arg} ? msgpack_pack_true(pk) : msgpack_pack_false(pk);|]
toMsgpack _ TString arg = [lt|do {
    size_t _l = strlen(#{arg});
    msgpack_pack_raw(pk, _l);
    msgpack_pack_raw_body(pk, #{arg}, _l);
} while(0);|]

toMsgpack _ TRaw arg = [lt|msgpack_pack_raw(pk, #{raw_size arg});
msgpack_pack_raw_body(pk, #{arg}, #{raw_size arg});|]

toMsgpack s (TList t) arg = [lt|msgpack_pack_array(pk, #{lis_size arg});
for (int _i = 0; i < #{lis_size arg}; ++_i) {
#{indent 4 $ toMsgpack s t $ lis_ith_val arg}}|]

toMsgpack s (TMap t1 t2) arg = [lt|msgpack_pack_map(pk, #{map_size arg});
for (int _i = 0; i < #{map_size arg}; ++_i) {
#{indent 4 $ toMsgpack s t1 $ map_ith_key arg}#{indent 4 $ toMsgpack s t2 $ map_ith_val arg}}|]

toMsgpack s (TNullable t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
#{indent 4 $ toMsgpack s t arg}}|]

toMsgpack s (TUserDef name _) arg =
  if elem name $ map enumName $ filter isEnumDecl s
  then toMsgpack s (TInt False 32) arg
  else [lt|#{name}_to_msgpack(pk, &#{arg});|]

toMsgpack _ t _ = [lt|msgpack_pack_nil(pk); /* #{show t} unsupported */|]


fromMsgpack :: Spec -> T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
fromMsgpack _ o (TInt _ _) _ arg = [lt|#{arg} = #{o}.via.i64;|]
fromMsgpack _ o (TFloat _) _ arg = [lt|#{arg} = #{o}.via.dec;|]
fromMsgpack _ o TBool _ arg = [lt|#{arg} = #{o}.via.boolean;|]
fromMsgpack _ o TString _ arg = [lt|#{arg} = strdup(#{o}.via.raw.ptr);|]
fromMsgpack s o (TUserDef name _) d arg =
  if elem name $ map enumName $ filter isEnumDecl s
  then fromMsgpack s o (TInt False 32) d arg
  else [lt|#{name}_from_msgpack(&#{o}, &#{arg});|]

fromMsgpack s o (TNullable t) d arg = [lt|if (#{o}.type == MSGPACK_OBJECT_NIL) {
    #{arg} = NULL;
} else {
#{indent 4 $ fromMsgpack s o t d arg}}|]

fromMsgpack _ o TRaw _ arg = [lt|#{arg} = malloc(#{o}.via.raw.size);
if (#{arg} == NULL) {
    return -1;
}
#{raw_size arg} = #{o}.via.raw.size;
memcpy(#{arg}, #{o}.via.raw.ptr, #{o}.via.raw.size);|]

fromMsgpack s o (TList t) d arg = [lt|#{arg} = malloc(#{o}.via.array.size * #{typeSize t});
if (#{arg} == NULL) {
    return -1;
}
#{lis_size arg} = #{o}.via.array.size;
for (int _i = 0; _i < #{o}.via.array.size; ++_i) {
#{indent 4 $ fromMsgpack s (T.pack $ printf "%s.via.array.ptr[_i]" $ T.unpack o) t d $ lis_ith_val arg}}|]

fromMsgpack s o (TMap t1 t2) d arg = [lt|#{map_keys arg} = malloc(#{o}.via.map.size * #{typeSize t1});
#{map_vals arg} = malloc(#{o}.via.map.size * #{typeSize t2});
if (#{map_keys arg} == NULL || #{map_vals arg} == NULL) {
    return -1;
}
#{map_size arg} = #{o}.via.map.size;
for (int _i = 0; _i < #{o}.via.map.size; ++_i) {
#{indent 4 $ fromMsgpack s (T.pack $ printf "%s.via.map.ptr[_i].key" $ T.unpack o) t1 d $ map_ith_key arg}#{indent 4 $ fromMsgpack s (T.pack $ printf "%s.via.map.ptr[_i].val" $ T.unpack o) t2 d $ map_ith_val arg}}|]

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
    
isEnumDecl :: Decl -> Bool
isEnumDecl MPEnum {..} = True
isEnumDecl _ = False

isEnumType :: Spec -> Type -> Bool
isEnumType spec (TUserDef name _) = elem name $ map enumName $ filter isEnumDecl spec
isEnumType _ _ = False

typeSize :: Type -> LT.Text
typeSize (TUserDef name _) = [lt|sizeof(#{name})|]
typeSize TString = [lt|sizeof(void*)|]
typeSize t = error $ show t

raw_size :: T.Text -> T.Text
raw_size arg = T.append arg (T.pack "_size")

lis_size :: T.Text -> T.Text
lis_size arg = T.append arg (T.pack "_size")

lis_ith_val :: T.Text -> T.Text
lis_ith_val arg = T.append arg (T.pack "[_i]")

map_size :: T.Text -> T.Text
map_size arg = T.append arg (T.pack "_size")

map_ith_key :: T.Text -> T.Text
map_ith_key arg = T.append arg (T.pack "_keys[_i]")

map_ith_val :: T.Text -> T.Text
map_ith_val arg = T.append arg (T.pack "_vals[_i]")

map_keys :: T.Text -> T.Text
map_keys arg = T.append arg (T.pack "_keys")

map_vals :: T.Text -> T.Text
map_vals arg = T.append arg (T.pack "_vals")
