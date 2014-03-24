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
--  * list and map value initialization
--  * destructor
--  * enum type initialization

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

#{LT.concat $ map genMsgInitFuncDecl ss }

#{LT.concat $ map genFuncDecl ss }

#endif /* _#{tag}_H_ */
|]

  LT.writeFile (name ++ "_types.c") $ templ configFilePath [lt|
#include <string.h>
#include <msgpack.h>
#include "#{name}_types.h"

#{LT.concat $ map (genMsgInitFuncImpl spec) ss }

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

genMsgInitFuncDecl :: Decl -> LT.Text
genMsgInitFuncDecl MPMessage {..} = [lt|
#{msgName}* #{msgName}_init(#{msgName}* arg);
void #{msgName}_destroy(#{msgName} *arg);|]

genMsgInitFuncDecl _ = ""

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

    msgpack_object *o = obj->via.array.ptr;
#{LT.concat $ map unpk $ zip nums msgFields}
    return 0;
}
|]
  where
  pk Field {..} = indent 4 $
    if isUserDef spec fldType
    then [lt|#{toMsgpack (TInt False 32) (T.append (T.pack "arg->") fldName)}|]
    else [lt|#{toMsgpack fldType (T.append (T.pack "arg->") fldName)}|]

  unpk (idx, Field {..}) = indent 4 $
    [lt|#{fromMsgpack spec (T.pack $ printf "o[%d]" idx) fldType fldDefault (T.append (T.pack "arg->") fldName)}|]
    if isUserDef spec fldType
    then [lt|#{fromMsgpack (T.pack $ printf "o[%d]" idx) (TInt False 32) fldDefault (T.append (T.pack "arg->") fldName)}|]
    else [lt|#{fromMsgpack (T.pack $ printf "o[%d]" idx) fldType fldDefault (T.append (T.pack "arg->") fldName)}|]

  nums :: [Integer]
  nums = [0..]

genFuncImpl _ MPEnum {..} = ""  -- enum is inline
genFuncImpl _ x = [lt|/* #{show x} unsupported */|]


genMsgInitFuncImpl :: Spec -> Decl -> LT.Text
genMsgInitFuncImpl spec MPMessage {..} = [lt|
#{msgName}* #{msgName}_init(#{msgName}* arg) {
    memset(arg, 0, sizeof(#{msgName}));
#{LT.concat $ map (indent 4 . genInitField) filteredFields}}

void #{msgName}_destroy(#{msgName} *arg) {
// TODO
}
|]
  where
    genInitField Field {..} = genInitField' (T.pack "arg") fldType fldDefault fldName
    filteredFields = filter bySpec msgFields
    bySpec Field {..}
      | fldDefault /= Nothing = True
      | otherwise             = case fldType of
        (TUserDef _ _) -> isUserDef spec fldType
        _ -> False
    
genMsgInitFuncImpl _ _ = ""

genInitField' :: T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
genInitField' arg _ (Just dflt) fldName = [lt|#{arg}->#{fldName} = #{genLiteral dflt};|]
genInitField' arg (TUserDef msgName _) _ fldName = [lt|#{msgName}_init(&#{arg}->#{fldName});|]
genInitField' _ _ _ _ = ""

genLiteral :: Literal -> LT.Text
genLiteral (LInt i) = [lt|#{show i}|]
genLiteral (LFloat d) = [lt|#{show d}|]
genLiteral (LBool b) = [lt|#{show b}|]
genLiteral LNull = [lt|NULL|]
genLiteral (LString s) = [lt|#{show s}|]


toMsgpack :: Type -> T.Text -> LT.Text
toMsgpack (TInt _ _) arg = [lt|msgpack_pack_int(pk, #{arg});|]
toMsgpack (TFloat _) arg = [lt|msgpack_pack_double(pk, #{arg});|]
toMsgpack TBool arg = [lt|#{arg} ? msgpack_pack_true(pk) : msgpack_pack_false(pk);|]
toMsgpack TString arg = [lt|do {
    size_t _l = strlen(#{arg});
    msgpack_pack_raw(pk, _l);
    msgpack_pack_raw_body(pk, #{arg}, _l);
} while(0);|]

toMsgpack TRaw arg = [lt|msgpack_pack_raw(pk, #{raw_size});
msgpack_pack_raw_body(pk, #{arg}, #{raw_size});|]
  where
    raw_size = T.append arg (T.pack "_size")

toMsgpack (TList t) arg = [lt|msgpack_pack_array(pk, #{lis_size});
for (int _i = 0; i < #{lis_size}; ++_i) {
#{indent 4 $ toMsgpack t ith_val}}|]
  where
    lis_size = T.append arg (T.pack "_size")
    ith_val = T.append arg (T.pack "[_i]")


toMsgpack (TMap t1 t2) arg = [lt|msgpack_pack_map(pk, #{map_size});
for (int _i = 0; i < #{map_size}; ++_i) {
#{indent 4 $ toMsgpack t1 ith_key}#{indent 4 $ toMsgpack t2 ith_val}}|]
  where
    map_size = T.append arg (T.pack "_size")
    ith_key = T.append (T.cons '&' arg) (T.pack "_keys[_i]")
    ith_val = T.append (T.cons '&' arg) (T.pack "_vals[_i]")

toMsgpack (TNullable t) arg = [lt|if (!#{arg}) {
    msgpack_pack_null(pk);
} else {
#{indent 4 $ toMsgpack t arg}}|]


toMsgpack (TUserDef name _) arg = [lt|#{name}_to_msgpack(pk, &#{arg});|]
toMsgpack t _ = [lt|msgpack_pack_nil(pk); /* #{show t} unsupported */|]


fromMsgpack :: T.Text -> Type -> Maybe Literal -> T.Text -> LT.Text
fromMsgpack o (TInt _ _) _ arg = [lt|#{arg} = #{o}.via.i64;|]
fromMsgpack o (TFloat _) _ arg = [lt|#{arg} = #{o}.via.dec;|]
fromMsgpack o TBool _ arg = [lt|#{arg} = #{o}.via.boolean;|]
fromMsgpack o TString _ arg = [lt|#{arg} = strdup(#{o}.via.raw.ptr);|]
fromMsgpack o (TUserDef name _) _ arg = [lt|#{name}_from_msgpack(&#{o}, &#{arg});|]

fromMsgpack o (TNullable t) d arg = [lt|if (#{o}.type == MSGPACK_OBJECT_NIL) {
    #{arg} = NULL;
} else {
#{indent 4 $ fromMsgpack o t d arg}}|]

fromMsgpack o TRaw _ arg = [lt|#{arg} = malloc(#{o}.via.raw.size);
if (#{arg} == NULL) {
    return -1;
}
#{raw_size} = #{o}.via.raw.size;
memcpy(#{arg}, #{o}.via.raw.ptr, #{o}.via.raw.size);|]
  where
    raw_size = T.append arg (T.pack "_size")

fromMsgpack o (TList t) d arg = [lt|#{arg} = malloc(#{o}.via.array.size * #{typeSize t});
if (#{arg} == NULL) {
    return -1;
}
#{lis_size} = #{o}.via.array.size;
for (int _i = 0; _i < #{o}.via.array.size; ++_i) {
#{indent 4 $ fromMsgpack (T.pack $ printf "%s.via.array.ptr[_i]" $ T.unpack o) t d ith_val}}|]
  where
    lis_size = T.append arg (T.pack "_size")
    ith_val = T.append arg (T.pack "[_i]")

fromMsgpack o (TMap t1 t2) d arg = [lt|#{key_arg} = malloc(#{o}.via.map.size * #{typeSize t1});
#{val_arg} = malloc(#{o}.via.map.size * #{typeSize t2});
if (#{key_arg} == NULL || #{val_arg} == NULL) {
    return -1;
}
#{map_size} = #{o}.via.map.size;
for (int _i = 0; _i < #{o}.via.map.size; ++_i) {
#{indent 4 $ fromMsgpack (T.pack $ printf "%s.via.map.ptr[_i].key" $ T.unpack o) t1 d ith_key}#{indent 4 $ fromMsgpack (T.pack $ printf "%s.via.map.ptr[_i].val" $ T.unpack o) t2 d ith_val}}|]
  where
    map_size = T.append arg $ T.pack "_size"
    key_arg = T.append arg $ T.pack "_keys"
    val_arg = T.append arg $ T.pack "_vals"
    ith_key = T.append key_arg $ T.pack "[_i]"
    ith_val = T.append val_arg $ T.pack "[_i]"

fromMsgpack _ t _ _ = [lt|/* #{show t} unsupported */|]

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

isUserDef :: Spec -> Type -> Bool
isUserDef spec (TUserDef name _) = not $ elem name $ map enumName $ filter isEnumDecl spec
isUserDef _ _ = False

typeSize :: Type -> LT.Text
typeSize (TUserDef name _) = [lt|sizeof(#{name})|]
typeSize TString = [lt|sizeof(void*)|]
typeSize t = error $ show t

