-- This module is a convenience layer upon the Registry module which knows
-- nothing about GL/GLX/EGL/WGL-specific things.
module MangledRegistry
  ( ToEnumType
  , parseRegistry
  , Registry(..)
  , Type(..)
  , Group(..)
  , Enum'(..)
  , R.TypeName(..)
  , R.TypeSuffix(..)
  , Command(..)
  , commandName
  , SignatureElement(..)
  , Modification(..)
  , R.ModificationKind(..)
  , R.ProfileName(..)
  , Extension(..)
  , ConditionalModification(..)
  , InterfaceElement(..)
  , GroupName(..)
  , EnumName(..)
  , EnumValue(..)
  , CommandName(..)
  , ExtensionName(..)
  , API(..)
  , Version(..)
  , splitWords
  , joinWords
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Set as S
import qualified Numeric as N

import qualified DeclarationParser as D
import qualified Registry as R

type ToEnumType
   = Maybe String -> Maybe String -> Maybe String -> Maybe R.TypeSuffix -> String -> R.TypeName

parseRegistry :: ToEnumType -> String -> Either String Registry
parseRegistry toEnumType str = toRegistry toEnumType `fmap` R.parseRegistry str

data Registry = Registry
  { types :: M.Map R.TypeName Type
  , groups :: M.Map GroupName Group
  , enums :: M.Map EnumName [Enum']
  , commands :: M.Map CommandName Command
  , features :: M.Map (API, Version) [Modification]
  , extensions :: [Extension]
  } deriving (Eq, Ord, Show)

toRegistry :: ToEnumType -> R.Registry -> Registry
toRegistry toEnumType r =
  Registry
  { types =
      fromList'
        [(typeNameOf t, toType t) | R.TypesElement te <- rs, t <- R.unTypes te]
  , groups =
      fromList'
        [ (GroupName . R.unName . R.groupName $ g, toGroup g)
        | R.GroupsElement ge <- rs
        , g <- R.unGroups ge
        ]
  , enums =
      M.fromListWith
        (++)
        [ (enumName en, [en])
        | R.EnumsElement ee <- rs
        , Left e <- R.enumsEnumOrUnuseds ee
        , let en =
                toEnum'
                  (toEnumType
                     (R.enumsNamespace ee)
                     (R.enumsGroup ee)
                     (R.enumsType ee))
                  e
        ]
  , commands =
      fromList'
        [ (CommandName . R.protoName . R.commandProto $ c, toCommand c)
        | R.CommandsElement ce <- rs
        , c <- R.commandsCommands ce
        ]
  , features =
      fromList'
        [ ( (API (R.featureAPI f), read (R.featureNumber f))
          , map toModification (R.featureModifications f))
        | R.FeatureElement f <- rs
        ]
  , extensions =
      [ toExtension x
      | R.ExtensionsElement ee <- R.unRegistry r
      , x <- R.unExtensions ee
      ]
  }
  where
    rs = R.unRegistry r

fromList' :: (Ord k, Show a) => [(k, a)] -> M.Map k a
fromList' =
  M.fromListWith (\n o -> error $ "clash for " ++ show n ++ " and " ++ show o)

typeNameOf :: R.Type -> R.TypeName
typeNameOf t =
  case (R.typeName1 t, R.typeName2 t) of
    (Nothing, Nothing) -> error ("missing type name in " ++ show t)
    (Just n1, Nothing) -> n1
    (Nothing, Just n2) -> n2
    (Just n1, Just n2)
      | n1 == n2 -> n1
      | otherwise -> error ("conflicting type name in " ++ show t)

data Type = Type
  { typeAPI :: Maybe API
  , typeRequires :: Maybe R.TypeName
  } deriving (Eq, Ord, Show)

toType :: R.Type -> Type
toType t =
  Type
  { typeAPI = API `fmap` R.typeAPI t
  , typeRequires = R.TypeName `fmap` R.typeRequires t
  }

newtype Group = Group
  { groupEnums :: [EnumName]
  } deriving (Eq, Ord, Show)

toGroup :: R.Group -> Group
toGroup g = Group {groupEnums = map (EnumName . R.unName) (R.groupEnums g)}

-- NOTE: Due to an oversight in the OpenGL ES spec, an enum can have different
-- values for different APIs (happens only for GL_ACTIVE_PROGRAM_EXT).
data Enum' = Enum
  { enumValue :: EnumValue
  , enumAPI :: Maybe API
  , enumType :: R.TypeName
  , enumName :: EnumName
  } deriving (Eq, Ord, Show)

toEnum' :: (Maybe R.TypeSuffix -> String -> R.TypeName) -> R.Enum' -> Enum'
toEnum' toTypeName e =
  Enum
  { enumValue = EnumValue (R.enumValue e)
  , enumAPI = API `fmap` R.enumAPI e
  , enumType = toTypeName (R.enumType e) (R.enumName e)
  , enumName = EnumName (R.enumName e)
  }

splitChar :: Char
splitChar = '_'

splitWords :: String -> [String]
splitWords = splitBy (== splitChar)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs =
  case break p xs of
    (ys, []) -> [ys]
    (ys, _:zs) -> ys : splitBy p zs

joinWords :: [String] -> String
joinWords = L.intercalate [splitChar]

data Command = Command
  { resultType :: SignatureElement
  , paramTypes :: [SignatureElement]
  , referencedTypes :: S.Set R.TypeName
  , vecEquiv :: Maybe CommandName
  , alias :: Maybe CommandName
  } deriving (Eq, Ord, Show)

toCommand :: R.Command -> Command
toCommand c =
  Command
  { resultType = resTy
  , paramTypes = paramTys
  , referencedTypes =
      S.fromList $
    -- Make sure that we don't reference pointers to structs, they are mapped to
    -- 'Ptr a' etc., anyway (glCreateSyncFromCLeventARB is an exmaple for this).
      filter (not . ("struct " `L.isPrefixOf`) . R.unTypeName) $
      DM.mapMaybe (R.protoPtype . R.paramProto) (pr : ps)
  , vecEquiv = (CommandName . R.unName) `fmap` R.commandVecEquiv c
  , alias = (CommandName . R.unName) `fmap` R.commandAlias c
  }
  where
    pr = R.Param {R.paramLen = Nothing, R.paramProto = R.commandProto c}
    ps = R.commandParams c
    varSupply = map (R.TypeName . showIntUsingDigits ['a' .. 'z']) [0 ..]
    (resTy:paramTys) = snd $ L.mapAccumL toSignatureElement varSupply (pr : ps)

showIntUsingDigits :: String -> Int -> String
showIntUsingDigits ds x = N.showIntAtBase (length ds) (ds !!) x ""

commandName :: Command -> CommandName
commandName = CommandName . signatureElementName . resultType

data SignatureElement = SignatureElement
  { arrayLength :: Maybe String
  , belongsToGroup :: Maybe GroupName
  , baseType :: R.TypeName
  , numPointer :: Int
  , signatureElementName :: String
  } deriving (Eq, Ord)

instance Show SignatureElement where
  showsPrec d ct
    | numPointer ct == 0 = showString (R.unTypeName (baseType ct))
    | otherwise =
      showParen (d > 10) $
      showString "Ptr " . showsPrec 11 (ct {numPointer = numPointer ct - 1})
  showList = flip . foldr $ \x -> shows x . showString " -> "

-- We want to get 'Ptr a' instead of 'Ptr ()', so we might have to rename.
toSignatureElement ::
     [R.TypeName] -> R.Param -> ([R.TypeName], SignatureElement)
toSignatureElement varSupply param =
  either
    error
    (\(b, n) ->
       renameIf
         (b == "()" && n > 0)
         varSupply
         SignatureElement
         { arrayLength = R.paramLen param
         , belongsToGroup = GroupName `fmap` R.protoGroup proto
         , numPointer = n
         , baseType = R.TypeName b
         , signatureElementName = R.protoName proto
         }) $
  D.parse $
  unwords $
  map
    ($ proto)
    [ R.protoText1
    , maybe "" R.unTypeName . R.protoPtype
    , R.protoText2
    , R.protoName
    , R.protoText3
    ]
  where
    proto = R.paramProto param

renameIf ::
     Bool
  -> [R.TypeName]
  -> SignatureElement
  -> ([R.TypeName], SignatureElement)
renameIf False varSupply ct = (varSupply, ct)
renameIf True varSupply ct = (tail varSupply, ct {baseType = head varSupply})

data Modification = Modification
  { modificationKind :: R.ModificationKind
  , modificationProfile :: Maybe R.ProfileName
  , modificationInterfaceElements :: [InterfaceElement]
  } deriving (Eq, Ord, Show)

toModification :: R.Modification -> Modification
toModification m =
  Modification
  { modificationKind = R.modificationModificationKind m
  , modificationProfile = R.modificationProfileName m
  , modificationInterfaceElements =
      map toInterfaceElement (R.modificationInterfaceElements m)
  }

data Extension = Extension
  { extensionName :: ExtensionName
  , extensionSupported :: Maybe [API]
  , extensionsRequireRemove :: [ConditionalModification]
  } deriving (Eq, Ord, Show)

toExtension :: R.Extension -> Extension
toExtension e =
  Extension
  { extensionName = toExtensionName $ R.extensionName e
  , extensionSupported = supp `fmap` R.extensionSupported e
  , extensionsRequireRemove =
      map toConditionalModification (R.extensionsRequireRemove e)
  }
  where
    supp = map API . splitBy (== '|') . R.unStringGroup

data ConditionalModification = ConditionalModification
  { conditionalModificationAPI :: Maybe API
  , conditionalModificationModification :: Modification
  } deriving (Eq, Ord, Show)

toConditionalModification ::
     R.ConditionalModification -> ConditionalModification
toConditionalModification c =
  ConditionalModification
  { conditionalModificationAPI = API `fmap` R.conditionalModificationAPI c
  , conditionalModificationModification =
      toModification (R.conditionalModificationModification c)
  }

data InterfaceElement
  = TypeElement R.TypeName
  | EnumElement EnumName
  | CommandElement CommandName
  deriving (Eq, Ord, Show)

toInterfaceElement :: R.InterfaceElement -> InterfaceElement
toInterfaceElement i =
  (case R.interfaceElementKind i of
     R.InterfaceElementType -> TypeElement . R.TypeName
     R.InterfaceElementEnum -> EnumElement . EnumName
     R.InterfaceElementCommand -> CommandElement . CommandName)
    (R.unName (R.interfaceElementName i))

newtype GroupName = GroupName
  { unGroupName :: String
  } deriving (Eq, Ord, Show)

newtype EnumName = EnumName
  { unEnumName :: String
  } deriving (Eq, Ord, Show)

-- Conceptually EnumValue should be an Integer, but the registry cheats a bit:
--
--   * xsd:decimal doesn't allow hex notation, which is used everywhere.
--   * egl.xml uses expression strings like "((EGLint)-1)".
--   * glx.xml uses "&quot;GLX&quot;", totally abusing it.
newtype EnumValue = EnumValue
  { unEnumValue :: String
  } deriving (Eq, Ord, Show)

newtype CommandName = CommandName
  { unCommandName :: String
  } deriving (Eq, Ord, Show)

-- See https://www.opengl.org/registry/doc/rules.html#spec_naming
data ExtensionName = ExtensionName
  { extensionNameAPI :: String
  , extensionNameCategory :: String
  , extensionNameName :: String
  } deriving (Eq, Ord, Show)

toExtensionName :: R.Name -> ExtensionName
toExtensionName name =
  ExtensionName
  { extensionNameAPI = a
  , extensionNameCategory = c
  , extensionNameName = joinWords rest
  }
  where
    (a:c:rest) = splitWords (R.unName name)

newtype API = API
  { unAPI :: String
  } deriving (Eq, Ord, Show)

data Version = Version
  { major :: Int
  , minor :: Int
  } deriving (Eq, Ord)

instance Show Version where
  showsPrec _ v = shows (major v) . showChar '.' . shows (minor v)

instance Read Version where
  readsPrec _ s =
    [ (Version ma mi, r3)
    | (ma, r1) <- N.readDec s
    , ('.':r2) <- [r1]
    , (mi, r3) <- N.readDec r2
    ]
