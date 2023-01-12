-- This module contains a 1:1 translation of registry.rnc with an (un-)parser.
-- No simplifications or GL/GLX/EGL/WGL-specific things are done here.
module Registry
  ( parseRegistry
  , unparseRegistry
  , Registry(..)
  , RegistryElement(..)
  , Types(..)
  , Type(..)
  , Groups(..)
  , Group(..)
  , Enums(..)
  , Enum'(..)
  , Unused(..)
  , Commands(..)
  , Command(..)
  , Proto(..)
  , Param(..)
  , GLX(..)
  , Feature(..)
  , Modification(..)
  , ModificationKind(..)
  , Extensions(..)
  , Extension(..)
  , ConditionalModification(..)
  , InterfaceElement(..)
  , InterfaceElementKind(..)
  , Integer'(..)
  , TypeName(..)
  , TypeSuffix(..)
  , StringGroup(..)
  , ProfileName(..)
  , Vendor(..)
  , Comment(..)
  , Name(..)
  ) where

import Data.Maybe (maybeToList)
import Text.XML.HXT.Core

parseRegistry :: String -> Either String Registry
parseRegistry =
  head .
  runLA
    (xreadDoc >>>
     neg isXmlPi >>>
     removeAllWhiteSpace >>>
     canonicalizeAllNodes >>> -- needed to e.g. process CDATA, remove XML comments, etc.
     arr (unpickleDoc' xpRegistry))

unparseRegistry :: Registry -> String
unparseRegistry =
  concat .
  (pickleDoc xpRegistry >>>
   runLA
     (writeDocumentToString
        [withIndent yes, withOutputEncoding utf8, withXmlPi yes]))

-- Note: We do this slightly different from the schema.
newtype Registry = Registry
  { unRegistry :: [RegistryElement]
  } deriving (Eq, Ord, Show)

xpRegistry :: PU Registry
xpRegistry =
  xpWrap (Registry, unRegistry) $ xpElem "registry" $ xpList xpRegistryElement

data RegistryElement
  = CommentElement { unCommentElement :: Comment }
  | TypesElement { unTypesElement :: Types }
  | GroupsElement { unGroupsElement :: Groups }
  | EnumsElement { unEnumsElement :: Enums }
  | CommandsElement { unCommandsElement :: Commands }
  | FeatureElement { unFeatureElement :: Feature }
  | ExtensionsElement { unExtensionsElement :: Extensions }
  deriving (Eq, Ord, Show)

xpRegistryElement :: PU RegistryElement
xpRegistryElement = xpAlt tag pus
  where
    tag (CommentElement _) = 0
    tag (TypesElement _) = 1
    tag (GroupsElement _) = 2
    tag (EnumsElement _) = 3
    tag (CommandsElement _) = 4
    tag (FeatureElement _) = 5
    tag (ExtensionsElement _) = 6
    pus =
      [ xpWrap (CommentElement, unCommentElement) $ xpElem "comment" xpComment
      , xpWrap (TypesElement, unTypesElement) xpTypes
      , xpWrap (GroupsElement, unGroupsElement) xpGroups
      , xpWrap (EnumsElement, unEnumsElement) xpEnums
      , xpWrap (CommandsElement, unCommandsElement) xpCommands
      , xpWrap (FeatureElement, unFeatureElement) xpFeature
      , xpWrap (ExtensionsElement, unExtensionsElement) xpExtensions
      ]

newtype Types = Types
  { unTypes :: [Type]
  } deriving (Eq, Ord, Show)

xpTypes :: PU Types
xpTypes = xpWrap (Types, unTypes) $ xpElem "types" $ xpList xpType

data Type = Type
  { typeAPI :: Maybe String
  , typeRequires :: Maybe String
  , typeName1 :: Maybe TypeName
  , typeType :: Maybe String
  , typeComment :: Maybe Comment
  , typeText1 :: String
  , typeAPIEntry :: Maybe String
  , typeText2 :: String
  , typeName2 :: Maybe TypeName
  , typeText3 :: String
  } deriving (Eq, Ord, Show)

xpType :: PU Type
xpType =
  xpWrap
    ( \(a, b, c, d, e, f, g, h, i, j) -> Type a b c d e f g h i j
    , \(Type a b c d e f g h i j) -> (a, b, c, d, e, f, g, h, i, j)) $
  xpElem "type" $
  xp10Tuple
    (xpAttrImplied "api" xpText)
    (xpAttrImplied "requires" xpText)
    (xpAttrImplied "name" xpTypeName)
    (xpAttrImplied "type" xpText)
    (xpOption xpCommentAttr)
    xpText0
    (xpOption $ xpElem "apientry" xpText0)
    xpText0
    (xpOption $ xpElem "name" xpTypeName)
    xpText0

newtype Groups = Groups
  { unGroups :: [Group]
  } deriving (Eq, Ord, Show)

xpGroups :: PU Groups
xpGroups = xpWrap (Groups, unGroups) $ xpElem "groups" $ xpList xpGroup

data Group = Group
  { groupName :: Name
  , groupComment :: Maybe Comment
  , groupEnums :: [Name]
  } deriving (Eq, Ord, Show)

xpGroup :: PU Group
xpGroup =
  xpWrap (\(a, b, c) -> Group a b c, \(Group a b c) -> (a, b, c)) $
  xpElem "group" $
  xpTriple xpName (xpOption xpCommentAttr) (xpList $ xpElem "enum" xpName)

data Enums = Enums
  { enumsNamespace :: Maybe String
  , enumsGroup :: Maybe String
  , enumsType :: Maybe String
  -- enumsStart is conceptually a 'Maybe Integer', but egl.xml cheats a bit and
  -- uses e.g. 'start="0x3060-0x306F"' as a shorthand for
  -- 'start="0x3060" end="0x306F"'.
  , enumsStart :: Maybe Integer'
  , enumsEnd :: Maybe Integer'
  , enumsVendor :: Maybe Vendor
  , enumsComment :: Maybe Comment
  , enumsEnumOrUnuseds :: [Either Enum' Unused]
  } deriving (Eq, Ord, Show)

xpEnums :: PU Enums
xpEnums =
  xpWrap
    ( \(a, b, c, d, e, f, g, h) -> Enums a b c d e f g h
    , \(Enums a b c d e f g h) -> (a, b, c, d, e, f, g, h)) $
  xpElem "enums" $
  xp8Tuple
    (xpAttrImplied "namespace" xpText)
    (xpAttrImplied "group" xpText)
    (xpAttrImplied "type" xpText)
    (xpAttrImplied "start" xpInteger)
    (xpAttrImplied "end" xpInteger)
    (xpOption xpVendor)
    (xpOption xpCommentAttr)
    (xpList $ xpEither xpEnum xpUnused)

xpEither :: PU a -> PU b -> PU (Either a b)
xpEither pl pr = xpAlt tag pus
  where
    tag (Left _) = 0
    tag (Right _) = 1
    pus = [xpWrap (Left, \(Left l) -> l) pl, xpWrap (Right, \(Right r) -> r) pr]

data Enum' = Enum
  { enumValue :: String
  , enumAPI :: Maybe String
  , enumType :: Maybe TypeSuffix
  , enumName :: String
  , enumAlias :: Maybe String
  , enumComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

-- NOTE: The spec uses the interleave pattern, which is not needed: Attributes
-- are by definition unordered.
xpEnum :: PU Enum'
xpEnum =
  xpWrap
    ( \(a, b, c, d, e, f) -> Enum a b c d e f
    , \(Enum a b c d e f) -> (a, b, c, d, e, f)) $
  xpElem "enum" $
  xp6Tuple
    (xpAttr "value" xpText)
    (xpAttrImplied "api" xpText)
    (xpAttrImplied "type" xpTypeSuffix)
    (xpAttr "name" xpText)
    (xpAttrImplied "alias" xpText)
    (xpOption xpCommentAttr)

data Unused = Unused
  { unusedStart :: Integer'
  , unusedEnd :: Maybe Integer'
  , unusedVendor :: Maybe Vendor
  , unusedComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

xpUnused :: PU Unused
xpUnused =
  xpWrap (\(a, b, c, d) -> Unused a b c d, \(Unused a b c d) -> (a, b, c, d)) $
  xpElem "unused" $
  xp4Tuple
    (xpAttr "start" xpInteger)
    (xpAttrImplied "end" xpInteger)
    (xpOption xpVendor)
    (xpOption xpCommentAttr)

data Commands = Commands
  { commandsNamespace :: Maybe String
  , commandsCommands :: [Command]
  } deriving (Eq, Ord, Show)

xpCommands :: PU Commands
xpCommands =
  xpWrap (uncurry Commands, \(Commands a b) -> (a, b)) $
  xpElem "commands" $
  xpPair (xpAttrImplied "namespace" xpText) (xpList xpCommand)

data Command = Command
  { commandComment :: Maybe Comment
  , commandProto :: Proto
  , commandParams :: [Param]
  , commandAlias :: Maybe Name
  , commandVecEquiv :: Maybe Name
  , commandGLXs :: [GLX]
  } deriving (Eq, Ord, Show)

xpCommand :: PU Command
xpCommand =
  xpWrap
    ( \(a, b, c, (d, e, f)) -> Command a b c d e f
    , \(Command a b c d e f) -> (a, b, c, (d, e, f))) $
  xpElem "command" $
  xp4Tuple (xpOption xpCommentAttr) xpProto (xpList xpParam) xpCommandTail

-- The spec uses the interleave pattern here, which is not supported in hxt.
-- As a workaround, we use a list of disjoint types.
xpCommandTail :: PU (Maybe Name, Maybe Name, [GLX])
xpCommandTail =
  xpWrapEither
    ( \xs -> do
        a <- check "alias" [x | AliasElement x <- xs]
        b <- check "vecequiv" [x | VecEquivElement x <- xs]
        let c = [x | GLXElement x <- xs]
        return (a, b, c)
    , \(a, b, c) ->
        map AliasElement (maybeToList a) ++
        map VecEquivElement (maybeToList b) ++ map GLXElement c) $
  xpList $ xpAlt tag pus
  where
    tag (AliasElement _) = 0
    tag (VecEquivElement _) = 1
    tag (GLXElement _) = 2
    pus =
      [ xpWrap (AliasElement, unAliasElement) $ xpElem "alias" xpName
      , xpWrap (VecEquivElement, unVecEquivElement) $ xpElem "vecequiv" xpName
      , xpWrap (GLXElement, unGLXElement) xpGLX
      ]
    check n xs =
      case xs of
        [] -> Right Nothing
        [x] -> Right $ Just x
        _ -> Left $ "expected at most one '" ++ n ++ "' element"

data CommandTail
  = AliasElement { unAliasElement :: Name }
  | VecEquivElement { unVecEquivElement :: Name }
  | GLXElement { unGLXElement :: GLX }
  deriving (Eq, Ord, Show)

data Proto = Proto
  { protoGroup :: Maybe String
  , protoText1 :: String
  , protoPtype :: Maybe TypeName
  , protoText2 :: String
  , protoName :: String
  , protoText3 :: String
  } deriving (Eq, Ord, Show)

xpProto :: PU Proto
xpProto =
  xpWrap
    ( \(a, b, c, d, e, f) -> Proto a b c d e f
    , \(Proto a b c d e f) -> (a, b, c, d, e, f)) $
  xpElem "proto" $
  xp6Tuple
    (xpAttrImplied "group" xpText)
    xpText0
    (xpOption $ xpElem "ptype" xpTypeName)
    xpText0
    (xpElem "name" xpText)
    xpText0

data Param = Param
  { paramLen :: Maybe String
  , paramProto :: Proto
  } deriving (Eq, Ord, Show)

xpParam :: PU Param
xpParam =
  xpWrap
    ( \(b, a, c, d, e, f, g) -> Param a (Proto b c d e f g)
    , \(Param a (Proto b c d e f g)) -> (b, a, c, d, e, f, g)) $
  xpElem "param" $
  xp7Tuple
    (xpAttrImplied "group" xpText)
    (xpAttrImplied "len" xpText)
    xpText0
    (xpOption $ xpElem "ptype" xpTypeName)
    xpText0
    (xpElem "name" xpText)
    xpText0

data GLX = GLX
  { glxType :: String
  , glxOpcode :: Int
  , glxName :: Maybe Name
  , glxComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

xpGLX :: PU GLX
xpGLX =
  xpWrap (\(a, b, c, d) -> GLX a b c d, \(GLX a b c d) -> (a, b, c, d)) $
  xpElem "glx" $
  xp4Tuple
    (xpAttr "type" xpText)
    (xpAttr "opcode" xpInt)
    (xpOption xpName)
    (xpOption xpCommentAttr)

data Feature = Feature
  { featureAPI :: String
  , featureName :: Name
  , featureNumber :: String -- actually xsd:decimal, but used as a string
  , featureProtect :: Maybe String
  , featureComment :: Maybe Comment
  , featureModifications :: [Modification]
  } deriving (Eq, Ord, Show)

xpFeature :: PU Feature
xpFeature =
  xpWrap
    ( \(a, b, c, d, e, f) -> Feature a b c d e f
    , \(Feature a b c d e f) -> (a, b, c, d, e, f)) $
  xpElem "feature" $
  xp6Tuple
    (xpAttr "api" xpText)
    xpName
    (xpAttr "number" xpText)
    (xpAttrImplied "protect" xpText)
    (xpOption xpCommentAttr)
    (xpList xpModification)

data Modification = Modification
  { modificationModificationKind :: ModificationKind
  , modificationProfileName :: Maybe ProfileName
  , modificationComment :: Maybe Comment
  , modificationInterfaceElements :: [InterfaceElement]
  } deriving (Eq, Ord, Show)

data ModificationKind
  = Require
  | Remove -- TODO: Better name
  deriving (Eq, Ord, Show, Enum)

xpModification :: PU Modification
xpModification = xpAlt (fromEnum . modificationModificationKind) pus
  where
    pus = [xpMod "require" Require, xpMod "remove" Remove]
    xpMod el kind =
      xpWrap
        ( \(a, b, c) -> Modification kind a b c
        , \(Modification _ a b c) -> (a, b, c)) $
      xpElem el $
      xpTriple
        (xpOption xpProfileName)
        (xpOption xpCommentAttr)
        (xpList xpInterfaceElement)

newtype Extensions = Extensions
  { unExtensions :: [Extension]
  } deriving (Eq, Ord, Show)

xpExtensions :: PU Extensions
xpExtensions =
  xpWrap (Extensions, unExtensions) $ xpElem "extensions" $ xpList xpExtension

data Extension = Extension
  { extensionName :: Name
  , extensionProtect :: Maybe String
  , extensionSupported :: Maybe StringGroup
  , extensionComment :: Maybe Comment
  , extensionsRequireRemove :: [ConditionalModification]
  } deriving (Eq, Ord, Show)

xpExtension :: PU Extension
xpExtension =
  xpWrap
    ( \(a, b, c, d, e) -> Extension a b c d e
    , \(Extension a b c d e) -> (a, b, c, d, e)) $
  xpElem "extension" $
  xp5Tuple
    xpName
    (xpAttrImplied "protect" xpText)
    (xpAttrImplied "supported" xpStringGroup)
    (xpOption xpCommentAttr)
    (xpList xpConditionalModification)

data ConditionalModification = ConditionalModification
  { conditionalModificationAPI :: Maybe String
  , conditionalModificationModification :: Modification
  } deriving (Eq, Ord, Show)

xpConditionalModification :: PU ConditionalModification
xpConditionalModification =
  xpAlt
    (fromEnum .
     modificationModificationKind . conditionalModificationModification)
    pus
  where
    pus = [xpMod "require" Require, xpMod "remove" Remove]
    xpMod el kind =
      xpWrap
        ( \(a, b, c, d) -> ConditionalModification a (Modification kind b c d)
        , \(ConditionalModification a (Modification _ b c d)) -> (a, b, c, d)) $
      xpElem el $
      xp4Tuple
        (xpAttrImplied "api" xpText)
        (xpOption xpProfileName)
        (xpOption xpCommentAttr)
        (xpList xpInterfaceElement)

data InterfaceElement = InterfaceElement
  { interfaceElementKind :: InterfaceElementKind
  , interfaceElementName :: Name
  , interfaceElementComment :: Maybe Comment
  } deriving (Eq, Ord, Show)

data InterfaceElementKind
  = InterfaceElementType
  | InterfaceElementEnum
  | InterfaceElementCommand
  deriving (Eq, Ord, Show, Enum)

xpInterfaceElement :: PU InterfaceElement
xpInterfaceElement = xpAlt (fromEnum . interfaceElementKind) pus
  where
    pus =
      [ xpIE InterfaceElementType "type"
      , xpIE InterfaceElementEnum "enum"
      , xpIE InterfaceElementCommand "command"
      ]
    xpIE ty el =
      xpWrap
        (uncurry (InterfaceElement ty), \(InterfaceElement _ a b) -> (a, b)) $
      xpElem el $ xpPair xpName (xpOption xpCommentAttr)

newtype Integer' = Integer
  { unInteger :: String
  } deriving (Eq, Ord, Show)

xpInteger :: PU Integer'
xpInteger = xpWrap (Integer, unInteger) xpText

newtype TypeName = TypeName
  { unTypeName :: String
  } deriving (Eq, Ord, Show)

xpTypeName :: PU TypeName
xpTypeName = xpWrap (TypeName, unTypeName) xpText

newtype TypeSuffix = TypeSuffix
  { unTypeSuffix :: String
  } deriving (Eq, Ord, Show)

xpTypeSuffix :: PU TypeSuffix
xpTypeSuffix = xpWrap (TypeSuffix, unTypeSuffix) xpText

newtype StringGroup = StringGroup
  { unStringGroup :: String
  } deriving (Eq, Ord, Show)

xpStringGroup :: PU StringGroup
xpStringGroup = xpWrap (StringGroup, unStringGroup) xpText

newtype ProfileName = ProfileName
  { unProfileName :: String
  } deriving (Eq, Ord, Show)

xpProfileName :: PU ProfileName
xpProfileName = xpAttr "profile" $ xpWrap (ProfileName, unProfileName) xpText

newtype Vendor = Vendor
  { unVendor :: String
  } deriving (Eq, Ord, Show)

xpVendor :: PU Vendor
xpVendor = xpAttr "vendor" $ xpWrap (Vendor, unVendor) xpText

newtype Comment = Comment
  { unComment :: String
  } deriving (Eq, Ord, Show)

xpComment :: PU Comment
xpComment = xpWrap (Comment, unComment) xpText

xpCommentAttr :: PU Comment
xpCommentAttr = xpAttr "comment" xpComment

newtype Name = Name
  { unName :: String
  } deriving (Eq, Ord, Show)

xpName :: PU Name
xpName = xpAttr "name" $ xpWrap (Name, unName) xpText
