module Main
  ( main
  ) where

import qualified Control.Monad as CM
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as DM
import qualified Data.Set as S
import ManPages
import MangledRegistry
import qualified System.Directory as D
import qualified System.Environment as E
import qualified System.FilePath as F
import qualified System.IO as SI
import qualified Text.PrettyPrint.HughesPJClass as P

main :: IO ()
main = do
  [a, registryPath] <- E.getArgs
  let api = API a
  res <- parseRegistry toEnumType `fmap` readFile registryPath
  case res of
    Left msg -> SI.hPutStrLn SI.stderr msg
    Right registry -> do
      printTokens api registry
      printGroups api registry
      let sigMap = signatureMap registry
      printForeign sigMap
      printFunctions api registry sigMap
      let extModules = extensionModules api registry
      CM.forM_ extModules printExtensionModule
      printReExports extModules
      printExtensionSupport extModules
      CM.forM_ (openGLVersions api) $ \v ->
        CM.forM_ (supportedProfiles api v) $ \p -> printFeature api v p registry
      printTopLevel api extModules

openGLVersions :: API -> [Version]
openGLVersions api =
  map read $
  case unAPI api of
    "gl" ->
      [ "1.0"
      , "1.1"
      , "1.2"
      , "1.3"
      , "1.4"
      , "1.5"
      , "2.0"
      , "2.1"
      , "3.0"
      , "3.1"
      , "3.2"
      , "3.3"
      , "4.0"
      , "4.1"
      , "4.2"
      , "4.3"
      , "4.4"
      , "4.5"
      , "4.6"
      ]
    "gles1" -> ["1.0"]
    "gles2" -> ["2.0", "3.0", "3.1", "3.2"]
    a -> error $ "unknown API " ++ a

latestVersion :: API -> Version
latestVersion = last . openGLVersions

supportedProfiles :: API -> Version -> [Maybe ProfileName]
supportedProfiles api v =
  case unAPI api of
    "gl"
      | major v < 3 -> [Nothing]
      | otherwise -> map (Just . ProfileName) ["core", "compatibility"]
    "gles1" -> map (Just . ProfileName) ["lite", "common"]
    "gles2" -> [Nothing]
    a -> error $ "unknown API " ++ a

latestProfiles :: API -> [Maybe ProfileName]
latestProfiles api = supportedProfiles api (latestVersion api)

profileToReExport :: API -> Maybe ProfileName
profileToReExport = last . latestProfiles

printFeature :: API -> Version -> Maybe ProfileName -> Registry -> IO ()
printFeature api version mbProfile registry =
  printExtension [featureName version mbProfile] Nothing $
  fixedReplay api version mbProfile registry

featureName :: Version -> Maybe ProfileName -> String
featureName version mbProfile =
  maybe "Version" (capitalize . unProfileName) mbProfile ++
  show (major version) ++ show (minor version)

printTokens :: API -> Registry -> IO ()
printTokens api registry = do
  let cmnt =
        [ Comment "All enumeration tokens from the"
        , Comment "<http://www.opengl.org/registry/ OpenGL registry>."
        ]
  startModule
    ["Tokens"]
    (Just
       "{-# LANGUAGE CPP, PatternSynonyms, ScopedTypeVariables #-}\n#if __GLASGOW_HASKELL__ >= 800\n{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}\n#endif")
    cmnt $ \moduleName h -> do
    hRender h $ Module moduleName P.empty
    hRender h $ Import (moduleNameFor ["Types"]) P.empty
    SI.hPutStrLn h ""
    mapM_
      (SI.hPutStrLn h . unlines . convertEnum)
      [e | es <- M.elems (enums registry), e <- es, api `matches` enumAPI e]

printGroups :: API -> Registry -> IO ()
printGroups api registry = do
  let cmnt =
        [ Comment "All enumeration groups from the"
        , Comment "<http://www.opengl.org/registry/ OpenGL registry>."
        ]
  startModule ["Groups"] Nothing cmnt $ \moduleName h -> do
    hRender h $ Module moduleName (P.text "(\n  -- $EnumerantGroups\n)")
    hRender h $ Comment "$EnumerantGroups"
    hRender h $
      Comment
        "Note that the actual set of valid values depend on the OpenGL version, the"
    hRender h $
      Comment
        "chosen profile and the supported extensions. Therefore, the groups mentioned"
    hRender h $
      Comment
        "here should only be considered a rough guideline, for details see the OpenGL"
    hRender h $ Comment "specification."
    CM.forM_ (M.assocs (groups registry)) $ \(gn, g) -> do
      let ugn = unGroupName gn
          es = getGroupEnums api registry g
      hRender h $ Comment ""
      hRender h $ Comment ("=== #" ++ ugn ++ "# " ++ ugn)
      hRender h $ Comment (groupHeader gn es)
      hRender h $ Comment ""
      -- TODO: Improve the alias computation below. It takes quadratic time and
      -- is very naive about what is the canonical name and what is an alias.
      CM.forM_ es $ \e -> do
        let same = L.sort [f | f <- es, enumValue e == enumValue f]
        CM.when (e == head same) $
          hRender h $
          Comment
            ("* " ++
             linkToToken e ++
             (case tail same of
                [] -> ""
                aliases ->
                  " (" ++
                  al ++
                  ": " ++ L.intercalate ", " (map linkToToken aliases) ++ ")"
                  where al
                          | length aliases == 1 = "alias"
                          | otherwise = "aliases"))

linkToToken :: Enum' -> String
linkToToken e =
  "'" ++
  (case moduleNameFor ["Tokens"] of
     ModuleName mn -> mn) ++
  "." ++ (unEnumName . enumName) e ++ "'"

-- There are several enums which are mentioned in groups, but commented out in
-- enums (12 GL_*_ICC_SGIX enumerants). These are implicitly filtered out below.
getGroupEnums :: API -> Registry -> Group -> [Enum']
getGroupEnums api registry g =
  [ e
  | name <- groupEnums g
  , Just es <- [M.lookup name (enums registry)]
  , e <- es
  , api `matches` enumAPI e
  ]

groupHeader :: GroupName -> [Enum'] -> String
groupHeader gn es =
  case sortUnique (map enumTypeWithFix es)
  -- There are 2 empty groups: DataType and FfdMaskSGIX.
        of
    [] -> "There are no values defined for this enumeration group."
    [t]
      | isMask t -> "A bitwise combination of several of the following values:"
      | otherwise -> "One of the following values:"
    tys ->
      error $
      "Contradicting enumerant types " ++
      L.intercalate " and " (map unTypeName tys) ++
      " in group " ++
      unGroupName gn ++
      ":\n" ++
      unlines
        [ "  " ++ unEnumName (enumName e) ++ " :: " ++ unTypeName (enumType e)
        | e <- es
        ]
  -- NV_path_rendering screws up typing: It uses GL_NONE as a bitfield, and this
  -- leads to a collision in the PathFontStyle group. :-/
  -- Furthermore, glClampColor uses GL_TRUE and GL_FALSE as a GLenum.
  where
    enumTypeWithFix e
      | gn == GroupName "PathFontStyle" && enumName e == EnumName "GL_NONE" =
        TypeName "GLbitfield"
      | gn == GroupName "ClampColorModeARB" && enumName e == EnumName "GL_TRUE" =
        TypeName "GLenum"
      | gn == GroupName "ClampColorModeARB" && enumName e == EnumName "GL_FALSE" =
        TypeName "GLenum"
      | otherwise = enumType e

-- Calulate a map from compact signature to short names.
signatureMap :: Registry -> M.Map String String
signatureMap registry =
  fst $ M.foldl' step (M.empty, 0 :: Integer) (commands registry)
  where
    step (m, n) command = memberAndInsert (n + 1) n (sig command) (dyn n) m
    sig = flip (showSignatureFromCommand registry) False
    dyn n = "dyn" ++ show n
    memberAndInsert notFound found key value theMap =
      (newMap, maybe notFound (const found) maybeValue)
      where
        (maybeValue, newMap) =
          M.insertLookupWithKey (\_ _ s -> s) key value theMap

printForeign :: M.Map String String -> IO ()
printForeign sigMap = do
  let cmnt = [Comment "All foreign imports."]
  startModule
    ["Foreign"]
    (Just "{-# LANGUAGE CPP #-}\n{-# OPTIONS_HADDOCK hide #-}")
    cmnt $ \moduleName h -> do
    hRender h $ Module moduleName P.empty
    hRender h $ Import (ModuleName "Foreign.C.Types") P.empty
    hRender h $
      Import (ModuleName "Foreign.Marshal.Error") (P.text "( throwIf )")
    hRender h $ Import (ModuleName "Foreign.Ptr") P.empty
    hRender h $
      Import (moduleNameFor ["GetProcAddress"]) (P.text "( getProcAddress )")
    hRender h $ Import (moduleNameFor ["Types"]) P.empty
    hRender h $ Import (ModuleName "Numeric.Fixed") P.empty
    hRender h $ Import (ModuleName "Numeric.Half") P.empty
    SI.hPutStrLn h ""
    SI.hPutStrLn h "getCommand :: String -> IO (FunPtr a)"
    SI.hPutStrLn h "getCommand cmd ="
    SI.hPutStrLn
      h
      "  throwIfNullFunPtr (\"unknown OpenGL command \" ++ cmd) $ getProcAddress cmd"
    SI.hPutStrLn
      h
      "  where throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)"
    SI.hPutStrLn h "        throwIfNullFunPtr = throwIf (== nullFunPtr) . const"
    SI.hPutStrLn h ""
    mapM_ (SI.hPutStrLn h . uncurry makeImportDynamic) (M.assocs sigMap)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . L.unfoldr (Just . splitAt n)

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n c xs =
  reverse . take (max n (length xs)) . (++ repeat c) . reverse $ xs

printFunctions :: API -> Registry -> M.Map String String -> IO ()
printFunctions api registry sigMap = do
  let cmnt =
        [ Comment "All raw functions from the"
        , Comment "<http://www.opengl.org/registry/ OpenGL registry>."
        ]
      cmds = chunksOf 100 . M.toAscList . commands $ registry
      mnames =
        [ ["Functions", "F" ++ justifyRight 2 '0' (show i)]
        | i <- [1 .. length cmds]
        ]
  startModule ["Functions"] Nothing cmnt $ \moduleName h -> do
    hRender h $
      Module
        moduleName
        (P.text
           ("(\n" ++
            separate
              (\x ->
                 "module " ++
                 (case moduleNameFor x of
                    ModuleName mn -> mn))
              mnames ++
            "\n)"))
    CM.forM_ mnames $ \mname -> hRender h $ Import (moduleNameFor mname) P.empty
  CM.zipWithM_ (printSubFunctions api registry sigMap) mnames cmds

printSubFunctions ::
     API
  -> Registry
  -> M.Map String String
  -> [String]
  -> [(CommandName, Command)]
  -> IO ()
printSubFunctions api registry sigMap mname cmds = do
  let cmnt =
        [ Comment "Raw functions from the"
        , Comment "<http://www.opengl.org/registry/ OpenGL registry>."
        ]
  startModule mname (Just "{-# OPTIONS_HADDOCK hide #-}") cmnt $ \moduleName h -> do
    hRender h $
      Module
        moduleName
        (P.text ("(\n" ++ separate unCommandName (map fst cmds) ++ "\n)"))
    hRender h $
      Import (ModuleName "Control.Monad.IO.Class") (P.text "( MonadIO(..) )")
    hRender h $ Import (ModuleName "Foreign.Ptr") P.empty
    hRender h $ Import (moduleNameFor ["Foreign"]) P.empty
    hRender h $ Import (moduleNameFor ["Types"]) P.empty
    hRender h $
      Import (ModuleName "System.IO.Unsafe") (P.text "( unsafePerformIO )")
    SI.hPutStrLn h ""
    mapM_ (SI.hPutStrLn h . showCommand api registry sigMap . snd) cmds

type ExtensionParts = ([TypeName], [Enum'], [Command])

type ExtensionModule = (ExtensionName, ExtensionName, ExtensionParts)

printExtensionModule :: ExtensionModule -> IO ()
printExtensionModule (extName, mangledExtName, extensionParts) =
  printExtension
    [extensionNameCategory mangledExtName, extensionNameName mangledExtName]
    (Just extName)
    extensionParts

extendWithProfile :: ExtensionName -> Maybe ProfileName -> ExtensionName
extendWithProfile extName =
  maybe
    extName
    (\p ->
       extName
       { extensionNameName =
           joinWords [extensionNameName extName, capitalize (unProfileName p)]
       })

mangleExtensionName :: ExtensionName -> ExtensionName
mangleExtensionName extName =
  extName
  { extensionNameCategory = fixCategory $ extensionNameCategory extName
  , extensionNameName =
      zip (splitWords (extensionNameName extName)) [0 :: Integer ..] >>=
      fixExtensionWord
  }
  where
    fixCategory c =
      case c of
        "3DFX" -> "ThreeDFX"
        _ -> c
    fixExtensionWord (w, pos) =
      case w of
        "422"
          | pos == 0 -> "FourTwoTwo"
        "64bit" -> "64Bit"
        "ES2" -> "ES2"
        "ES3" -> "ES3"
        "FXT1" -> "FXT1"
        "a2ui" -> "A2UI"
        "abgr" -> "ABGR"
        "astc" -> "ASTC"
        "bgra" -> "BGRA"
        "bptc" -> "BPTC"
        "cl" -> "CL"
        "cmyka" -> "CMYKA"
        "dxt1" -> "DXT1"
        "es" -> "ES"
        "ffd" -> "FFD"
        "fp64" -> "FP64"
        "gpu" -> "GPU"
        "hdr" -> "HDR"
        "latc" -> "LATC"
        "ldr" -> "LDR"
        "lod" -> "LOD"
        "pn" -> "PN"
        "rg" -> "RG"
        "rgb" -> "RGB"
        "rgb10" -> "RGB10"
        "rgb32" -> "RGB32"
        "rgtc" -> "RGTC"
        "s3tc" -> "S3TC"
        "sRGB" -> "SRGB"
        "snorm" -> "SNorm"
        "texture3D" -> "Texture3D"
        "texture4D" -> "Texture4D"
        "vdpau" -> "VDPAU"
        "ycbcr" -> "YCbCr"
        "ycrcb" -> "YCrCb"
        "ycrcba" -> "YCrCbA"
        _ -> capitalize w

extensionModules :: API -> Registry -> [ExtensionModule]
extensionModules api registry =
  [ (extName, mangledExtName, executeModifications api mbProfile registry mods)
  | (extName, mods) <- supportedExtensions api registry
  , mbProfile <-
      if isProfileDependent mods
        then suppProfs
        else [Nothing]
  , let mangledExtName =
          mangleExtensionName (extendWithProfile extName mbProfile)
  ]
  where
    suppProfs = latestProfiles api
    isProfileDependent mods =
      any (`S.member` allProfileNames) (mentionedProfileNames mods)
    mentionedProfileNames = DM.mapMaybe modificationProfile
    allProfileNames = S.fromList . DM.catMaybes $ suppProfs

-- We only consider non-empty supported extensions/modifications for the given API.
supportedExtensions :: API -> Registry -> [(ExtensionName, [Modification])]
supportedExtensions api registry =
  [ nameAndMods
  | ext <- extensions registry
  , api `supports` extensionSupported ext
  , nameAndMods@(_, _:_) <- [nameAndModifications ext]
  ]
  where
    nameAndModifications :: Extension -> (ExtensionName, [Modification])
    nameAndModifications e =
      ( extensionName e
      , [ conditionalModificationModification cm
        | cm <- extensionsRequireRemove e
        , api `matches` conditionalModificationAPI cm
           -- ARB_compatibility has an empty "require" element only
        , not .
            null .
            modificationInterfaceElements . conditionalModificationModification $
            cm
        ])

extensionHyperlink :: ExtensionName -> String
extensionHyperlink n =
  "<https://www.opengl.org/registry/specs/" ++
  fixRegistryPath (extensionNameCategory n ++ "/" ++ extensionNameName n) ++
  ".txt " ++ joinWords [extensionNameCategory n, extensionNameName n] ++ ">"
  where
    fixRegistryPath :: String -> String
    fixRegistryPath path =
      case path of
        "3DFX/multisample" -> "3DFX/3dfx_multisample"
        "EXT/debug_label" -> "EXT/EXT_debug_label"
        "EXT/debug_marker" -> "EXT/EXT_debug_marker"
        "EXT/multisample" -> "EXT/wgl_multisample"
        "EXT/texture_cube_map" -> "ARB/texture_cube_map"
        "INGR/blend_func_separate" -> "EXT/blend_func_separate"
        "KHR/blend_equation_advanced_coherent" -> "KHR/blend_equation_advanced"
        "KHR/texture_compression_astc_ldr" -> "KHR/texture_compression_astc_hdr"
        "NV/blend_equation_advanced_coherent" -> "NV/blend_equation_advanced"
        "NVX/conditional_render" -> "NVX/nvx_conditional_render"
        "OES/byte_coordinates" -> "OES/OES_byte_coordinates"
        "OES/compressed_paletted_texture" ->
          "OES/OES_compressed_paletted_texture"
        "OES/fixed_point" -> "OES/OES_fixed_point"
        "OES/query_matrix" -> "OES/OES_query_matrix"
        "OES/read_format" -> "OES/OES_read_format"
        "OES/single_precision" -> "OES/OES_single_precision"
        "SGIS/fog_function" -> "SGIS/fog_func"
        "SGIS/point_parameters" -> "EXT/point_parameters"
        "SGIX/fragment_lighting" -> "EXT/fragment_lighting"
        "SGIX/pixel_texture" -> "SGIX/sgix_pixel_texture"
        "SGIX/texture_add_env" -> "SGIX/texture_env_add"
        _ -> path

printReExports :: [ExtensionModule] -> IO ()
printReExports extModules = do
  let extMap =
        M.fromListWith
          (++)
          [ ( ( extensionNameCategory extName
              , extensionNameCategory mangledExtName)
            , [mangledExtName])
          | (extName, mangledExtName, _) <- extModules
          ]
      reExports =
        [ (cat, L.sort mangledExtNames)
        | (cat, mangledExtNames) <- M.toList extMap
        ]
  CM.forM_ reExports $ \((category, mangledCategory), mangledExtNames) -> do
    let cmnt =
          [ Comment
              ("A convenience module, combining all raw modules containing " ++
               category ++ " extensions.")
          ]
    startModule [mangledCategory] Nothing cmnt $ \moduleName h -> do
      hRender h $
        Module
          moduleName
          (P.text
             ("(\n" ++
              separate
                (\mangledExtName ->
                   "module " ++
                   (case extensionNameFor mangledExtName of
                      ModuleName mn -> mn))
                mangledExtNames ++
              "\n)"))
      CM.forM_ mangledExtNames $ \mangledExtName ->
        hRender h $ Import (extensionNameFor mangledExtName) P.empty

printExtensionSupport :: [ExtensionModule] -> IO ()
printExtensionSupport extModules = do
  let cmnt = [Comment "Extension support predicates."]
  startModule ["ExtensionPredicates"] (Just "{-# OPTIONS_HADDOCK hide #-}") cmnt $ \moduleName h -> do
    hRender h $ Module moduleName P.empty
    hRender h $
      Import (ModuleName "Control.Monad.IO.Class") (P.text "( MonadIO(..) )")
    hRender h $ Import (ModuleName "Data.Set") (P.text "( member )")
    hRender h $
      Import
        (moduleNameFor ["GetProcAddress"])
        (P.text "( getExtensions, extensions )")
    let names = sortUnique [extName | (extName, _, _) <- extModules]
    CM.forM_ names $ \extName -> do
      let predNameMonad = extensionPredicateNameMonad extName
          predName = extensionPredicateName extName
          extString =
            joinWords
              [ extensionNameAPI extName
              , extensionNameCategory extName
              , extensionNameName extName
              ]
      SI.hPutStrLn h ""
      hRender h $
        Comment
          ("| Is the " ++ extensionHyperlink extName ++ " extension supported?")
      SI.hPutStrLn h $ predNameMonad ++ " :: MonadIO m => m Bool"
      SI.hPutStrLn h $
        predNameMonad ++
        " = getExtensions >>= (return . member " ++ show extString ++ ")"
      SI.hPutStrLn h ""
      hRender h $
        Comment
          ("| Is the " ++ extensionHyperlink extName ++ " extension supported?")
      hRender h $
        Comment
          "Note that in the presence of multiple contexts with different capabilities,"
      hRender h $
        Comment
          ("this might be wrong. Use '" ++
           predNameMonad ++ "' in those cases instead.")
      SI.hPutStrLn h $ predName ++ " :: Bool"
      SI.hPutStrLn h $
        predName ++ " = member " ++ show extString ++ " extensions"
      SI.hPutStrLn h $ "{-# NOINLINE " ++ predName ++ " #-}"

extensionNameFor :: ExtensionName -> ModuleName
extensionNameFor mangledExtName =
  moduleNameFor
    [extensionNameCategory mangledExtName, extensionNameName mangledExtName]

supports :: API -> Maybe [API] -> Bool
_ `supports` Nothing = True
a `supports` Just apis = a `elem` apis

capitalize :: String -> String
capitalize str = C.toUpper (head str) : map C.toLower (tail str)

separate :: (a -> String) -> [a] -> String
separate f = L.intercalate ",\n" . map (("  " ++) . f)

-- Note that we handle features just like extensions.
printExtension :: [String] -> Maybe ExtensionName -> ExtensionParts -> IO ()
printExtension moduleNameSuffix mbExtName (ts, es, cs) = do
  let pragma =
        if null es
          then Nothing
          else Just "{-# LANGUAGE PatternSynonyms #-}"
  startModule moduleNameSuffix pragma [] $ \moduleName h -> do
    let extStr =
          flip (maybe "") mbExtName $ \extName ->
            "  -- * Extension Support\n" ++
            separate
              id
              [ extensionPredicateNameMonad extName
              , extensionPredicateName extName
              ] ++
            ",\n"
        typeStr
          | null ts = ""
          | otherwise =
            "  -- * Types\n" ++
            separate unTypeName ts ++
            if null es && null cs
              then "\n"
              else ",\n"
        enumStr
          | null es = ""
          | otherwise =
            "  -- * Enums\n" ++
            separate (("pattern " ++) . unEnumName . enumName) es ++
            if null cs
              then "\n"
              else ",\n"
        funcStr
          | null cs = ""
          | otherwise =
            "  -- * Functions\n" ++
            separate (unCommandName . commandName) cs ++ "\n"
    hRender h $
      Module
        moduleName
        (P.text ("(\n" ++ extStr ++ typeStr ++ enumStr ++ funcStr ++ ")"))
    CM.when (DM.isJust mbExtName) $
      hRender h $ Import (moduleNameFor ["ExtensionPredicates"]) P.empty
    CM.unless (null ts) $ hRender h $ Import (moduleNameFor ["Types"]) P.empty
    CM.unless (null es) $ hRender h $ Import (moduleNameFor ["Tokens"]) P.empty
    CM.unless (null cs) $
      hRender h $ Import (moduleNameFor ["Functions"]) P.empty

extensionPredicateName :: ExtensionName -> String
extensionPredicateName extName =
  joinWords
    [ map C.toLower (extensionNameAPI extName)
    , extensionNameCategory extName
    , extensionNameName extName
    ]

extensionPredicateNameMonad :: ExtensionName -> String
extensionPredicateNameMonad extName =
  map C.toLower (extensionNameAPI mangledExtName) ++
  "Get" ++
  extensionNameCategory mangledExtName ++ extensionNameName mangledExtName
  where
    mangledExtName = mangleExtensionName extName

printTopLevel :: API -> [ExtensionModule] -> IO ()
printTopLevel api extModules = do
  let mangledCategories =
        sortUnique
          [ extensionNameCategory mangledExtName
          | (_, mangledExtName, _) <- extModules
          ]
      profToReExport = profileToReExport api
      lastComp = featureName (latestVersion api) profToReExport
      moduleNames =
        [ moduleNameFor [c]
        | c <- [lastComp, "GetProcAddress"] ++ mangledCategories
        ]
      cmnt =
        [ Comment
            (unwords
               [ "A convenience module, combining the latest"
               , apiName api
               , maybe
                   "version"
                   (\p -> unProfileName p ++ " profile")
                   profToReExport
               , "plus"
               ])
        , Comment "all extensions."
        ]
  startModule [] Nothing cmnt $ \moduleName h -> do
    hRender h $
      Module
        moduleName
        (P.text
           ("(\n" ++
            separate (\(ModuleName m) -> "module " ++ m) moduleNames ++ "\n)"))
    CM.forM_ moduleNames $ \theModuleName ->
      hRender h $ Import theModuleName P.empty

apiName :: API -> String
apiName api =
  case unAPI api of
    "gl" -> "OpenGL"
    "gles1" -> "OpenGL ES 1.x"
    "gles2" -> "OpenGL ES"
    a -> error $ "unknown API " ++ a

sortUnique :: Ord a => [a] -> [a]
sortUnique = S.toList . S.fromList

startModule ::
     [String]
  -> Maybe String
  -> [Comment]
  -> (ModuleName -> SI.Handle -> IO ())
  -> IO ()
startModule moduleNameSuffix mbPragma comments action = do
  let path = modulePathFor moduleNameSuffix
      moduleName = moduleNameFor moduleNameSuffix
  D.createDirectoryIfMissing True $ F.takeDirectory path
  SI.withFile path SI.WriteMode $ \h -> do
    printModuleHeader h mbPragma moduleName comments
    action moduleName h

moduleNameFor :: [String] -> ModuleName
moduleNameFor = ModuleName . L.intercalate "." . moduleNameParts

modulePathFor :: [String] -> FilePath
modulePathFor moduleNameSuffix =
  F.joinPath (moduleNameParts moduleNameSuffix) `F.addExtension` "hs"

moduleNameParts :: [String] -> [String]
moduleNameParts = (["Graphics", "GL"] ++)

printModuleHeader ::
     SI.Handle -> Maybe String -> ModuleName -> [Comment] -> IO ()
printModuleHeader h mbPragma (ModuleName moduleName) comments = do
  maybe (return ()) (SI.hPutStrLn h) mbPragma
  hRender h $
    Comment
      "------------------------------------------------------------------------------"
  hRender h $ Comment "|"
  hRender h $ Comment ("Module      :  " ++ moduleName)
  hRender h $ Comment "Copyright   :  (c) Sven Panne 2019"
  hRender h $ Comment "License     :  BSD3"
  hRender h $ Comment ""
  hRender h $ Comment "Maintainer  :  Sven Panne <svenpanne@gmail.com>"
  hRender h $ Comment "Stability   :  stable"
  hRender h $ Comment "Portability :  portable"
  hRender h $ Comment ""
  CM.unless (null comments) $ do
    mapM_ (hRender h) comments
    hRender h $ Comment ""
  hRender h $
    Comment
      "------------------------------------------------------------------------------"
  SI.hPutStrLn h ""

-- Annoyingly enough, the OpenGL registry doesn't contain any enums for
-- OpenGL 1.0, so let's just use the OpenGL 1.1 ones. Furthermore, features
-- don't explicitly list the types referenced by commands, so we add them.
fixedReplay :: API -> Version -> Maybe ProfileName -> Registry -> ExtensionParts
fixedReplay api version mbProfile registry
  | api == API "gl" && version == read "1.0" = (ts', es11, cs)
  | otherwise = (ts', es, cs)
  where
    (ts, es, cs) = replay api version mbProfile registry
    (_, es11, _) = replay api (read "1.1") mbProfile registry
    ts' =
      S.toList . addFuncsAndMakes . S.unions $
      S.fromList ts : map referencedTypes cs

-- For debug callbacks, we want to export the Haskell types and their creators, too.
addFuncsAndMakes :: S.Set TypeName -> S.Set TypeName
addFuncsAndMakes =
  flip
    (foldr addFuncAndMake)
    ["GLDEBUGPROC", "GLDEBUGPROCAMD", "GLDEBUGPROCARB", "GLDEBUGPROCKHR"]
  where
    addFuncAndMake t ts
      | TypeName t `S.member` ts =
        ts `S.union` S.fromList (map TypeName [t ++ "Func", "make" ++ t])
      | otherwise = ts

-- Here is the heart of the feature construction logic: Chronologically replay
-- the whole version history for the given API/version/profile triple.
replay :: API -> Version -> Maybe ProfileName -> Registry -> ExtensionParts
replay api version mbProfile registry =
  executeModifications api mbProfile registry modifications
  where
    modifications = history >>= flip lookup' (features registry)
    history =
      L.sort
        [key | key@(a, v) <- M.keys (features registry), a == api, v <= version]

executeModifications ::
     API -> Maybe ProfileName -> Registry -> [Modification] -> ExtensionParts
executeModifications api mbProfile registry modifications = (ts, es, cs)
  where
    ts = [n | TypeElement n <- lst]
    es =
      [ e
      | EnumElement n <- lst
      , e <- lookup' n (enums registry)
      , api `matches` enumAPI e
      ]
    cs = [lookup' n (commands registry) | CommandElement n <- lst]
    lst = S.toList $ interfaceElementsFor mbProfile modifications

interfaceElementsFor ::
     Maybe ProfileName -> [Modification] -> S.Set InterfaceElement
interfaceElementsFor mbProfile modifications =
  foldl (flip ($)) S.empty modificationsFor
  where
    modificationsFor =
      [ op (modificationKind m) ie
      | m <- modifications
      , maybe True (`matches` modificationProfile m) mbProfile
      , ie <- modificationInterfaceElements m
      ]
    op Require = S.insert
    op Remove = S.delete

lookup' :: (Ord k, Show k) => k -> M.Map k a -> a
lookup' k = M.findWithDefault (error ("unknown name " ++ show k)) k

matches :: Eq a => a -> Maybe a -> Bool
_ `matches` Nothing = True
s `matches` Just t = s == t

convertEnum :: Enum' -> [String]
convertEnum e =
  [ "pattern " ++
    n ++ " = " ++ unEnumValue (enumValue e) ++ " :: " ++ unTypeName (enumType e)
  ]
  where
    n = unEnumName . enumName $ e

showCommand :: API -> Registry -> M.Map String String -> Command -> String
showCommand api registry sigMap c =
  showString
    (P.render
       (P.pPrint (Comment (take 77 (name ++ " " ++ repeat '-'))) P.$+$ P.text "" P.$+$
        P.text "")) .
  showString (P.render cmnt) .
  showString (name ++ "\n") .
  showString "  :: MonadIO m\n" .
  showString ("  => " ++ signature True) .
  showString
    (name ++
     args ++ " = liftIO $ " ++ dyn_name ++ " " ++ ptr_name ++ args ++ "\n\n") .
  showString ("{-# NOINLINE " ++ ptr_name ++ " #-}\n") .
  showString (ptr_name ++ " :: FunPtr (" ++ compactSignature ++ ")\n") .
  showString
    (ptr_name ++ " = unsafePerformIO $ getCommand " ++ str_name ++ "\n") $
  ""
  where
    name = signatureElementName (resultType c)
    dyn_name = lookup' compactSignature sigMap
    ptr_name = "ptr_" ++ name
    str_name = show name
    compactSignature = signature False
    signature = showSignatureFromCommand registry c
    urls = M.findWithDefault [] (api, CommandName name) manPageURLs
    links = L.intercalate " or " (map renderURL urls)
    cmnt =
      case concat (man ++ ve ++ al) of
        "" -> P.empty
        cs -> P.pPrint (Comment ("|" ++ cs)) P.$+$ P.text ""
    man =
      case urls of
        [] -> []
        [_] -> [" Manual page for " ++ links ++ "."]
        _ -> [" Manual pages for " ++ links ++ "."]
    ve =
      [ " The vector equivalent of this command is '" ++ unCommandName v ++ "'."
      | Just v <- [vecEquiv c]
      ]
    al =
      [ " This command is an alias for '" ++ unCommandName a ++ "'."
      | Just a <- [alias c]
      ]
    renderURL (u, l) = "<" ++ u ++ " " ++ l ++ ">"
    args = [1 .. length (paramTypes c)] >>= \i -> " v" ++ show i

makeImportDynamic :: String -> String -> String
makeImportDynamic compactSignature dyn_name =
  "foreign import CALLCONV \"dynamic\" " ++
  dyn_name ++
  "\n" ++
  "  :: FunPtr (" ++
  compactSignature ++ ")\n" ++ "  ->         " ++ compactSignature ++ "\n"

showSignatureFromCommand :: Registry -> Command -> Bool -> String
showSignatureFromCommand registry c withComment =
  L.intercalate
    ((if withComment
        then " "
        else "") ++
     " -> ")
    ([showSignatureElement registry withComment False t | t <- paramTypes c] ++
     [showSignatureElement registry withComment True (resultType c)])

showSignatureElement :: Registry -> Bool -> Bool -> SignatureElement -> String
showSignatureElement registry withComment isResult sigElem = el ++ cmnt
  where
    el
      | isResult = monad ++ " " ++ showsPrec 11 sigElem ""
      | otherwise = show sigElem
    monad
      | withComment = "m"
      | otherwise = "IO"
    cmnt
      | withComment =
        P.render (showComment registry name sigElem P.$+$ P.text "")
      | otherwise = ""
    name
      | isResult = ""
      | otherwise = signatureElementName sigElem

showComment :: Registry -> String -> SignatureElement -> P.Doc
showComment registry name sigElem
  | null name' && null info = P.text ""
  | otherwise = P.text " " P.<> P.pPrint (Comment ("^" ++ name' ++ info ++ "."))
  where
    name'
      | null name = ""
      | otherwise = " " ++ inlineCode name
    info
      | isInteresting = elms ++ " of type " ++ hurz
      | otherwise = ""
        -- Alas, there are tons of group names which are referenced, but never
        -- defined, so we have to leave them without a link.
        -- TODO: Do not use Show instance for SignatureElement.
    hurz =
      case belongsToGroup sigElem of
        Just gn
          | numPointer sigElem <= 1 && fgn `M.member` groups registry ->
            linkToGroup fgn
          where fgn = fixGroupName gn
        _ -> inlineCode (show (base sigElem))
    isInteresting =
      DM.isJust (arrayLength sigElem) || DM.isJust (belongsToGroup sigElem)
    elms
      | numPointer sigElem > 0 = " pointing to" ++ len ++ " " ++ elements
      | otherwise = ""
    elements
      | arrayLength sigElem == Just "1" = "element"
      | otherwise = "elements"
    len = maybe "" (\l -> " " ++ inlineCode l) (arrayLength sigElem)
    base = maybeDeref . maybeSetBaseType
    maybeDeref e
      | numPointer e > 0 = e {numPointer = numPointer e - 1}
      | otherwise = e
    maybeSetBaseType e =
      maybe e (\g -> e {baseType = TypeName (unGroupName g)}) (belongsToGroup e)

fixGroupName :: GroupName -> GroupName
fixGroupName g
  | g == GroupName "PixelInternalFormat" = GroupName "InternalFormat"
  | g == GroupName "SGIXFfdMask" = GroupName "FfdMaskSGIX"
  | otherwise = g

-- TODO: This is very fragile, but currently there is no clean way to specify
-- link texts when referencing anchors in Haddock.
linkToGroup :: GroupName -> String
linkToGroup g =
  "[" ++ n ++ "](" ++ htmlFilenameFor ["Groups"] ++ "#" ++ n ++ ")"
  where
    n = unGroupName g

htmlFilenameFor :: [String] -> String
htmlFilenameFor = (++ ".html") . L.intercalate "-" . moduleNameParts

inlineCode :: String -> String
inlineCode s = "@" ++ s ++ "@"

-- TODO: Use Either instead of error below?
toEnumType :: ToEnumType
toEnumType eNamespace eGroup eType suffix eName =
  TypeName $
  case (eNamespace, eGroup, eType, unTypeSuffix `fmap` suffix, eName)
    -- glx.xml
        of
    (Just "GLXStrings", _, _, _, _) -> "String"
    (Just ('G':'L':'X':_), _, _, _, _) -> "CInt"
    -- egl.xml
    -- TODO: EGLenum for EGL_OPENGL_API, EGL_OPENGL_ES_API, EGL_OPENVG_API, EGL_OPENVG_IMAGE?
    (Just ('E':'G':'L':_), _, Nothing, Just "ull", _) -> "EGLTime"
    (Just ('E':'G':'L':_), _, _, _, _) -> "EGLint"
    -- wgl.xml
    (Just "WGLLayerPlaneMask", _, _, _, _) -> "UINT"
    (Just "WGLColorBufferMask", _, _, _, _) -> "UINT"
    (Just "WGLContextFlagsMask", _, _, _, _) -> "INT"
    (Just "WGLContextProfileMask", _, _, _, _) -> "INT"
    (Just "WGLImageBufferMaskI3D", _, _, _, _) -> "UINT"
    (Just "WGLDXInteropMaskNV", _, _, _, _) -> "GLenum"
    (Just ('W':'G':'L':_), _, _, _, _) -> "CInt"
    -- gl.xml
    (Just "OcclusionQueryEventMaskAMD", _, _, _, _) -> "GLuint"
    (Just "GL", Just "SpecialNumbers", _, _, "GL_FALSE") -> "GLboolean"
    (Just "GL", Just "SpecialNumbers", _, _, "GL_TRUE") -> "GLboolean"
    (Just "GL", Just "PathRenderingTokenNV", _, _, _) -> "GLubyte"
    (Just "GL", _, Just "bitmask", _, _) -> "GLbitfield"
    (Just "GL", _, Nothing, Just "u", _) -> "GLuint"
    (Just "GL", _, Nothing, Just "ull", _) -> "GLuint64"
    (Just "GL", _, Nothing, Nothing, _) -> "GLenum"
    (_, _, _, _, _) -> error "can't determine enum type"

isMask :: TypeName -> Bool
isMask = (== TypeName "GLbitfield")

--------------------------------------------------------------------------------
data Module =
  Module ModuleName
         Exports

instance P.Pretty Module where
  pPrint (Module mn ex) =
    P.text "module" P.<+> P.pPrint mn P.<+> ex P.<+> P.text "where\n"

type Exports = P.Doc

data Import =
  Import ModuleName
         ImportSpecs

instance P.Pretty Import where
  pPrint (Import mn im) = P.text "import" P.<+> P.pPrint mn P.<+> im

type ImportSpecs = P.Doc

newtype ModuleName =
  ModuleName String

instance P.Pretty ModuleName where
  pPrint (ModuleName m) = P.text m

newtype Comment =
  Comment String

instance P.Pretty Comment where
  pPrint (Comment c)
    | null c = P.text "--"
    | all (== '-') c = P.pPrint (Comment "") P.<> P.text c
    | otherwise = P.pPrint (Comment "") P.<+> P.text c

hRender :: P.Pretty a => SI.Handle -> a -> IO ()
hRender h = SI.hPutStrLn h . P.render . P.pPrint
