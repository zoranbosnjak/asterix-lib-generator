-- | Generate asterix 'python' source code.

{-# LANGUAGE TemplateHaskell #-}

module Asterix.Language.Python.Generator (mkCode) where

import           Control.Monad.State
import           Data.List (nub, sort, sortOn, sortBy, intersperse, inits)
import           Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Bool
import           Data.String (IsString)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar
import           Numeric (showHex)
import           Formatting as F
import           Options.Applicative as Opt
import           Data.FileEmbed (makeRelativeToProject, embedFile)

import           Asterix.Specs
                    (Name, RegisterSize, Title, ExtendedType(..), Edition(..), UapSelector(..))
import           Asterix.Indent

import           Asterix.Struct

-- | The same as 'line $ bformat (formating) arg1 arg2 ...'
fmt :: Format (BlockM Builder ()) a -> a
fmt m = runFormat m line

-- | Escape text (single quotes).
escaped :: Text -> Text
escaped s
    | s == "''" = s
    | otherwise = "'" <> T.replace "'" "\\'" s <> "'"

-- | Helper function to find variation index from the variation database.
indexOf :: VariationDb -> Variation -> VariationIx
indexOf db subvar = fst $ db Map.! subvar

-- | Name of variation with given index.
nameOf :: VariationIx -> Text
nameOf vc = sformat ("Variation_" % int) vc

-- | Name of argument with given (variation) index.
argOf :: VariationIx -> Text
argOf vc = nameOf vc <> "_Arg"

-- | Show path as text.
tPath :: Path -> Text
tPath = T.pack . show . reverse

-- | Python building blocks

typeAlias :: Text -> Text -> BlockM Builder ()
typeAlias a b = fmt (stext % " : TypeAlias = " % stext) a b

tList :: (Monoid t, IsString t) => [t] -> t
tList = mconcat . intersperse ", "

pyClass :: Text -> [Text] -> BlockM Builder () -> BlockM Builder ()
pyClass name bases body = do
    fmt ("class " % stext % stext % ":") name blist
    indent body
 where
    blist = case bases of
        [] -> ""
        _ -> "(" <> tList bases <> ")"

pyFunc :: Text -> [Text] -> Text -> BlockM Builder () -> BlockM Builder ()
pyFunc name args ret body = do
    fmt ("def " % stext % "(" % stext % ") -> " % stext % ":") name (tList args) ret
    indent body

pyIf :: Text -> BlockM Builder () -> BlockM Builder ()
pyIf cond body = do
    fmt ("if " % stext % ":") cond
    indent body

pyList :: Text -> [BlockM Builder ()] -> BlockM Builder ()
pyList name lst = enclose (line $ BL.fromText $ name <> " = [") "]" (mconcat lst)

pyDict :: Text -> [BlockM Builder ()] -> BlockM Builder ()
pyDict name lst = enclose (line $ BL.fromText $ name <> " = {") "}" (mconcat lst)

subSpecs :: [(Name, Text)] -> BlockM Builder ()
subSpecs lst = blocksLn
    [ bool (pure ()) (blocksLn $ fmap overload lst) (length lst > 1)
    , specsFunc
    ]
  where
    overload (name, cls) = do
        line "@overload"
        line "@classmethod"
        pyFunc "spec"
            ["cls", "key : Literal[" <> escaped name <> "]"]
            ("Type[" <> cls <> "]")
            "..."

    a = mconcat $ intersperse ", " $ do
        (name, _x) <- lst
        pure $ "Literal[" <> escaped name <> "]"

    b = mconcat $ intersperse ", " $ do
        (_name, cls) <- lst
        pure $ "Type['" <> cls <> "']"

    specsFunc = do
        line "@classmethod"
        pyFunc "spec"
            ["cls", "key : Union[" <> a <> "]"]
            ( "Union[" <> b <> "]") $ do
                forM_ lst $ \(name, cls) -> do
                    pyIf ("key == " <> escaped name)
                        (line $ "return " <> BL.fromText cls)
                line $ "assert_never(key)"

handleElement :: VariationIx -> OctetOffset -> RegisterSize -> Maybe Content -> BlockM Builder ()
handleElement vc o n mCont = do
    typeAlias (argOf vc) arg
    pyClass (nameOf vc) ["Element"] $ blocksLn
        [ fmt "variation = 'Element'"
        , constants
        , initFunc
        , tableLookup
        , toString
        , toQuantity
        ]
  where
    arg = case mCont of
        Just (ContentString _st) -> "Union[Raw,str]"
        Just (ContentQuantity _sig _num _frac unit) ->
            "Union[Raw,float,Tuple[float,Literal[" <> escaped unit <> "]]]"
        _ -> "Raw"

    tableConst = case mCont of
        Just (ContentTable lst) -> enclose "table = {" "}" $ mconcat $ do
            (x, t) <- lst
            pure $ fmt (int % ": " % stext % ",") x (escaped t)
        _ -> pure ()

    strConst = case mCont of
        Just (ContentString st) -> fmt ("string_type = " % string % "()") (show st)
        _ -> pure ()

    quantityConst = case mCont of
        Just (ContentQuantity sig num frac unit) -> fmt ("quantity = Quantity("
            % "'" % string % "'"
            % ", " % string
            % ", " % string
            % ", " % stext
            % ")") (show sig) (show num) (show frac) (escaped unit)
        _ -> pure ()

    constants = do
        fmt ("bit_offset8 = " % int) (unOctetOffset o)
        fmt ("bit_size = " % int) n
        tableConst
        strConst
        quantityConst

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        pyIf "isinstance(arg, Bits)"
            "super().__init__(arg); return"
        pyIf "isinstance(arg, Raw)"
            "super().__init__(self._from_raw(arg)); return"
        case mCont of
            Just (ContentString _st) -> pyIf "isinstance(arg, str)"
                "super().__init__(self._from_string(arg)); return"
            Just (ContentQuantity _sig _num _frac _unit) -> do
                pyIf "isinstance(arg, float)" "super().__init__(self._from_float(arg)); return"
                pyIf "isinstance(arg, tuple)" "super().__init__(self._from_float(arg[0])); return"
            _ -> pure ()
        line $ "assert_never(arg)"

    tableLookup = case mCont of
        Just (ContentTable _lst) -> do
            line $ "@property"
            pyFunc "table_value" ["self"] "Optional[str]" $
                "return self.__class__.table.get(self.to_uinteger())"
        _ -> pure ()

    toString = case mCont of
        Just (ContentString _st) -> pyFunc "to_string" ["self"] "str" $
            "return self._to_string()"
        _ -> pure ()

    toQuantity = case mCont of
        Just (ContentQuantity _sig _num _frac _unit) -> pyFunc "to_quantity" ["self"] "float" $
            "return self._to_quantity()"
        _ -> pure ()

-- | Filter items, keep only non-spare items.
nonSpare :: [(a, b, Item)] -> [(a, b, Name, Title, Variation)]
nonSpare lst = do
    (goff, n, item) <- lst
    case item of
        Spare _o _n -> empty
        Item name title var -> pure (goff, n, name, title, var)

handleGroup :: VariationDb -> VariationIx -> [(GroupOffset, RegisterSize, Item)] -> BlockM Builder ()
handleGroup db vc lst = do
    argGroup
    typeAlias (argOf vc) ("Union[Raw, " <> argOf vc <> "_Group]")
    pyClass (nameOf vc) ["Group"] $ blocksLn
        [ fmt "variation = 'Group'"
        , bitSize
        , subitemsList
        , subitemsDict
        , spec
        , initFunc
        , getItem
        , setItem
        ]
  where
    nLst = do
        (a, b, c, d, var) <- nonSpare lst
        pure (a, b, c, d, indexOf db var)

    argGroup = do
        let a = argOf vc <> "_Group = TypedDict('" <> argOf vc <> "_Group', {"
            b = "})"
        enclose (fmt stext a) b $ mconcat $ do
            (_goff, _n, name, _title, ix) <- nLst
            pure $ fmt (stext % ": Union[" % stext % ", " % stext % "],")
                (escaped name)
                (nameOf ix)
                (argOf ix)

    bitSize = fmt ("bit_size = " % int) $ sum [n | (_,n,_) <- lst]

    subitemsList = pyList "subitems_list" $ do
        (_goff, _n, item) <- lst
        pure $ case item of
            Spare o n -> fmt ("Spare(" % int % ", " % int % "),") (unOctetOffset o) n
            Item name _title var -> fmt ("(" % stext % ", " % stext % "),")
                (escaped name) (nameOf $ indexOf db var)

    subitemsDict = do
        line "# name: (title, cls, group_offset, bit_size)"
        pyDict "subitems_dict" $ do
            (goff, n, name, title, ix) <- nLst
            pure $ fmt (stext % ": (" % stext % ", " % stext % ", " % int % ", " % int % "),")
                (escaped name)
                (escaped title)
                (nameOf ix)
                goff
                n

    spec = subSpecs $ do
        (_goff, _n, name, _title, ix) <- nLst
        pure (name, nameOf ix)

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        pyIf "isinstance(arg, dict)" "super().__init__(*self._from_items(arg)); return"
        pyIf "isinstance(arg, Raw)" "super().__init__(*self._from_raw(arg)); return"
        "assert_never(arg)"

    getItem = case length nLst of
        1 -> pyFunc "get_item" ["self", "name : " <> a ] "Any"
                "return self._get_item(name)"
          where
            (_goff, _n, name, _title, _cls) = head nLst
            a = "Literal[" <> escaped name <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, ix) <- nLst
                pure $ do
                    line "@overload"
                    pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                        (nameOf ix)
                        "..."
            , pyFunc "get_item" ["self", "name : Union[" <> a <> "]"] "Any"
                "return self._get_item(name)"
            ]
          where
            a = mconcat $ intersperse ", " $ do
                (_goff, _n, name, _title, _ix) <- nLst
                pure $ "Literal[" <> escaped name <> "]"

    setItem = case length nLst of

        1 -> pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
            ("'" <> nameOf vc <> "'")
            "return self._set_item(name, val) # type: ignore"
          where
            (_goff, _n, name, _title, ix) = head nLst
            arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, ix) <- nLst
                let arg = "Union[" <> nameOf ix <> ", " <> argOf ix <> "]"
                pure $ do
                    line "@overload"
                    pyFunc "set_item" ["self", "name : Literal[" <> escaped name <> "]", "val : " <> arg]
                        ("'" <> nameOf vc <> "'")
                        "..."
            , pyFunc "set_item" ["self", "name : Any", "val : Any"] "Any"
                "return self._set_item(name, val)"
            ]

handleExtended :: VariationDb -> VariationIx
    -> ExtendedType -> RegisterSize -> RegisterSize -> [[(GroupOffset, RegisterSize, Item)]]
    -> BlockM Builder ()
handleExtended db vc et n1 n2 grps = do
    mconcat (fmap mkGrp grps')
    argUnion
    pyClass (nameOf vc) ["Extended"] $ blocksLn
        [ fmt "variation = 'Extended'"
        , constants
        , pyList "subitems_list" (fmap subitemsList grps)
        , subitemsDict
        , spec
        , initFunc
        , getItem
        ]
  where
    grps' :: [(Int, [(GroupOffset, RegisterSize, Item)])]
    grps' = zip [1..] (fmap mconcat (drop 1 $ inits grps))

    ag :: Int -> Text
    ag n = argOf vc <> "_Group_" <> sformat int n

    mkGrp :: (Int, [(GroupOffset, RegisterSize, Item)]) -> BlockM Builder ()
    mkGrp (n, lst) = enclose a b $ mconcat $ do
        (_goff, _n, name, _title, var) <- nonSpare lst
        let cls = nameOf $ indexOf db var
        pure $ fmt (stext % ": Union[" % stext % ", " % stext % "_Arg],")
            (escaped name) cls cls
      where
        a = fmt (stext % " = TypedDict('" % stext % "', {") (ag n) (ag n)
        b = "})"

    argUnion = enclose a "]" $ mconcat ("int," : args)
      where
        a = fmt (stext % " : TypeAlias = Union[") (argOf vc)
        args = do
            (n, _lst) <- grps'
            pure $ do
                fmt (stext % ",") (ag n)
                line $ "Tuple["
                    <> mconcat (intersperse "," (replicate n "int"))
                    <> "],"

    constants = do
        line $ "no_trailing_fx = " <> case et of
            ExtendedRegular -> "False"
            ExtendedNoTrailingFx -> "True"
        fmt ("prim_bit_size = " % int) n1
        fmt ("ext_bit_size = " % int) n2
        enclose "groups_bit_sizes = [" "]" $ mconcat $ do
            lst <- grps
            pure $ fmt (int % ",") (sum [n | (_,n,_) <- lst])

    subitemsList lst = enclose "[" "]," $ mconcat $ do
        (_goff, _n, item) <- lst
        pure $ case item of
            Spare o n -> fmt ("Spare(" % int % ", " % int % "),") (unOctetOffset o) n
            Item name _title var -> fmt
                ("(" % stext % ", " % stext % "),")
                (escaped name)
                (nameOf $ indexOf db var)

    subitemsDict = do
        line "# name: (title, cls, group_offset, bit_size)"
        pyDict "subitems_dict" $ do
            (goff, n, name, title, var) <- nonSpare (join grps)
            let cls = nameOf $ indexOf db var
            pure $ fmt (stext % ": (" % stext % ", " % stext % ", " % int % ", " % int % "),")
                (escaped name) (escaped title) cls goff n

    spec = subSpecs $ do
        (_goff, _n, name, _title, var) <- nonSpare (join grps)
        let cls = nameOf $ indexOf db var
        pure (name, cls)

    initFunc = pyFunc "__init__" ["self", "arg : " <> argOf vc] "None" $ do
        -- In the case of single group, the fx is present, see [ref:extended-no-trailing-fx].
        pyIf "isinstance(arg, int)"
            "super().__init__(*self._from_single_int(0, arg, False)); return"
        pyIf "isinstance(arg, tuple)" $ do
            pyIf "isinstance(arg[0], Bits)"
                "super().__init__(*arg); return"
            "super().__init__(*self._from_tuple_int(arg)); return"
        pyIf "isinstance(arg, dict)" $ do
            forM_ grps' $ \(n, _lst) -> do
                pyIf (ag n <> ".__required_keys__ == arg.keys()") $
                    fmt ("super().__init__(*self._from_dict(" % int % ", arg)); return") n
            "raise Exception('internal error: unexpected argument')"
        "assert_never(arg)"

    getItem = case length lst of
        1 -> pyFunc "get_item" ["self", "name : " <> a ] "Any"
                "return self._get_item(name)"
          where
            (_goff, _n, name, _title, _cls) = head lst
            a = "Literal[" <> escaped name <> "]"

        _ -> blocksLn
            [ blocksLn $ do
                (_goff, _n, name, _title, var) <- lst
                let cls = nameOf $ indexOf db var
                pure $ mconcat
                    [ "@overload"
                    , pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                        cls
                        "..."
                    ]
            , pyFunc "get_item" ["self", "name : Union[" <> a <> "]"] "Any"
                "return self._get_item(name)"
            ]
          where
            a = mconcat $ intersperse ", " $ do
                (_goff, _n, name, _title, _cls) <- lst
                pure $ "Literal[" <> escaped name <> "]"
      where
        lst = nonSpare (join grps)

handleRepetitive :: VariationDb -> VariationIx -> Int -> RegisterSize -> Variation -> BlockM Builder ()
handleRepetitive db vc repByteSize varBitSize var = do
    typeAlias (argOf vc) arg
    pyClass (nameOf vc) ["Repetitive"] $ blocksLn
        [ fmt "variation = 'Repetitive'"
        , constants
        , spec
        , initFunc
        ]
  where
    iv = indexOf db var
    arg = "List[Union[" <> nameOf iv <> ", " <> argOf iv <> "]]"

    cnV = nameOf $ indexOf db var

    constants = do
        fmt ("rep_byte_size = " % int) repByteSize
        fmt ("variation_bit_size = " % int) varBitSize
        fmt ("variation_type = " % stext) cnV

    spec = do
        line "@classmethod"
        pyFunc "spec" ["cls"] ("Type[" <> cnV <> "]") $ do
            fmt ("return " % stext) cnV

    initFunc = pyFunc "__init__" ["self", "arg : " <> arg] "None" $ do
        pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        pyIf "isinstance(arg, list)" "super().__init__(*self._from_list(arg)); return"
        "assert_never(arg)"

handleExplicit :: VariationIx -> BlockM Builder ()
handleExplicit vc = do
    typeAlias (argOf vc) "bytes"
    pyClass (nameOf vc) ["Explicit"] $ blocksLn
        [ fmt "variation = 'Explicit'"
        , initFunc
        ]
  where
    initFunc = pyFunc "__init__" ["self", "arg : bytes"] "None" $ mconcat
        [ pyIf "isinstance(arg, tuple)" "super().__init__(*arg); return"
        , pyIf "isinstance(arg, bytes)" "super().__init__(*self._from_bytes(arg)); return"
        , "assert_never(arg)"
        ]

handleCompound :: VariationDb -> VariationIx -> Maybe ByteSize -> ByteSize
    -> [Maybe (Name, Title, Variation, Fspec)]
    -> BlockM Builder ()
handleCompound db vc mn fspec_max_bytes lst = do
    hdr
    pyClass (nameOf vc) ["Compound"] $ blocksLn
        [ fmt "variation = 'Compound'"
        , constants
        , subitemsList
        , subitemsDict
        , spec
        , initFunc
        , setItem
        , delItem
        , getItem
        ]
  where
    hdr = enclose a b $ mconcat $ do
        (name, _title, var, _fspec) <- catMaybes lst
        let iv = indexOf db var
        pure $ fmt (stext % ": Union[" % stext % ", " % stext % "],")
            (escaped name)
            (nameOf iv)
            (argOf iv)
      where
        a = fmt (stext % " = TypedDict('" % stext % "', {") (argOf vc) (argOf vc)
        b = "}, total=False)"

    constants = do
        fmt ("fspec_fx = " % stext) (maybe "True" (const "False") mn)
        fmt ("fspec_max_bytes = " % int) fspec_max_bytes

    subitemsList = enclose "subitems_list = [" "]" $ mconcat $ do
        mItem <- lst
        pure $ case mItem of
            Nothing -> "None,"
            Just (name, _title, var, _fspec) ->
                fmt ("(" % stext % ", " % stext % "),")
                    (escaped name)
                    (nameOf $ indexOf db var)

    showFspec :: Fspec -> Text
    showFspec fspecs =
        let f = T.pack . reverse . take 2 . (<> "00") . reverse . flip showHex ""
        in mconcat $ fmap f fspecs

    subitemsDict = do
        line "# name: (cls, fspec)"
        enclose "subitems_dict = {" "}" $ mconcat $ do
            (name, _title, var, fspec) <- catMaybes lst
            pure $ fmt (stext % ": (" % stext % ", 0x" % stext % "),")
                (escaped name)
                (nameOf $ indexOf db var)
                (showFspec fspec)

    spec = subSpecs $ do
        (name, _title, var, _fspec) <- catMaybes lst
        pure (name, nameOf $ indexOf db var)

    initFunc = pyFunc "__init__" ["self", "arg : Optional[" <> argOf vc <> "] = None"] "None" $ do
        pyIf "isinstance(arg, tuple)"
            "super().__init__(*arg); return"
        line "super().__init__()"
        line "if arg is not None: self._update(arg)"

    setItem = blocksLn
        [ blocksLn $ do
            (name, _title, var, _fspec) <- catMaybes lst
            let iv = indexOf db var
                arg = "Union[" <> (nameOf iv) <> ", " <> argOf iv <> "]"
            pure $ do
                line "@overload"
                pyFunc "set_item"
                    [ "self"
                    , "name : Literal[" <> escaped name <> "]"
                    , "val : " <> arg
                    ]
                    ("'" <> nameOf vc <> "'")
                    "..."
        , pyFunc "set_item" ["self", "name : Any, val : Any"] "Any"
            "return self._set_item(name, val)"
        ]

    delItem = blocksLn
        [ blocksLn $ do
            (name, _title, _var, _fspec) <- catMaybes lst
            pure $ do
                line "@overload"
                pyFunc "del_item" ["self", "name : Literal[" <> escaped name <> "]"]
                    ("'" <> nameOf vc <> "'")
                    "..."
        , pyFunc "del_item" ["self", "name : Any"] "Any"
            "return self._del_item(name)"
        ]

    getItem = blocksLn
        [ blocksLn $ do
            (name, _title, var, _fspec) <- catMaybes lst
            pure $ mconcat
                [ "@overload"
                , pyFunc "get_item" ["self", "name : Literal[" <> escaped name <> "]"]
                    (nameOf $ indexOf db var)
                    "..."
                ]
        , pyFunc "get_item" ["self", "name : Any"] "Any"
            "return self._get_item(name)"
        ]

-- | Create 'Block', representing a 'Variation', call proper handlers.
variationBlock :: VariationDb -> VariationIx -> Variation -> BlockM Builder ()
variationBlock db vc variation = case variation of
    Element o n cont -> handleElement vc o n cont
    Group lst -> handleGroup db vc lst
    Extended et n1 n2 grps -> handleExtended db vc et n1 n2 grps
    Repetitive repByteSize varBitSize var2 -> handleRepetitive db vc repByteSize varBitSize var2
    Explicit -> handleExplicit vc
    Compound mn fspec_max_bytes lst -> handleCompound db vc mn fspec_max_bytes lst

-- | Create top-level variations.
programVariations :: Bool -> VariationDb -> BlockM Builder ()
programVariations includeComments db = blocksLn $ do
    (n, paths, var) <- lst
    pure $ do
        when includeComments $ forM_ (Set.toList paths) $ \path -> do
            line $ BL.fromText $ "# " <> tPath path
        variationBlock db n var
  where
    lst :: [(VariationIx, Set Path, Variation)]
    lst
        = sortOn (\(n,_,_) -> n)
        $ flip Map.foldMapWithKey db $ \var (n, paths) -> [(n, paths, var)]

-- | Create top-level asterix spec.
handleSpec :: VariationDb -> Asterix -> BlockM Builder ()
handleSpec db ast = case astType ast of
    AstCat uap -> pyClass (specName ast) ["Basic"] $ case uap of
        Uap var -> blocksLn
            [ catLine >> varLine (nameOf $ iv var)
            , specLine
            , parseBits, unparseBits
            , fMkRecord (iv var)
            , fMkDatablock (nameOf $ iv var)
            , fParseSingle (nameOf $ iv var)
            ]
        Uaps uaps msel -> blocksLn
            [ catLine
            , fUap uaps msel
            , fMkRecordUnsafe uaps
            , fMkDatablock $ "Union[" <> tList [nameOf (iv var) | (_name, var) <- uaps] <> "]"
            , fParseMultiple uaps msel
            , maybe (pure ()) (fIsValid uaps) msel
            ]
    AstRef var -> pyClass (specName ast) ["Expansion"] $ blocksLn
        [ catLine >> varLine (nameOf $ iv var)
        , specLine
        , parseBits, unparseBits
        , fMkExtended (iv var)
        , fParseExpansion (nameOf $ iv var)
        ]
  where

    iv = indexOf db

    catLine = fmt ("cat = " % int) (astCat ast)

    varLine cls = fmt ("variation = " % stext) cls

    specLine = "spec = variation.spec"

    parseBits = "parse_bits = variation.parse_bits"

    unparseBits = "unparse_bits = variation.unparse_bits"

    fMkRecord ix = do
        line "@classmethod"
        pyFunc "make_record" ["cls, val : " <> argOf ix] (nameOf ix) $ do
            fmt ("return " % stext % "(val)") (nameOf ix)

    fMkRecordUnsafe uaps = blocksLn [mapM_ overload uaps, func]
      where
        overload (name, var) = do
            let ix = indexOf db var
            line "@overload"
            line "@classmethod"
            pyFunc "make_record_unsafe"
                ["cls, uap : Literal['" <> name <> "'], arg : " <> argOf ix]
                (nameOf ix)
                "..."

        func = do
            line "@classmethod"
            pyFunc "make_record_unsafe"
                ["cls", "uap : Any", "arg : Any"]
                "Any"
                "return cls.uaps[uap](arg)"

    fMkDatablock arg = do
        line "@classmethod"
        pyFunc "make_datablock"
            ["cls, val : Union[" <> arg <> ", List[" <> arg <> "]]"]
            ("Datablock[" <> arg <> "]")
            "return Datablock(cls.cat, val)"

    fParseSingle arg = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : RawDatablock"]
            ("Datablock[" <> arg <> "]")
            "return cls._parse(val) # type: ignore"

    fParseMultiple uaps msel = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : RawDatablock", "uap : " <> uapArg]
            "Any"
            "return cls._parse(val, uap=uap)"
      where
        uapSelection = tList [T.pack (show name) | name <- fmap fst uaps]
        uapArg = case msel of
            Nothing -> "Literal[" <> uapSelection <> "]"
            Just _sel -> "Optional[Literal[" <> uapSelection <> "]] = None"

    fMkExtended ix = do
        line "@classmethod"
        pyFunc "make_extended"
            ["cls", "val : " <> argOf ix]
            (nameOf ix)
            (fmt ("return " % stext % "(val)") (nameOf ix))

    fParseExpansion cls = do
        line "@classmethod"
        pyFunc "parse"
            ["cls", "val : bytes"]
            cls $ do
                line "s = Bits.from_bytes(val)"
                fmt ("(rec, s2) = " % stext % ".parse_bits(s)") cls
                pyIf "len(s2) != 0"
                    "raise AsterixError('unable to parse expansion')"
                line "return rec # type: ignore"

    fUap uaps msel = blocksLn
        [ pyDict "uaps" $ do
            (name, var) <- uaps
            pure $ fmt ("'" % stext % "': " % stext % ",")
                name
                (nameOf $ indexOf db var)
        , blocksLn $ case msel of
            Nothing ->
                [ "uap_selector_item = None"
                , "uap_selector_table = None"
                ]
            Just (UapSelector item table) ->
                [ fmt ("uap_selector_item = [" % stext % "]") (tList (fmap (T.pack . show) item))
                , pyDict "uap_selector_table" $ do
                    (val, name) <- table
                    pure $ fmt (int % ": '" % stext % "',") val name
                ]
        , do
            line "@classmethod"
            pyFunc "spec"
                ["cls", "val : Literal[" <> tList (fmap (T.pack . show . fst) uaps) <> "]" ]
                ("Any")
                "return cls.uaps[val]"
        ]

    fIsValid uaps _sel = do
        line "@classmethod"
        pyFunc "is_valid"
            ["cls", "arg : Union[" <> tList args <> "]"]
            ("bool")
            "return cls._is_valid(arg)"
      where
        args = do
            (_name, var) <- uaps
            pure $ nameOf $ indexOf db var

programSpecs :: VariationDb -> [Asterix] -> BlockM Builder ()
programSpecs db lst = blocksLn $ fmap (handleSpec db) lst

-- | Create 'manifest' of all defined specs.
programManifest :: [Asterix] -> BlockM Builder ()
programManifest specs = enclose "manifest = {" "}" $ do
    enclose "'CATS': {" "}," $ mconcat $ go "CAT"
    enclose "'REFS': {" "}," $ mconcat $ go "REF"
  where
    lst = do
        spec@(Asterix cat ed at) <- specs
        pure $ case at of
            AstCat _ -> ("CAT", (cat, (ed, specName spec)))
            AstRef _ -> ("REF", (cat, (ed, specName spec)))

    go :: Text -> [BlockM Builder ()]
    go t = do
        let candidates = [b | (a, b) <- lst, a == t]
        cat <- sort $ nub $ fmap fst candidates
        let hdr = fmt (int % ": {") cat
        pure $ enclose hdr "}," $ mconcat $ do
            (Edition eMaj eMin, cls) <- sortOn fst [b | (a, b) <- candidates, a == cat]
            let edition = sformat ("'" % int % "." % int % "'") eMaj eMin
            pure $ fmt stext ( edition <> ": " <> cls <> ",")

versionText :: Integer -> Text
versionText unixtime = sformat
    (int % left 2 '0' % left 2 '0' % "." % int)
    year month day (seconds :: Integer)
  where
    Clock.UTCTime uDay uTime = Time.posixSecondsToUTCTime $ fromIntegral unixtime
    (year, month, day) = Data.Time.Calendar.toGregorian uDay
    seconds = round uTime

programVersion :: Integer -> Text -> BlockM Builder ()
programVersion unixtime reference = blocksLn
    [ fmt ("VERSION = '" % stext % "'") (versionText unixtime)
    , fmt ("REFERENCE = '" % stext % "'") reference
    ]

-- | Helper function to order specs.
compareSpecs :: Asterix -> Asterix -> Ordering
compareSpecs a b
    = compare (astType a) (astType b)
   <> compare (astCat a) (astCat b)
   <> compare (astEdition a) (astEdition b)

-- | Create generated code.
mkGeneratedCode :: Bool -> Integer -> Text -> Set Asterix -> BlockM Builder ()
mkGeneratedCode includeComments unixtime reference specsSet = blocksLn
    [ "# --- Generated code ---"
    , programVariations includeComments db
    , programSpecs db specs
    , programManifest specs
    , programVersion unixtime reference
    ]
  where
    db :: VariationDb
    db = variationDb specsSet

    specs :: [Asterix]
    specs = sortBy compareSpecs $ Set.toList specsSet

-- | Import generic code.
genericCodeBS :: BS.ByteString
genericCodeBS = $(makeRelativeToProject "lib/Asterix/Language/Python/generic.py" >>= embedFile)

-- | Source code generator entry point.
mkCode :: Bool -> Integer -> Text -> Set Asterix -> TL.Text
mkCode includeComments unixtime reference specs =
    headerCode <> "\n" <> genericCode <> generatedCode
  where
    genericCode = either (error . show) id $ TL.decodeUtf8' $ BSL.fromStrict $ genericCodeBS
    generatedCode = BL.toLazyText $ renderBlockM 4 $ mkGeneratedCode includeComments unixtime reference specs
    headerCode = mconcat $ intersperse "\n"
        [ "#!/usr/bin/env python"
        , "# -*- coding: utf-8 -*-"
        , "#"
        , "# This file is generated, DO NOT EDIT!"
        , "# For more details, see:"
        , "#    - https://github.com/zoranbosnjak/asterix-lib-generator"
        , "#    - https://github.com/zoranbosnjak/asterix-specs"
        ]

