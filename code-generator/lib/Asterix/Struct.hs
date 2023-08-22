-- | Derived asterix structures and helper functions.
--
-- Use intermediate data types, to simplify code generation.
--
-- 'Dependent Rule' is simplified to 'Raw' content.
--
-- Random Field Sequencing is not supported.
--
-- Sizes and offsets are calculated from the original structure.
--
-- 'VariationDb' - Variation database is a collection of all subitems, without
-- duplications (the same definitions can be reused). For example,
-- 'I010/SAC' and 'I010/SIC' fields normally share exactly the same structure,
-- so it is stored in a database only once.
--
-- [tag:extended-no-trailing-fx]
-- In case of no-trailing-fx type of extended item, the length of extended
-- groups must be > 1, otherwise it would be the same as 'group'. The regular
-- extended item however can contain a single group (with the 'fx' at the end).

module Asterix.Struct where

import           Control.Monad.State
import           Control.Applicative (empty)
import           Data.List (unfoldr)
import           Data.Maybe (listToMaybe, isJust, catMaybes, fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Bool
import           Data.Word
import qualified Data.Text as T
import           Data.Text (Text)
import           Formatting as F

import qualified Asterix.Specs as A

-- | Element offset from the start of the group
type GroupOffset = Int

-- | Offset inside octet [0..7]
newtype OctetOffset = OctetOffset {unOctetOffset :: Int}
    deriving (Eq, Ord)

instance Show OctetOffset where
    show = show . unOctetOffset

instance Semigroup OctetOffset where
    OctetOffset x <> OctetOffset y = OctetOffset (mod (x+y) 8)

instance Monoid OctetOffset where
    mempty = OctetOffset 0

-- | Octet offset smart constructor
octetOffset :: Int -> OctetOffset
octetOffset n = OctetOffset (mod n 8)

-- | Item full name in reverse, like ["SAC", "010", "CAT_002"]
type Path = [A.Name]

-- | Size is bytes
type ByteSize = Int

-- | Fspec as list of octets
type Fspec = [Word8]

-- | Derived content
data Content
    = ContentRaw
    | ContentTable [(Int, Text)]
    | ContentString A.StringType
    | ContentQuantity A.Signed Double A.FractBits A.Unit
    deriving (Eq, Ord, Show)

-- | Derived variation
data Variation
    = Element OctetOffset A.RegisterSize Content
    | Group [(GroupOffset, A.RegisterSize, Item)]
    | Extended A.ExtendedType A.RegisterSize A.RegisterSize
        [[(GroupOffset, A.RegisterSize, Item)]]
    | Repetitive A.RepetitiveType A.RegisterSize Variation
    | Explicit (Maybe A.ExplicitType)
    | Compound (Maybe ByteSize) ByteSize [Maybe (A.Name, A.Title, Variation, Fspec)]
    deriving (Eq, Ord, Show)

-- | Derived item
data Item
    = Spare OctetOffset A.RegisterSize
    | Item A.Name A.Title Variation
    deriving (Eq, Ord, Show)

-- | Category number
type Cat = Int

-- | Derived uap structure
data Uap
    = Uap Variation
    | Uaps [(A.UapName, Variation)] (Maybe A.UapSelector)
    deriving (Eq, Ord, Show)

-- | Type of asterix spec
data AstSpec
    = AstCat Uap
    | AstRef Variation
    deriving (Eq, Show)

instance Ord AstSpec where
    compare (AstCat _) (AstRef _) = LT
    compare (AstRef _) (AstCat _) = GT
    compare (AstCat _) (AstCat _) = EQ
    compare (AstRef _) (AstRef _) = EQ

-- | Derived Asterix structure
data Asterix = Asterix
    { astCat :: Cat
    , astEdition :: A.Edition
    , astSpec :: AstSpec
    } deriving (Eq, Ord, Show)

-- | Show path as text.
tPath :: Path -> Text
tPath = T.pack . show . reverse

-- | Helper function to order specs.
compareSpecs :: Asterix -> Asterix -> Ordering
compareSpecs a b
    = compare (astSpec a) (astSpec b)
   <> compare (astCat a) (astCat b)
   <> compare (astEdition a) (astEdition b)

-- | Assert assumption
assert :: Applicative f => String -> Bool -> f ()
assert _msg True = pure ()
assert msg False = error msg

-- | Assert byte alignment
byteAligned :: Path -> String -> State OctetOffset ()
byteAligned path msg = do
    o <- get
    assert (show path <> ", " <> msg <> ": bit offset " <> show o) (o == mempty)

-- | Derive content
deriveContent :: A.Rule -> Content
deriveContent (A.Dependent _ _) = ContentRaw
deriveContent (A.ContextFree content) = case content of
    A.ContentRaw -> ContentRaw
    A.ContentTable lst -> ContentTable lst
    A.ContentString st -> ContentString st
    A.ContentInteger _signed _cont -> ContentRaw
    A.ContentQuantity sig num k unit _con ->
        let n = fromRational $ case num of
                A.NumberZ val -> toRational val
                A.NumberQ val -> val
                A.NumberR val -> val
        in ContentQuantity sig n k unit
    A.ContentBds _bds -> ContentRaw

-- | Calculate fixed items size (assume fixed item)
sizeOf :: Item -> Int
sizeOf = \case
    Spare _o n -> n
    Item name _title var -> case var of
        Element _o n _cont -> n
        Group lst -> foldr (+) 0 $ do
            (_offset, n, _i) <- lst
            pure n
        _ -> error $ "non-fixed item " <> show name

-- | Split list of 'Maybe' values to the lists of equal size append 'Nothing'
chunksOf :: Int -> [Maybe a] -> [[Maybe a]]
chunksOf n = unfoldr f
  where
    f [] = Nothing
    f lst =
        let (a,b) = splitAt n lst
        in Just (take n (a <> repeat Nothing), b)

-- | Fspec weight of selected (by name) item
fspecChunkOf :: Num a => A.Name -> [Maybe A.Item] -> Maybe a
fspecChunkOf name lst = listToMaybe $ do
    (x, i) <- zip [(0::Int)..] (reverse lst)
    item <- maybe empty pure i
    name2 <- case item of
        A.Spare _ -> empty
        A.Item name2 _title _var _doc -> pure name2
    guard $ name == name2
    pure (2^x)

-- | Boolean implication.
implies :: Bool -> Bool -> Bool
implies True b = b
implies False _ = True

-- | Derive variation
deriveVariationS :: Path -> A.Variation -> State OctetOffset Variation
deriveVariationS path = \case
    A.Element n rule -> do
        o <- get
        modify (<> octetOffset n)
        pure $ Element o n (deriveContent rule)
    A.Group lst -> do
        result <- mapM (deriveItemS path) lst
        let loop _ [] = []
            loop ix (x:xs) = ((ix,sizeOf x,x) : loop (ix + sizeOf x) xs)
        pure $ Group $ loop 0 result
    A.Extended et n1 n2 lst -> do
        groups' <- mapM handleGroup groups
        -- Check the length, to satisfy [ref:extended-no-trailing-fx].
        assert "single trailing" $ (et == A.ExtendedNoTrailingFx) `implies` (length lst > 1)
        pure $ Extended et n1 n2 groups'
      where
        groups :: [[A.Item]]
        groups = fromJust $ A.extendedItemGroups et n1 n2 lst

        handleGroup :: [A.Item] -> State OctetOffset [(GroupOffset, A.RegisterSize, Item)]
        handleGroup [] = do
            put mempty
            pure []
        handleGroup (item:items) = do
            o <- get
            i <- deriveItemS path item
            rest <- handleGroup items
            pure ((unOctetOffset o, sizeOf i, i):rest)
    A.Repetitive rt var -> do
        byteAligned path "repetitive (pre)"
        var2 <- (deriveVariationS path) var
        case rt of
            A.RepetitiveRegular _ -> pure ()
            A.RepetitiveFx -> modify (<> octetOffset 1) -- FX bit
        byteAligned path "repetitive (post)"
        pure $ Repetitive rt (sizeOf $ Item mempty mempty var2) var2
    A.Explicit et -> do
        byteAligned path "explicit (pre)"
        pure $ Explicit et
    A.RandomFieldSequencing -> do
        error "Random Field Sequencing is not supported."
    A.Compound mn lst' -> do
        byteAligned path "compound (pre)"
        result <- Compound
            <$> pure mn
            <*> pure fspecMaxBytes
            <*> mapM stripMaybeItem lst
        byteAligned path "compound (post)"
        pure result
      where
        removeRfs = \case
            Just (A.Item _name _title A.RandomFieldSequencing _doc) -> Nothing
            other -> other
        lst = fmap removeRfs lst'
        fspecMaxBytes :: ByteSize
        fspecMaxBytes = case mn of
            Just m -> case divMod m 8 of    -- no FX
                (n, 0) -> n
                _ -> error "unexpected fx bit size"
            Nothing -> case divMod (length lst) 7 of    -- 1 bit for FX
                (n, 0) -> n
                (n, _) -> succ n
        fspecOf :: A.Name -> Fspec
        fspecOf name = case mn of
            Just _n ->
                let itemGroups = chunksOf 8 $ take (fspecMaxBytes * 8) (lst <> repeat Nothing)
                    fspecs = fspecChunkOf name <$> itemGroups
                    results = maybe 0 id <$> fspecs
                in case length (catMaybes fspecs) == 1 of
                    False -> error "unexpected fspecs bits"
                    True -> results
            Nothing ->
                -- If a particular FSPEC bit is set,
                -- all FX bits left of this bit must be set too.
                let itemGroups = reverse $ chunksOf 7 lst
                    fspecs = fspecChunkOf name <$> itemGroups
                    -- Infinite lists of FX flags, once the flag is set,
                    -- it remains set forever. It's processed in reverse.
                    fxBits = False : zipWith (\a b -> a || isJust b) fxBits fspecs
                    results = do
                        (mVal, fxFlag) <- zip fspecs fxBits
                        let val = maybe 0 (*2) mVal
                            fx = bool 0 1 fxFlag
                        pure $ val+fx
                in case length (catMaybes fspecs) == 1 of
                    False -> error "unexpected fspecs bits"
                    True -> reverse results
        stripMaybeItem Nothing = pure Nothing
        stripMaybeItem (Just i) = Just <$> do
            item <- (deriveItemS path) i
            case item of
                Spare _ _ -> error "Unexpected spare item in compound"
                Item name title variation -> do
                    pure (name, title, variation, fspecOf name)

-- | Derive item
deriveItemS :: Path -> A.Item -> State OctetOffset Item
deriveItemS path = \case
    A.Spare n -> do
        o <- get
        modify (<> octetOffset n)
        pure $ Spare o n
    A.Item name title var _doc -> Item
        <$> pure name
        <*> pure title
        <*> (deriveVariationS (name:path)) var

-- | Create toplevel compound item (which represents a category).
mkToplevel :: [A.Item] -> [Maybe A.Name] -> A.Variation
mkToplevel catalogue uap = A.Compound Nothing (fmap (fmap findItem) uap)
  where
    findItem :: A.Name -> A.Item
    findItem name = go catalogue
      where
        go [] = error "Item not found"
        go (A.Spare _n : xs) = go xs
        go (x@(A.Item iName _title _var _doc) : xs)
            | iName == name = x
            | otherwise = go xs

showCat :: Cat -> Text
showCat = sformat (left 3 '0')

showEdition :: A.Edition -> Text
showEdition (A.Edition eMaj eMin) = sformat (int % "_" % int) eMaj eMin

-- | Name of the spec, for example "CAT_002_1_2"
specName :: Asterix -> Text
specName (Asterix cat ed at) =
    sformat (stext % "_" % stext % "_" % stext) (t at) (showCat cat) (showEdition ed)
  where
    t (AstCat _) = "CAT"
    t (AstRef _) = "REF"

-- | Derive variation and check offset
deriveVariation :: Bool -> Path -> A.Variation -> Variation
deriveVariation isTop path var1 = case isTop of
    False -> var2
    True -> case o == mempty of
        True -> var2
        False -> error $ show (reverse path) <> ": unexpected offset"
  where
    (var2, o) = runState (deriveVariationS path var1) mempty

-- | Derive toplevel asterix spec
deriveAsterix :: A.Asterix -> Asterix
deriveAsterix = \case
    A.AsterixBasic (A.Basic cat _tit ed _date _pre catalogue auap) ->
        let t = "CAT"
            mkTopVariation name lst = deriveVariation True name $ mkToplevel catalogue lst
        in Asterix cat ed $ case auap of
            A.Uap lst -> AstCat $ Uap $ mkTopVariation (parent t cat ed) lst
            A.Uaps lsts mSel ->
                let path = parent t cat ed
                    uaps = [(uap, mkTopVariation (uap:path) lst) | (uap, lst) <- lsts]
                in AstCat $ Uaps uaps mSel
    A.AsterixExpansion (A.Expansion cat _tit ed _data var) ->
        Asterix cat ed (AstRef $ deriveVariation True (parent "REF" cat ed) var)
  where
    parent t cat ed = [showEdition ed, showCat cat, t]

-- | Database with all variations, indices and references
type VariationIx = Int
type VariationDb = Map Variation (VariationIx, Set Path)

-- | Helper function to find variation index from the variation database.
indexOf :: VariationDb -> Variation -> VariationIx
indexOf db subvar = fst $ db Map.! subvar

-- | Name of variation with given index.
nameOf :: VariationIx -> Text
nameOf vc = sformat ("Variation_" % int) vc

-- | Recursively save variation to the database
saveVariation :: Path -> Variation -> State VariationDb ()
saveVariation path var = do
    -- save childs
    case var of
        Element _ _ _ -> pure ()
        Group lst -> forM_ lst $ \(_of, _n, item) -> case item of
            Spare _ _ -> pure ()
            Item name _title var2 -> saveVariation (name:path) var2
        Extended _et _n1 _n2 grps -> forM_ (join grps) $ \(_of, _n, item) -> case item of
            Spare _ _ -> pure ()
            Item name _title var2 -> saveVariation (name:path) var2
        Repetitive _repByteSize _varBitSize var2 -> saveVariation path var2
        Explicit _et -> pure ()
        Compound _mn _max_b lst -> forM_ lst $ \case
            Nothing -> pure ()
            Just (name,_title,var2,_fspec) -> saveVariation (name:path) var2
    -- save variation itself
    modify $ \db ->
        let x = case Map.lookup var db of
                Nothing -> (Map.size db, Set.singleton path)
                Just (n, paths) -> (n, Set.insert path paths)
        in Map.insert var x db

-- | Create variation database
variationDb :: Set Asterix -> VariationDb
variationDb specs = execState (mapM_ save specs) mempty
  where
    save ast = case astSpec ast of
        AstCat uap -> case uap of
            Uap var -> saveVariation [specName ast] var
            Uaps lst _ -> forM_ lst $ \(name, var) -> do
                saveVariation [name, specName ast] var
        AstRef var -> saveVariation [specName ast] var

