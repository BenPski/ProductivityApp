{-# LANGUAGE DeriveGeneric, OverloadedStrings, Arrows #-}
module Productivity where

--for Text
import Data.Text
import Data.Text.Serialize

--for JSON representations
import Data.Aeson
import GHC.Generics
import Data.Serialize

--for collections
import Data.IntMap as IM

--auto
import Prelude hiding          ((.), id)
import Control.Auto
import Control.Auto.Serialize
import Control.Auto.Collection
import Control.Auto.Blip
import Data.Profunctor
import Control.Monad.IO.Class

{-
Lets organize a bit more

-}

--not that important, useful only for filling in definitions
data TodoItem = TodoItem {
    itemName :: Text,
    itemNotes :: [Text]
} deriving (Eq, Show, Generic)

instance Serialize TodoItem
instance ToJSON TodoItem
instance FromJSON TodoItem


{-
First we begin with the most basic element of a Productivity app, the elements to track
constists of a name and the informatoin it stores

a should generally be a monoid (enforce this somehow?)
-}

data ProdElement a = ProdElement {
    elemIdentity :: Text,
    elemInfo :: a
} deriving (Eq, Show, Generic)

instance Semigroup a => Semigroup (ProdElement a) where
    (ProdElement ident1 info1) <> (ProdElement ident2 info2) = ProdElement (ident1 <> ident2) (info1 <> info2)

instance Monoid a => Monoid (ProdElement a) where
    mempty = ProdElement mempty mempty

instance (Serialize a) => Serialize (ProdElement a)
instance (ToJSON a) => ToJSON (ProdElement a)
instance (FromJSON a) => FromJSON (ProdElement a)

--basic manipulation of an element
setIdentity x v = x {elemIdentity = v}
setInfo x v = x {elemInfo = v}
modInfo x@(ProdElement _ v) f = x {elemInfo = f v}


{-
Now we need a way of manipulating an exisiting element
the basic ways are do nothing, delete it, and to modify it

due to the function depenence of modify can't define an Eq instance
-}
data ElemCommand a = ElemNothing --do nothing
                   | ElemDelete --delete the item from the collection
                   | ElemModify (ProdElement a -> ProdElement a) --a way to modify a particular element (give a function :: ProdElement a -> ProdElement a)

{-
If we want to manipulate many at a time we can now handle
doing nothing, making new things, or doing something to a specific one
-}
data ProdCommand a = ProdNothing --do nothing
                   | ProdNew Text --setup a new one with the identifier
                   | ProdModify Int (ElemCommand a) --pass along another command to a specific item in the collection


{-
In the end want an auto wrapping together all the individually defined apps so the types have to unify
that leads to the input commands being a sum type on the different apps
and for the outputs to be a collection of all the outputs

do have to extend these whenever a new app is created
-}
type ProdCollection a = (IM.IntMap (ProdElement a)) --shorten the definition a bit
data Command = NotesCommand (ProdCommand Text)
             | TodosCommand (ProdCommand [TodoItem])

--the outputs are wrappers around the ProdCollection
data Collection = Collection {
    collectNotes :: ProdCollection Text,
    collectTodos :: ProdCollection [TodoItem]
} deriving (Eq, Show, Generic)

instance ToJSON Collection
instance FromJSON Collection

instance Semigroup Collection where
    (Collection a b) <> (Collection c d) = Collection (a<>c) (b<>d)

instance Monoid Collection where
    mempty = Collection mempty mempty


{-
The final type of auto to generate
-}
type ProdAuto m = Auto m Command Collection



{-
Now the auto definitions

need a way of initializing and building up an auto for a specific element
dealing with a collection of elements
one that handles commands
then one that puts it all together
-}
initElement :: (Serialize a, Monoid a) => Text -> Auto m (ElemCommand a) (Maybe (ProdElement a))
initElement ident = accum modify (Just $ mempty {elemIdentity = ident})
    where
        modify Nothing _ = Nothing
        modify v ElemNothing = v
        modify _ ElemDelete = Nothing
        modify (Just x) (ElemModify f) = Just $ f x

collectionAuto :: (Serialize a, Monoid a, Monad m) => Auto m (IM.IntMap (ElemCommand a), Blip [Text]) (ProdCollection a)
collectionAuto = dynMapF initElement ElemNothing

prodsApp :: (Serialize a, Monoid a, Monad m) => Auto m (ProdCommand a) (ProdCollection a)
prodsApp = proc input -> do
        --splitting up the inputs
        newProdBlip <- emitJusts getNewEvents -< input
        nothingProdBlip <- emitJusts getNothingEvents -< input
        modProdBlip <- emitJusts getModifyEvents -< input

        let allCommands = nothingProdBlip <> modProdBlip

        --inputs
        prodsCommands <- fromBlips IM.empty -< allCommands

        --sending in the commands to do and the signals to intialize
        prodsMap <- collectionAuto -< (prodsCommands, newProdBlip)

        id -< prodsMap --spiit it out
        where
            getEvent predicate extract v = if predicate v then Just $ extract v else Nothing

            getNothingEvents = getEvent (\x -> case x of ProdNothing -> True; _ -> False) (const IM.empty)
            getNewEvents = getEvent (\x -> case x of (ProdNew _) -> True; _ -> False) (\(ProdNew str) -> [str])
            getModifyEvents = getEvent (\x -> case x of (ProdModify _ _) -> True; _ -> False) (\(ProdModify index cmd) -> IM.singleton index cmd)



{-
This is the putting it all together and it seems like it could be generalized a bit more
-}

notesApp :: MonadIO m => Auto m (ProdCommand Text) (ProdCollection Text)
notesApp = serializing "/home/ben/.bski/todo/notes.dat" prodsApp

todosApp :: MonadIO m => Auto m (ProdCommand [TodoItem]) (ProdCollection [TodoItem])
todosApp = serializing "/home/ben/.bski/todo/todos2.dat" prodsApp


fullApp :: MonadIO m => Auto m Command Collection
fullApp = proc input -> do
    --first split up into blips of noteCommands and todoCommands
    noteCmd <- fromBlips ProdNothing . emitJusts noteCommands -< input
    todoCmd <- fromBlips ProdNothing . emitJusts todoCommands -< input

    --pass into the appropriate autos
    notesRes <- notesApp -< noteCmd
    todosRes <- todosApp -< todoCmd

    --put them in the general collection
    let notesRes' = mempty {collectNotes = notesRes}
    let todosRes' = mempty {collectTodos = todosRes}

    --merge the outputs
    id -< mconcat [notesRes', todosRes']

    where
        --would be nice to have a way of going to a maybe from these things
        noteCommands (NotesCommand a) = Just a
        noteCommands _ = Nothing
        todoCommands (TodosCommand a) = Just a
        todoCommands _ = Nothing
