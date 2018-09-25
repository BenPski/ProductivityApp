{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Productivity
import Network.Wai
import Data.Text
import Text.Read (readMaybe)


{-
For now parsing get requests

first have to split based on which app it is for (notes, todos, etc)
then do the individual parsing

-}

parseRequest path = case path of
    ("NOTE":xs) -> NotesCommand (parseCommand notesInfo xs)
    ("TODO":xs) -> TodosCommand (parseCommand todosInfo xs)
    _ -> NotesCommand (ProdNothing) --not sure if there should be a better default, but just want to do nothing


--for notes there is only saving
notesInfo ("save":n:content:_) = modify n $ ElemModify (flip setInfo content)
notesInfo _ = ProdNothing --id --sort of dorky, may need more control over what is given

{-for todos there is
adding an item
removing an item
adding a note
removing a note
-}
todosInfo ("add":n:item:_) = modify n $ ElemModify (\x@(ProdElement _ info) -> let item' = TodoItem {itemName = item, itemNotes = []} in x {elemInfo = item' : info })
todosInfo ("remove":n:item:_) = modify n $ ElemModify (\x@(ProdElement _ info) -> let info' = Prelude.filter (\y -> (itemName y) /= item) info
                                                                                  in x {elemInfo = info'})
todosInfo ("noteAdd":n:item:note:_) = modify n $ ElemModify (\x@(ProdElement _ info) -> let info' = fmap (\x@(TodoItem name notes) ->  if name == item then x {itemNotes = note : notes} else x) info
                                                                                        in x {elemInfo = info'})
todosInfo ("noteRemove":n:item:note:_) = modify n $ ElemModify (\x@(ProdElement _ info) -> let info' = fmap (\y@(TodoItem name notes) -> if name == item then y {itemNotes = Prelude.filter (/=note) notes} else y) info
                                                                                            in x {elemInfo = info'})
todosInfo _ = ProdNothing

--in general a parse command is new, delete, set identitiy, and some command for the info (alos nothing on a parse error)
--new, delete, and title are all generic, everything else modifies the info and has to be handled specfifically
parseCommand :: ([Text] -> ProdCommand a) -> [Text] -> ProdCommand a
parseCommand f path = case path of
                        ("new":ident:_) -> ProdNew ident
                        ("delete":n:_) -> modify n ElemDelete
                        ("title":n:title:_) -> modify n $ ElemModify (flip setIdentity title)
                        xs -> f xs

--very common pattern of select the int and apply a function
modify n f = case readMaybe (unpack n) of
                (Just v) -> ProdModify v f
                _ -> ProdNothing
