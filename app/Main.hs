{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (mapM_)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Data.GI.Base
import           Control.Monad 
import           Data.Text (Text, pack)
import           Data.GI.Base.GType (gtypeString)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

setValuesToListStore :: Gtk.ListStore ->  [(Text, Text)] -> Int -> IO Gtk.TreeIter

setValuesToListStore lsts artx counter = do

             let (stT::Text, stT2::Text) = artx !! counter

             gv <- toGValue (Just stT)
             gv2 <- toGValue (Just stT2)

             if (counter == (length artx)-1) then Gtk.listStoreInsertWithValuesv lsts (-1) [0, 1] [gv, gv2]              
             else do               
               m <- (Gtk.listStoreInsertWithValuesv lsts (-1) [0, 1] [gv, gv2])
               setValuesToListStore lsts artx (counter + 1)

main :: IO ()
main = do
    conn <- open "test.db"
    memos <- query_ conn "SELECT id, comment from memos;"
    close conn

    let memos2 = memos :: [(Int, Text)]

    let demoList = map (\(f, s) -> (Data.Text.pack (show f), s)) memos2

    void $ Gtk.init Nothing

    window <- new Gtk.Window []
    on window #destroy Gtk.mainQuit

    column <- new Gtk.TreeViewColumn [ #title Data.GI.Base.:= "id" ]
    render <- new Gtk.CellRendererText [ #ellipsize Data.GI.Base.:=  Pango.EllipsizeModeEnd
                                       , #editable Data.GI.Base.:= False ]
    #packStart column render True
    #addAttribute column render "text" 0

    column2 <- new Gtk.TreeViewColumn [ #title Data.GI.Base.:= "comment" ]
    render <- new Gtk.CellRendererText [ #ellipsize Data.GI.Base.:=  Pango.EllipsizeModeEnd
                                       , #editable Data.GI.Base.:= False ]

    #packStart column2 render True
    #addAttribute column2 render "text" 1

    mlistStore <- Gtk.listStoreNew [gtypeString, gtypeString]

    setValuesToListStore mlistStore demoList 0

    view <- new Gtk.TreeView[#enableTreeLines Data.GI.Base.:= True, #headersVisible Data.GI.Base.:= True] 

    lk <- (Gtk.treeViewSetModel view (Just mlistStore))

    #appendColumn view column
    #appendColumn view column2

    #expandAll view

    #add window view

    #showAll window

    Gtk.main
