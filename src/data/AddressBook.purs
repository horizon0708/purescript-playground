module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address = {
    street :: String,
    city :: String,
    state :: String
}

type Entry = {
    firstName :: String,
    lastName :: String,
    address :: Address
}

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
    entry.firstName <> ": " <>
    showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
    addr.city <> ", " <>
    addr.state 

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons 

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry 
    where 
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryWithStreet :: String -> AddressBook -> Maybe Entry 
findEntryWithStreet street = head <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.address.street == street

nameExists :: String -> AddressBook -> Boolean
nameExists name = null <<< filter filterEntry
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry = entry.firstName == name

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy dupl
    where 
        dupl :: Entry -> Entry -> Boolean
        dupl a b = a.firstName == b.firstName && b.lastName == b.lastName


