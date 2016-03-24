{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Discogs.Types.MarketPlace.PriceSuggestion where

import           Discogs.Types.MarketPlace.Price

import           Data.Aeson

data PriceSuggestion = PriceSuggestion
    { veryGood     :: Price
    , goodPlus     :: Price
    , nearMint     :: Price
    , good         :: Price
    , veryGoodPlus :: Price
    , mint         :: Price
    , fair         :: Price
    , poor         :: Price
    } deriving (Show, Read, Eq)

instance FromJSON PriceSuggestion where
    parseJSON = withObject "priceSuggestion" $ \o -> do
        veryGood     <- o .: "Very Good (VG)"
        goodPlus     <- o .: "Good Plus (G+)"
        nearMint     <- o .: "Near Mint (NM or M-)"
        good         <- o .: "Good (G)"
        veryGoodPlus <- o .: "Very Good Plus (VG+)"
        mint         <- o .: "Mint (M)"
        fair         <- o .: "Fair (F)"
        poor         <- o .: "Poor (P)"
        return PriceSuggestion{..}

instance ToJSON PriceSuggestion where
    toJSON PriceSuggestion{..} = object [
        "Very Good (VG)"       .= veryGood,
        "Good Plus (G+)"       .= goodPlus,
        "Near Mint (NM or M-)" .= nearMint,
        "Good (G)"             .= good,
        "Very Good Plus (VG+)" .= veryGoodPlus,
        "Mint (M)"             .= mint,
        "Fair (F)"             .= fair,
        "Poor (P)"             .= poor ]
