{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Euterpea

-- Source sound

sineTable :: Table
sineTable = tableSinesN 512 [1]

-- Apply a window to the source sound

grain :: Instr (AudSF () Double)
grain _ ap vol pfields =
  let f = apToHz ap
  in  proc () -> do
        source <- osc sineTable 0 -< f
        outA -< 0.95 * (source)

-- Constructing the InstrMap:

grainName :: InstrumentName
grainName = CustomInstrument "Grain"

grainMap :: InstrMap (AudSF () Double)
grainMap = [(grainName, grain)]

-- Single grain:

main :: IO ()
main =
    writeWav "sound-output/note.wav" grainMap
    $ tempo 0.5
    $ instrument grainName (e 4 0.1)
