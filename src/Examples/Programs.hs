module Examples.Programs where

import qualified Data.ByteString as BS

gameData1 :: BS.ByteString
gameData1 =
  BS.pack
    [0x00, 0xE0 -- clear screen
    ,0xF1, 0x29 -- set letter
    ,0xD0, 0x05 -- draw letter
    ,0x00, 0x00 -- DUMP
    ,0x12, 0x00 -- jump to start
    ]

gameDataBAD :: BS.ByteString
gameDataBAD =
  BS.pack
    [0x00, 0xE0 -- clear screen
    ,0xFB, 0x29 -- set letter
    ,0xD0, 0x15 -- draw letter
    ,0xFA, 0x29 -- set letter
    ,0xD5, 0x15 -- draw letter
    ,0xFD, 0x29 -- set letter
    ,0xDA, 0x15 -- draw letter
    ,0x71, 0x01 -- reg(1) <- reg(1) + 1
    ,0x12, 0x0E -- jump to previous command
    ]


gameDataKey :: BS.ByteString
gameDataKey =
  BS.pack
    [0x00, 0xE0 -- clear screen
    ,0xF1, 0x29 -- set letter 1
    ,0xD0, 0x15 -- draw letter
    ,0x60, 0x00 -- reg(0) <- 0
    ,0x71, 0x01 -- reg(1) <- reg(1) + 1
    ,0xE0, 0x9E -- skip next instruction if key stored in V0 is pressed
    ,0x12, 0x08 -- jump back twice
    ,0x00, 0xE0 -- clear screen
    ,0xF2, 0x29 -- set letter 2
    ,0xD0, 0x15 -- draw letter
    ,0x60, 0x01 -- reg(0) <- 1
    ,0x61, 0x00 -- reg(1) <- 0
    ,0x71, 0x01 -- reg(1) <- reg(1) + 1
    ,0xE0, 0x9E -- skip next instruction if key stored in V0 is pressed
    ,0x12, 0x18 -- jump back twice
    ,0x12, 0x00 -- jump to start
    ]



