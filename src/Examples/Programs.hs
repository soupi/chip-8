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
    ,0x71, 0x04 -- reg(1) <- reg(1) + 4
    ,0xFB, 0x29 -- set letter
    ,0xD0, 0x15 -- draw letter
    ,0xFA, 0x29 -- set letter
    ,0xD5, 0x15 -- draw letter
    ,0xFD, 0x29 -- set letter
    ,0xDA, 0x15 -- draw letter
  --,0x00, 0x00 -- DUMP
    ,0x12, 0x00 -- jump to start
    ]



