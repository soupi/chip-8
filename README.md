HIP-8
=====

A purely functional toy emulator for the CHIP-8 system written in Haskell

![](chip8.gif)

### Videos

> - [Playing Pong on Youtube](https://www.youtube.com/watch?v=8gVS-433w8g)
> - [Playing Tetris on Youtube](https://www.youtube.com/watch?v=WzFXH5XzN7A)

### Controls:

The key mapping for the chip-8 is a bit... different. the key mapping is represented in the following code:

```hs
keyMapping :: [(Int, SDL.Scancode)]
keyMapping =
  [(0x1, SDL.Scancode1)
  ,(0x2, SDL.Scancode2)
  ,(0x3, SDL.Scancode3)
  ,(0x4, SDL.ScancodeQ)
  ,(0x5, SDL.ScancodeW)
  ,(0x6, SDL.ScancodeE)
  ,(0x7, SDL.ScancodeA)
  ,(0x8, SDL.ScancodeS)
  ,(0x9, SDL.ScancodeD)
  ,(0x0, SDL.ScancodeX)

  ,(0xa, SDL.ScancodeZ)
  ,(0xb, SDL.ScancodeC)
  ,(0xc, SDL.Scancode4)
  ,(0xd, SDL.ScancodeR)
  ,(0xe, SDL.ScancodeF)
  ,(0xf, SDL.ScancodeV)
  ]
```
