# hblock

Block puzzle in Haskell

![demo image](https://github.com/seroron/hblock/wiki/img/hblock_demo.gif)

## Build & Execute

To Build, use [Stack](https://docs.haskellstack.org)

### Ubuntu

```
apt install libsdl1.2-dev
stack build

stack exec hblock
```

### Windows

```
stack exec -- pacman -Syu
stack exec -- pacman -S mingw64/mingw-w64-x86_64-gcc mingw64/mingw-w64-x86_64-SDL
stack build

stack exec hblock
```

## Licence

MIT

## Author

[Kenichi HIROSE](https://github.com/seroron)
