# Nump

A command-line tool to batch bump file numbering:

```
02-fish.md -> 03-fish.md
03-cow.md -> 04-cow.md
04-spoon.md -> 05-spoon.md
```

Only works with two-digit files currently.

## Building

```
ghc -O3 Nump.hs
```
