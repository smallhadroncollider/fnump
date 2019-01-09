# Nump

A command-line tool to batch bump file numbering:

```
02-fish.md -> 03-fish.md
03-cow.md -> 04-cow.md
04-spoon.md -> 05-spoon.md
```

Only currently works with two-digit files.

## Building

```
stack build
```

## Running

```
stack exec nump <value>
```

Where `<value>` is the first two digit number you want to bump from.
