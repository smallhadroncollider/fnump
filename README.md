# fnump

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
stack install
```

## Running

To update all files in the current directory from the prefix 03 and upwards:

```
fnump 03
```

You can also automatically update references to the renamed files in a text file by adding it as the second argument:

```
fnump 03 document.md
```
