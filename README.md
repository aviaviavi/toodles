# Toodles

[![Build Status](https://travis-ci.org/aviaviavi/toodles.svg?branch=master)](https://travis-ci.org/aviaviavi/toodles)
[![Hackage](https://img.shields.io/hackage/v/toodles.svg)](https://hackage.haskell.org/package/toodles)

Toodles scrapes your entire repository for TODO entries and organizes them so
you can manage your project directly from the code. View, filter, sort, and edit
your TODO's with an easy to use web application. When you make changes via
toodles, the edits will be applied directly the TODO entries in your code.
When you're done, commit and push your changes to share them with your team!

![Toodles Screenshot](https://i.imgur.com/DEwzMYn.png)

### TODO details

Specify details about your TODO's so that you can filter and sort them with
ease! Specify details within parenthasis and separate with the `|` delimeter.

```python
# TODO(assignee|p=1|keys=vals|#tags) 
```

#### Priority

The key `p=<integer>` will be interpreted as a priority number

#### KeyVals

Use arbitrary key value pairs `<key>=<value>|<key2>=<value2>|...` and design any
organization scheme you wish! A good use for this is to enter dates of deadlines
for TODO's that you can sort on in Toodles

#### Tags

A detail starting with `#`, eg `#bug|#techdebt|#database|...` will be interpreted as
a tag, which can be used to label and group your TODO's.

#### Assign

Assign your TODO's to someone. Any plain word that will be interpreted as an assignee.

```python
# TODO(bob) - something we need to do later
```

### Per Project Configuration

You can configure toodles by putting a `.toodles.yaml` file in the root of your
project. See this repo's `.toodles.yaml` for the full configuration spec.

Currently via config you can:

- Set files to ignore via a list of regular expressions

### Scanned Languages

These languages will be scanned for any TODO's:

- C/C++
- C#
- Elixir
- Erlang
- Go
- Haskell
- Java
- Javascript
- Kotlin
- Lua
- Objective-C
- PHP
- Plaintext files (`*.txt`)
- Protobuf
- Python
- Ruby
- Rust
- Scala
- Shell / Bash
- Swift
- Typescript
- Yaml

Submit a PR if you'd like a language to be added. There will eventually be
support for this to be user configurable

### Installing

The easiest way to get toodles is via [stack](https://docs.haskellstack.org).
Just a `stack install toodles` and you're done! Alternatively, with GHC 8.4.3
you can use [cabal](https://www.haskell.org/cabal/download.html). If there is
desire for it I can look into precompiled distribution.

### Running

Invoking `toodles` with no arguments will treat the current directory as the
project root and will start a server on port 9001. You can set these with the
`-d` and `-p` flags, respectively.


```bash
# $ toodles -d <root directory of your project> -p <port to run server>
# for more info run:
# $ toodles --help
$ toodles -d /path/to/your/project -p 9001
# or simply
$ toodles
```

### Current Limitations

Due to the parser's current simplicity, Toodles won't see TODO's in multiline
initiated comment. For instance in javascript

```javascript
// TODO(#bug) this would be parsed

/*

 TODO(#bug) this will _not_ be picked up by toodles

*/
```

