# Toodles

[![Build Status](https://travis-ci.org/aviaviavi/toodles.svg?branch=master)](https://travis-ci.org/aviaviavi/toodles)
[![Hackage](https://img.shields.io/hackage/v/toodles.svg)](https://hackage.haskell.org/package/toodles)

Toodles scrapes your entire repository for TODO entries and organizes them so
you can manage your project directly from the code. View, filter, sort, and then
edit your TODO's with an easy to use web application. When you're done, commit
and push your changes and share changes with your team!

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

Submit a PR if you'd like a language to be added. There will eventually be
support for this to be user configurable

- C/C++
- Elixir
- Erlang
- Go
- Haskell
- Java
- Javascript
- Objective-C
- Protobuf
- Python
- Ruby
- Rust
- Scala
- Shell / Bash
- Swift
- Typescript
- Yaml


### Current Limitations

Due to the parser's current simplicity, Toodles won't see TODO's in multiline initiated comment. For instance in javascript

```javascript
// TODO(#bug) this would be parsed

/*

 TODO(#bug) this will _not_ be picked up by toodles

*/
```


