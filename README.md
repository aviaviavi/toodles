# Toodles

Toodles scrapes your entire repository for TODO entries and organizes them so
you can manage your project directly from the code. View, filter, sort, and then
edit your TODO's with an easy to use web application. When you're done, commit
and push your changes and share changes with your team!

### TODO details

Specify details about your TODO's so that you can filter and sort them with
ease! Separate details within parenthasis and separate with the `|` delimeter.

```python
# TODO(assignee|p=1|keys=vals|#tags) 
```

#### Priority

The key `p=<integer>` will be interpreted as a priority number

#### KeyVals

Use arbitrary key value pairs `<key>=<value>|<key2>=<value2>|...` and design any
organization scheme you wish!

#### Tags

A detail starting with `#`, eg `#bug|#techdebt|#database|...` will be interpreted as
a tag, which can be used to label and group your TODO's.

#### Assign

Assign your TODO's to someone. Any plain word that will be interpreted as an assignee.

```python
# TODO(bob) - something we need to do later
```

### Current Limitations

Due to the parser's current simplicity, Toodles won't see TODO's in multiline initiated comment. For instance in javascript

```javascript
// TODO this would be parsed

/*

 TODO(#bug|open_to=anyone) this will _not_ be picked up by toodles

*/
```

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
