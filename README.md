# Erlang

## Reading List

### Books

- [ ] [Programming Erlang: Software for a Concurrent World](https://books.google.se/books/about/Programming_Erlang.html)
- [ ] [Learn You Some Erlang for Great Good! A Beginner's Guide](https://learnyousomeerlang.com/content)
- [ ] [The Erlang Runtime System](https://blog.stenmans.org/theBeamBook/)
- [ ] [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action)
- [ ] [Erlang in Anger](http://www.erlang-in-anger.com/)

### Official Docs
- [X] [Programming Rules](http://www.erlang.se/doc/programming_rules.shtml)
- [ ] [Erlang module](http://erlang.org/doc/man/erlang.html)
- [ ] [Erl](http://erlang.org/doc/man/erl.html)
- [ ] [External Term format](https://erlang.org/doc/apps/erts/erl_ext_dist.html)
- [ ] [ERTS PDF](http://erlang.org/doc/apps/erts/erts.pdf)
- [ ] [Rebar3](https://www.rebar3.org/docs/)

### Miscellaneous
- [ ] [Beam Wisdoms](http://beam-wisdoms.clau.se/en/latest/)

### Videos
- [ ] [Learning Erlang - Easier than you think](https://www.youtube.com/watch?v=OCkL9z8IxOI)

## Tooling

- [x] Use Dialyzer plugin for Vim.
- [ ] Find a plugin powerful enough to suggest options for `gen_tcp:listen`.
- [ ] Set up a code formatter.

## Directories

### programming_erlang

Code and exercises from Programming Erlang 2nd edition

### beam_book

Code from https://blog.stenmans.org/theBeamBook/

## Beginner Talk Ideas

- [ ] Dispelling Superstitions with Experiments

> Erlang is a constantly developing language and some common advice may become less relevant. How can we confirm that they still hold? For example, when studying Erlang lists we are told that `List ++ [Element]` is less efficient than `reverse([Element|List])`. Is it still true? By how much is it less efficient? Also, "Make servers tail-recursive or you will run out of memory!" How fast will that happen? Let us build small programs to put these claims to the test and put numbers on the tribal knowledge.
