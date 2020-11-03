# Erlang

## Reading List

### Books

- [X] [Programming Erlang: Software for a Concurrent World](https://books.google.se/books/about/Programming_Erlang.html)
- [ ] [Learn You Some Erlang for Great Good! A Beginner's Guide](https://learnyousomeerlang.com/content)
- [ ] [The Erlang Runtime System aka The Beam Book](https://blog.stenmans.org/theBeamBook/)
- [ ] [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action)
- [ ] [Adopting Erlang](https://adoptingerlang.org)
- [ ] [Erlang Programming](https://www.oreilly.com/library/view/erlang-programming/9780596803940/) 2009, outdated?
- [ ] [Erlang in Anger](http://www.erlang-in-anger.com/)
- [ ] [Études for Erlang](https://www.oreilly.com/library/view/etudes-for-erlang/9781491917657/)

### Official Docs

- [X] [Programming Rules](http://www.erlang.se/doc/programming_rules.shtml)
- [ ] [Erlang module](http://erlang.org/doc/man/erlang.html)
- [X] [Erl](http://erlang.org/doc/man/erl.html)
- [ ] [gen_server](http://erlang.org/doc/man/gen_server.html) + source
- [ ] [design_principles.pdf](http://www.erlang.org/doc/pdf/design_principles.pdf)
- [ ] [OTP Version Histories](https://www.erlang.org/downloads/)
- [ ] [Mnesia](http://erlang.org/doc/man/mnesia.html)
- [ ] [QLC](http://erlang.org/doc/man/qlc.html)
- [ ] [Debugger](http://www.erlang.org/doc/apps/debugger/debugger.pdf)
- [ ] [External Term format](https://erlang.org/doc/apps/erts/erl_ext_dist.html)

### Miscellaneous

- [ ] [Beam Wisdoms](http://beam-wisdoms.clau.se/en/latest/)
- [ ] [Checklist Manifesto](https://www.amazon.com/Checklist-Manifesto-How-Things-Right/dp/0312430000)
- [ ] https://medium.com/@jlouis666

## Tooling

- [ ] https://www.youtube.com/watch?v=8FibGzqygo0
- [x] Use Dialyzer plugin for Vim.
- [x] Find a plugin powerful enough to suggest options for `gen_tcp:listen`.
- [ ] Set up a configurable code formatter `rebar3 format`?
- [ ] [Rebar3](https://www.rebar3.org/docs/)

* [Glass](https://github.com/klarna-incubator/glass)

## Debugging

* https://robertoaloi.github.io/erlang/profiling-erlang-applications-using-redbug
* https://blog.stenmans.org/theBeamBook/#_redbug

## Deeper Theory

* [OTP Blog](http://blog.erlang.org/)

## Directories

### programming_erlang

Code and exercises from Programming Erlang 2nd edition.

### beam_book

Code from The Beam Book.

## Beginner Talk Ideas

- [ ] Dispelling Superstitions with Experiments

> Erlang is a constantly developing language and some common advice may become less relevant. How can we confirm that they still hold? For example, when studying Erlang lists we are told that `List ++ [Element]` is less efficient than `reverse([Element|List])`. Is it still true? By how much is it less efficient? Also, "Make servers tail-recursive or you will run out of memory!" How fast will that happen? Let us build small programs to put these claims to the test and put numbers on the tribal knowledge.

* http://erlang.org/doc/efficiency_guide/retired_myths.html

- [ ] Making Sense of Erlang Tooling

> I have always been fond of tools surrounding development. Here I want to give a survey of current tools for Erlang, their dependencies, strong and weaker sides.

* https://tech.nextroll.com/blog/dev/2020/02/25/erlang-rebar3-format.html
* https://notamonadtutorial.com/erlang-tooling-in-2020-b9606596353a

- [ ] `+sFlag Value` Scheduling specific flags
Erl has quite a few BindTypes for scheduling. Maybe it's worth exploring and visualizing.
