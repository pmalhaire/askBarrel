# An Haskell trip [part 1]: high level design for askBarrel a REPL client for barrelDB

- [Introduction](#introduction)
  - [What is askBarrel ?](#what-is-askbarrel-)
- [A basic example](#a-basic-example)
- [Where do I begin with ?](#where-do-i-begin-with-)
- [Cut it into pieces](#cut-it-into-pieces)
  - [Mandatory](#mandatory)
  - [Must have](#must-have)
- [Conclusion](#conclusion)

## Introduction

Disclamer : this part is not about haskell, it's the high level design of the project, if you want code right now go to the part 2.

A year ago I created a POC for a repl client for the interesting barrelDB. It was a pet project, and gave me the opportunity to explore Haskell basics.

A year after I made a kind of `ok but not great talk` about functional programming in `C++`. I learned a lot doing it seems like my interest for functional programing stayed.

I met @benoic the author of barrelDB which encourage me to make this POC a real thing. We discussed and I identified subjects to be addressed.

### What is askBarrel ?

`askBarrel` is a barrelDB command line client.

A comparable project would be `psql` for `postgres`.

It's goal is to be able to get and put documents to `barrelDB` and to get information on the `barrelDB` nodes.

## A basic example

Let's make things more concrete. Let's create a db.

The `Repl engine` feeds the `Command line parser` with input strings.

```bash
barrelDB >>> create_db { "name" : "my_db" }
```

The `Command line parser` cuts the command in parts to feed the `Query parser`.

| command   | data                 |
| --------- | -------------------- |
| create_db | { "name" : "my_db" } |

The `Query parser` check the validity of the data given to the command and gives a buffer to the `Transport library`.

| command   | check                 | status   |
| --------- | --------------------- | -------- |
| create_db | has a name            | &#10003; |
| create_db | has name is not empty | &#10003; |


The `Transport library` sends the query buffer to the `DB`.

```bash
┌────────┬─────────────────────────┬─────────────────────────┬────────┬────────┐
│00000000│ 7b 20 6e 61 6d 65 20 3a ┊ 20 6d 79 5f 64 62 20 7d │{ name :┊ my_db }│
│00000010│ 0a                      ┊                         │_       ┊        │
└────────┴─────────────────────────┴─────────────────────────┴────────┴────────┘
```

The `Transport library` receives the response buffer from the `DB` and the it to the `Command line parser`.

```bash
┌────────┬─────────────────────────┬─────────────────────────┬────────┬────────┐
│00000000│ 7b 20 72 65 73 75 6c 74 ┊ 20 3a 20 6f 6b 20 7d 0a │{ result┊ : ok }_│
└────────┴─────────────────────────┴─────────────────────────┴────────┴────────┘
```

The `Command line parser` get the gives the response to the `Repl engine`.

| message  | data                  |
| -------- | --------------------- |
| response | `{ "result" : "ok" }` |

The `Repl engine` write the response to the shell

```bash
{ "result" : "ok" }
```

![askBarrel](askBarrel.png)

## Where do I begin with ?

This project has several part, to be able to do it I need to know where do I begin.

<!-- my brain (small) my problem (big) -->

Let's write the steps from our previous example.

1. get query `q` from the user
2. connect to the DB
3. send `q` to the DB server
4. get response `r` from the DB server
5. write `r` to the shell
6. close the program


After an intense usage of an agile disruptive methodology - I faced to a terrible conclusion :
  If I can't talk to the `DB server` this project is dead.

<!-- muted crossed scotch head in a form of a DB -->

Step 3 and 4 are the one to begin with so the first part I'll work on will be the ... `Transport library`.

## Cut it into pieces

Now that we know where we will work first. It's good to have an idea of the pieces that come next.
This list won't be followed as it is. Software development is like that, what is planned is not what will happen.
Meanwhile it's dangerous to write code without a clear plan. Let's write down the pieces we want, an class prioritize it in `mandatory` and `must have`.

### Mandatory

```md
1. Repl engine                   : interact with the shell
2. Command line parser           : parse written sentences to commands
3. Query parser                  : parse DB queries
4. Transport library             : sends and receive data from the DB server
```

### Must have

```md
1. Document reader               : read and validate input documents before sending them
2. Concurrency                   : allow several commands to run at the same time
3. Stream processing             : handle the case of stream
4. Post processing operations    : give the ability to do operations on the received documents
```

## Conclusion

In this first post we get to know the basic design of `askBarrel` a command line `barrelDB` client. For most developer (that mean at least me) it's rather frustrating not to see a line of code but getting the lights of the high level design helps not to fall in the shadow of bad code.

[Part 2](haskell_syntax.md) will be an introduction on `haskell` focussed on the concepts needed to do the first part of the project :
the `Transport library`.