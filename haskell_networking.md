# An Haskell trip [part 4] : Networking for askBarrel a REPL client for barrelDB

- [The monad universe](#The-monad-universe)
  - [A big picture](#A-big-picture)
  - [functor](#functor)
    - [technical definition](#technical-definition)
    - [practical example](#practical-example)
  - [applicative](#applicative)
    - [technical definition](#technical-definition-1)
    - [practical example](#practical-example-1)
  - [Monad](#Monad)
    - [technical definition](#technical-definition-2)
  - [practical example](#practical-example-2)
  - [All programs begin with the `IO monad`](#All-programs-begin-with-the-IO-monad)
    - [Huston there is a problem](#Huston-there-is-a-problem)
    - [Interact with the shell](#Interact-with-the-shell)
- [conclusion](#conclusion)

## Querying server

`Haskell` got it's power for networking.

Let's show what we do here.

```
cd network
./tmux_once.sh
```

You should get something like

![animated](network/once.gif)



Let's have a look at the client code