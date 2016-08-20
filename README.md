# marktime

Task based time tracking.

## Warning

Currently this program is unfinished. While still usable, as I use it to keep
track of what I plan to do on marktime itself, it will be subject to breaks in
compatibility and drastically changing interfaces.

**I make no promises while it is still version 0.0.0.0**

Not everything is fully implemented and there are a number of placeholders
currently for functionality I haven't gotten around to finishing.

## Installation

I recommend you use `stack` to install anything Haskell related, but cabal
should work fine too. You'll also need `sqlite3` for the database that I use to
store all the information.

If you use git you can get started pretty quickly with this

```bash
# Clone the repository with git
$ git clone https://github.com/taksuyu/marktime

# change the directory to where the downloaded repository is
$ cd marktime

# build the repository with stack
$ stack build --copy-bins
```

Assuming that `~/.local/bin` is on your path you'll be able to use `marktime`
from there.

## Getting started

There are a number of commands that marktime has implemented and here are some
quick examples to get you started.

### add

You can use `marktime add` to create tasks.

```bash
$ marktime add "this is a task with spaces in the description"
1 has been added.
```

### list

We can now take a look at our unfinished tasks using `marktime list`.

```bash
$ marktime list
Tasks (Key: Description)
         1: this is a task with spaces in the description
```

As we add and finish tasks this list will grow larger and smaller.

### start

You can start working on a tasks using the key associated to the task and the command `marktime start`

```bash
$ marktime start 1
Task 1 has been started.
```

#### Warning

Currently there is no way to really interact with the timestamps that start
creates, but later on I'll have it so you can generate time reports, among other
time related goodness.

### stop

When you are done with a task you can stop it with `marktime stop`

```bash
$ marktime stop 1
Task 1 has been stopped.
```

This will mark the task as finished if the task has already been started.

```bash
$ marktime list
Tasks (Key: Description)
```

## Contact

If you are interested in this project feel free to leave some feedback on my
twitter @taksuyu

Issues and feature requests can be submitted through Github
https://github.com/taksuyu/marktime/issues

I also have a freenode channel `#marktime` for the project if you want to
discuss the project or shoot the breeze.
