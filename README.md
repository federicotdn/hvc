# hvc
A basic version control system written in Haskell (learning project).

## Installation
In order to build `hvc`, [`stack`](http://docs.haskellstack.org/en/stable/README.html) must be installed.  Once that is done, clone and build:
```bash
$ git clone git@github.com:federicotdn/hvc.git
$ cd hvc
$ stack setup
$ stack build
```
After building, use `stack exec hvc` to run, or copy the generated binary to another directory.

## Usage
Assuming that the `hvc` binary is `./hvc`:
```bash
$ ./hvc <your project directory> <command> [arguments]
```
Valid commands are:
- `init`: initializes the project directory, which is needed to be able to run other `hvc` commands on it.
- `commit`: stores all of the project's files in a commit, identified by a unique hash and a message, which must be passed as an additional argument.
- `checkout`: restores the project directory to the state it was on a given commit.  A valid commit hash must be specified as an additional argument.
- `log`: lists all commits in chronological order, showing hash, date and message for each one of them.
- `status`: prints the current commit hash, and lists any new, deleted or modified files.
- `help`: displays help on how to operate `hvc`.