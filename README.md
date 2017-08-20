# ffs [![Linux Build Status](https://travis-ci.org/tcsc/ffs.svg)](https://travis-ci.org/tcsc/ffs) [![Windows Build status](https://ci.appveyor.com/api/projects/status/1vfc1i0rjx2u8lwp?svg=true)](https://ci.appveyor.com/project/tcsc/ffs)

Generate a timesheet from your JIRA work logs.

## Installation
Either:
 - Grab a pre-built binary package from the Releases page (NB: on linux you may need to install libgmp), or
 - Clone the repo and run `stack install` (assuming you already have Haskell 
   infrastructure in-place). Have a look at `.travis.yml` or `appveyor.yml` 
   for build instructions

## Running

```bash
$ ffs --jira https://jira.example.com
```

You will be prompted for your JIRA username & password. Note that you _can_ 
supply both on the command line if you wish, but I recommend against it (for
your password at any rate - you don't want it cropping up in your command 
history if you can help it). 

By default, `ffs` will assume you want to generate a timesheet for the user 
you're logging in as. You can override this by passing a username on the 
command line, or using the config file to specify a new default (see below).

Run `ffs --help` for more info.

## Configuration

The `ffs` config file (linux: `~/.ffs`, windows: `%HOMEDIR%/.ffs`) can supply 
infrequently-changing values so you don't have to specify them every time. An 
example file would look like: 

```ini
[login]
username = your-jira-username

# Uncomment to store your JIRA password in your config file. Not recommended, 
# as it's an enormous security hole
# password = not-recommnded

[JIRA]
host = https://jira.example.com

# uncomment to disable TLS certificate checking
# insecure = yes

[report]
# Set the reporting day for the timesheet. 
week-ends-on = sun
group-by = field: billing-code
roll-up-subtasks = True
```

# The name

The tool is called `ffs` because its very close to what I say whenever I have 
to double- or triple-handle timesheet information for accounting reasons. I 
figured I'd stop just saying it, and automate the process away.

I also like the supergroup with members of Franz Ferdinand and Sparks.

# TODO
 - [X] ~~Reporting over arbitrary date ranges, rather than "the current week"~~
 - [X] ~~Refuse to use unencypted HTTP, as this will send your username and 
       password in essentially clear text. Maybe with a `--force` option for 
       the terminally masochistic.~~
 - [X] ~~CSV output for injection into other tools~~
 - [X] ~~Grouping options, e.g. by custom field, by epic, by label, etc.~~
 - [X] ~~Optionally roll up subtasks into parent~~
 - [ ] Debian packaging for dependency management
 - [X] ~~Display totals on table - per day, per bucket, grand total~~
