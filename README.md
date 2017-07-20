# ffs [![Linux Build Status](https://travis-ci.org/tcsc/ffs.svg)](https://travis-ci.org/tcsc/ffs) [![Windows Build status](https://ci.appveyor.com/api/projects/status/1vfc1i0rjx2u8lwp?svg=true)](https://ci.appveyor.com/project/tcsc/ffs)

Generate a timesheet from your JIRA work logs.

## Installation
Either:
 - Grab a pre-built binary package from the Releases page, or
 - Clone the repo and run `stack install` (assuming you already have Haskell 
   infrastructure in-place). Have a look at `.travis.yml` or `appveyor.yml` 
   for build instructions

## Running

```bash
$ ffs --login $jira_username --jira https://jira.example.com
```

You will be prompted for your JIRA password. Note that you _can_ pass it on the 
command line, but it's a better idea not to as you probably don't want it showing
up in your command history. 

Run `ffs --help` for more info.

## Configuration

The `ffs` config file (linux: `~/.ffs`, windows: `%HOMEDIR%/.ffs`) can supply 
infrequently-changing values so you don't have to specify them every time. An 
example file would look like: 

```ini
[login]
username = "your-jira-username"

# Uncomment to store your JIRA password in your config file. Not recommended, 
# as it's an enormous security hole
# password = "not-recommnded"

[JIRA]
host = https://jira.example.com

# uncomment to disable TLS certificate checking
# insecure = yes

[report]
# Set the reporting day for the timesheet. 
week-ends-on = sun
```

# The name

The tool is called `ffs` because its very close to what I say whenever I have 
to double- or triple-handle timesheet information for accounting reasons. I 
figured I'd stop just saying it, and automate the process away.

# TODO
 - [ ] Reporting over arbitrary date ranges, rather than "the current week"
 - [ ] Refuse to use unencypted HTTP, as this will send your username and password 
       in essentially clear text. Maybe with a `--force` option for the terminally 
       masochistic.
 - [ ] CSV output for injection into other tools
 - [ ] Grouping options, e.g. by custom field, by epic, by label, etc.
 - [ ] Optionally roll up subtasks into parent
 - [ ] Debian packaging for dependency management
