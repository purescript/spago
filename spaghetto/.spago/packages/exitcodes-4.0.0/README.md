# purescript-exitcodes

[![Latest release](http://img.shields.io/bower/v/purescript-exitcodes.svg)](https://github.com/Risto-Stevcev/purescript-exitcodes/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-exitcodes/badge)](https://pursuit.purescript.org/packages/purescript-exitcodes)

## Usage

This library is useful to aid in the readability of exit codes for APIs that use them. You can restrict your API to only use these codes, or if you are using an API that takes an `Int` for an exit code, you can make the code more explicit by deriving it via `fromEnum`:

```purescript
> fromEnum Error
1

> fromEnum SIGKILL
137
```


## References

The exit codes follow the Bash and BSD standards. Here are some excerpts for reference (see the full text for complete details).

### Advanced Bash Scripting Guide excerpt

| Exit Code Number | Meaning                                                    | Example                 | Comments                                                                                                     |
|------------------|------------------------------------------------------------|-------------------------|--------------------------------------------------------------------------------------------------------------|
| 1                | Catchall for general errors                                | let "var1 = 1/0"        | Miscellaneous errors, such as "divide by zero" and other impermissible operations                            |
| 2                | Misuse of shell builtins (according to Bash documentation) | empty_function() {}     | Missing keyword or command, or permission problem (and diff return code on a failed binary file comparison). |
| 126              | Command invoked cannot execute                             | /dev/null               | Permission problem or command is not an executable                                                           |
| 127              | "command not found"                                        | illegal_command         | Possible problem with $PATH or a typo                                                                        |
| 128              | Invalid argument to exit                                   | exit 3.14159            | exit takes only integer args in the range 0 - 255 (see first footnote)                                       |
| 128+n            | Fatal error signal "n"                                     | kill -9 $PPID of script | $? returns 137 (128 + 9)                                                                                     |
| 130              | Script terminated by Control-C                             | Ctl-C                   | Control-C is fatal error signal 2, (130 = 128 + 2, see above)                                                |
| 255*             | Exit status out of range                                   | exit -1                 | exit takes only integer args in the range 0 - 255                                                            |   

### Signal manpage excerpt

| Signal   |  Value  | Action |  Comment                                                                |
|----------|---------|--------|-------------------------------------------------------------------------|
|SIGHUP    |   1     |  Term  | Hangup detected on controlling terminal or death of controlling process |
|SIGINT    |   2     |  Term  | Interrupt from keyboard                                                 |
|SIGQUIT   |   3     |  Core  | Quit from keyboard                                                      |
|SIGILL    |   4     |  Core  | Illegal Instruction                                                     |
|SIGABRT   |   6     |  Core  | Abort signal from abort(3)                                              |
|SIGFPE    |   8     |  Core  | Floating point exception                                                |
|SIGKILL   |   9     |  Term  | Kill signal                                                             |
|SIGSEGV   |  11     |  Core  | Invalid memory reference                                                |
|SIGPIPE   |  13     |  Term  | Broken pipe: write to pipe with no readers                              |
|SIGALRM   |  14     |  Term  | Timer signal from alarm(2)                                              |
|SIGTERM   |  15     |  Term  | Termination signal                                                      |

### Sysexits.h excerpt

```c
#define EX_OK		0	/* successful termination */

#define EX__BASE	64	/* base value for error messages */

#define EX_USAGE	64	/* command line usage error */
#define EX_DATAERR	65	/* data format error */
#define EX_NOINPUT	66	/* cannot open input */
#define EX_NOUSER	67	/* addressee unknown */
#define EX_NOHOST	68	/* host name unknown */
#define EX_UNAVAILABLE	69	/* service unavailable */
#define EX_SOFTWARE	70	/* internal software error */
#define EX_OSERR	71	/* system error (e.g., can't fork) */
#define EX_OSFILE	72	/* critical OS file missing */
#define EX_CANTCREAT	73	/* can't create (user) output file */
#define EX_IOERR	74	/* input/output error */
#define EX_TEMPFAIL	75	/* temp failure; user is invited to retry */
#define EX_PROTOCOL	76	/* remote error in protocol */
#define EX_NOPERM	77	/* permission denied */
#define EX_CONFIG	78	/* configuration error */
```
