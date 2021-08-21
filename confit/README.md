# Setup
1. Install racket.
	- On Ubuntu, `apt install racket`
	- On Arch, install the official `racket` package using your package manager of choice
2. Run `raco setup`
	- You may need to execute `raco` with `sudo` or Administrator privileges
3. Run `raco pkg install ./confit`
	- This will automatically install any dependencies that are necessary
	- If you've already installed the package but need to update the install, instead run `raco pkg update ./confit`

## Using
Our language utilizes racket's built-in support for parsing and evaluating languages using racket's VM.
Because of this, there are 3 central ways to use our language (after installing the package).

The first option is to use our language with Racket's REPL.
This can be done by executing `racket -I confit`, which will enter a REPL using `confit`'s syntax.

The second option is to make a `.rkt` file of your choice, specifying `#lang confit` at the top of the file.
The rest of the file can be written using `confit`'s syntax.

The third option is to make a `.rkt` file of your choice, but having `confit` syntax apply to modules of your choice.
This will only apply the `confit` syntax and evaluator to the module(s) that specifies `confit` like so:
```racket
#lang racket
(module my-module confit
	...)
```
