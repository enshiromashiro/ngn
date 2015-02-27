# ngn - novel page generator

For more information, in Japanese, see `doc/doc.pdf`

## Usage
ngn [target-file] [template-file]


## Requirements

* [clozure cl](http://ccl.clozure.com/) or [sbcl](http://www.sbcl.org/)
* [quicklisp](http://beta.quicklisp.org)
* [shelly](http://github.com/fukamachi/shelly)
* [guess](http://github.com/t-sin/guess)


## Installation
Generate a executable and run it, in terminal.

1.get sources in your computer.

     $ cd /to/any/dir
     $ hg cl http://bitbucket.org/subaru45/ngn
    
2.run `shly` to generate a executables in ngn directory.

     $ shly save-app

3.move the executables to a directory in your PATH.

     $mv ngn /any/dir/in/your/path

## Author

* subaru45

## Copyright

Copyright (c) 2013 subaru45

# License

Licensed under the [NYSL](http://www.kmonos.net/nysl/).

