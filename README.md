iRuby
=====

**Introducing iRuby**

[iRuby][iruby] is an independent fork of [`inf-ruby`][inf-ruby]

**Features**

* Inspired by [`inf-ruby`][inf-ruby]

* Provides an API using EIEIO and Emacs CL classes, with integrated
  class models, generic functions, and method definitions

    * Ruby implementations (Ruby, jRuby, other)
    * Interactive Ruby frameworks (IRB, Pry, other)
    * Consoles modeled after [`inf-ruby`][inf-ruby] (Bundler, Rails, other)
    * Customization support using EIEIO, e.g `iruby-interactive-impls`

* Tested with IRB, Ruby 3.1, and Bundler

**Known Limitations**

* iRuby has not been published to MELPA

* Has not been well tested for interactive Ruby evaluation with Pry

* Limted testing with jRuby

* Untested with Truffle Ruby, rbenv, rails, rdbg

* Implements some assumptions in Bundler support, e.g that `irb` or
  `pry` is available as a gem for the bundler configuration in a project
  directory

## Using iRuby

**Installation**

iRuby can be installed from source.

**An overview of interactive commands for iRuby**

* `iruby` and `run-iruby` will create an interactive Ruby process in
  Emacs

* When called with an interactive prefix argument, the `iruby` Emacs
  Lisp command will prompt the user to select a Ruby implementation, using
  the customization option `iruby-interactive-impls`

* When called without an interactive prefix argument, `iruby` will
  activate a _console_ if a Gemfile or other supported _project file_ is
  found in the default directory or any containing directory. For
  instance, the bundler console by default will run `irb` under bundler,
  within the detected project directory. (This assumes that irb is a
  dependency for the current bundler configuration in that project
  directory).

  If no project file is found, `iruby` will use the interactive Ruby
  framework selected under the customization option,
  `iruby-default-interactive-ruby`

* `run-iruby` will prompt the user for a shell command to run, for the
  interactive Ruby process

**Support**

The iRuby source code is [available at GitHub][iruby]

[inf-ruby]: http://github.com/nonsequitur/inf-ruby/
[iruby]: https://github.com/rubyblox/iruby
