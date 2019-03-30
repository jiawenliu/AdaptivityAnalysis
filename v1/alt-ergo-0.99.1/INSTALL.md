## Build and Installation

  You need OCaml >= 3.12.1, zarith and ocamlgraph to compile the
  sources. You need LablGtk2 and the widget GSourceView2 to compile
  the GUI. You may need superuser permissions to perform the
  installation.

#### Common Steps

  1. If a configure file is not distributed with the sources, then
  execute "autoconf"

  2. Configure with "./configure" to generate a Makefile

  3. Alternatively, you can configure with "./configure -prefix
  some-absolute-path-prefix" to add a prefix for installation
  directories You may also want to use "make show-dest-dirs" to see
  directories where things will be installed.

The steps below will build and install native or bytecode binaries
depending on whether ocamlopt is installed or only ocamlc is detected.

#### Alt-Ergo

  1. Compile with "make"

  2. Install with "make install"
 
#### AltGr-Ergo

  1. Compile with "make gui"
  
  2. Install with "make install-gui"
 
#### The Ctrl-Alt-Ergo wrapper
   (not provided with this public release)

The steps below will build and install OCamlPro plugins (extension
.cmxs if ocamlopt is installed or .cma if only ocamlc is detected).

#### The SatML Plugin

  1. Compile with "make satML"

  2. Install with "make install-satML"

#### The Fm-Simplex Plugin
   (not provided with this public release)


#### The Profiler Plugin
   (not provided with this public release)


## Usage

- Alt-Ergo and AltGr-Ergo are executed with the following commands,
  respectively:

        $ alt-ergo   [options] file.why
        $ altgr-ergo [options] file.why

- The SatML plugin can be used as follows: 

        $ alt-ergo -sat-plugin satML-plugin.cmxs [other-options] file.why
        $ alt-ergo -sat-plugin some-path/satML-plugin.cmxs [other-options] file.why

   Alt-Ergo will try to load a local plugin called
   "satML-plugin.cmxs". If this fails, Alt-Ergo tries to load it from
   the default plugins directory (execute "alt-ergo -where plugins" to
   see its absolute path). You can also provide a relative or an
   absolute path as shown by the second command above. Also, you
   should replace ".cmxs" by ".cma" if you are working with bytcode
   binaries.
