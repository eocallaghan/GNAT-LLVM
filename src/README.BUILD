This is README.BUILD from the source distribution of GNAT. It contains
directions for building GNAT from the sources.

--------------------------------------
-- BUILDING GNAT - GENERAL COMMENTS --
--------------------------------------

GNAT being mainly written in Ada, you need GNAT to compile it. So, the
procedures described here presume that you have GCC and the current or the
previous version of GNAT installed (we only try to make it possible to compile
one version of the compiler with the previous one -- not any older versions)
-- i.e. the "gcc" command will run GNAT.  The steps here will build and
install new C and Ada compilers.  If you thoroughly understand the process of
building gcc you may find shortcuts that apply to your environment -- the
possibilities are too numerous to spell out here.

The current version of GNAT can be built on top of GCC. The GNAT sources
reside in a subdirectory named "ada" under the GCC compiler source directory,
the latter being the "gcc" subdirectory of the GCC source distribution.

The directory named "src" in this GNAT distribution contains the "ada"
subdirectory, as well as a set of patches that you may need to apply to the
GCC sources, depending on your target platform. One of these patches is
"gcc-VV.dif" where VV is a GCC version (e.g. 41) and should be applied
whatever the target.

See the example build sequence below for more details on a simple build
sequence. The GCC documentation gives more details for specific instructions
related to a particular build configuration.

You can also use "gcc -v" from the GNAT binaries to check which exact
configure options have been used on a given environment.

--------------------------------------
-- BUILDING GNAT - EXAMPLE SEQUENCE --
--------------------------------------

Supposing you have one tarball for the GCC sources and one for the GNAT
sources, here is a possible way to proceed:

0/ If not available on your system, build libgmp and libmpfr following
   build instructions from these packages.

1/ Create a root directory into which you'll later setup the source and
   build subdirectories:

   ~ $ mkdir GNAT
   ~ $ cd GNAT

2/ Setup an initial source tree from the GCC and the GNAT tarballs:

  ~/GNAT $ gzip -dc [...]/gcc-4.3*.tgz | tar xf -
  ~/GNAT $ mv gcc-4* src
  ~/GNAT $ cd src

  ~/GNAT/src $ gzip -dc [...]/gnat-xxx-src.tgz  | tar xf -
  ~/GNAT/src $ mv gnat-xxx-src/src/ada gcc
  ~/GNAT/src $ patch -p0 < gnat-xxx-src/src/gcc-43.dif 
  ~/GNAT/src $ touch gcc/cstamp-h.in 

3/ Prepare and configure the build tree:

  ~/GNAT/src $ mkdir ../obj; cd ../obj
  ~/GNAT/obj $ ../src/configure --enable-languages="c,ada" --disable-libada ...
  [see the GCC documentation for other relevant configure options]

4/ Build the compiler, run time and tools:

  Note that the use of "GNU Make" is required by the GCC Makefiles.

  ~/GNAT/obj $ make bootstrap
  ~/GNAT/obj $ make -C gcc gnatlib gnattools

  If you are building a cross compiler, you will need to replace
  the above sequence by something similar to:

  ~/GNAT/obj $ make
  ~/GNAT/obj $ make -C gcc gnatlib cross-gnattools ada.all.cross

5/ Install the whole package:

  ~/GNAT/obj $ make install

--------------------------------------------------------
-- Building GNAT - SHARED/MULTIPLE RUN-TIME LIBRARIES --
--------------------------------------------------------

On some targets, a shared GNAT library can also be built by replacing
"gnatlib" by "gnatlib-shared" in the sequence above. Check the GNAT binary
distribution to see whether shared libraries is supported on your system.

Additionally on some targets, it is possible to build a GNAT run time
targetting a different thread library. To do that, check the file
src/gcc/ada/Makefile.in to see what THREAD_KIND are supported on your system,
and specify "THREAD_KIND=<kind>" as part of the gnatlib build, e.g:

    ~/GNAT/obj $ make -C gcc THREAD_KIND=pthread gnatlib gnattools

