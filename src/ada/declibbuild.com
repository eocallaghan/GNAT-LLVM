$!set verify
$ on error then exit
$! This script builds the GNAT DEC Ada compatibility library.
$! It should be run with "DECLIB" as the current directory.
$!
$! Command line arguments:
$! P1: directory containing GNAT replacement DECLIB units (required)
$! P2: directory containing DEC Ada Predefined library sources (optional)
$!     defaults to sys$common:[syslib.adalib]
$! P3: directory containing DECLIB location (optional)
$!     defaults to gnu:[lib.gcc-lib.declib]
$! P4: directory containing replacement Motif bindings (optional)
$!     defaults to P2.
$!
$! The following DEC Ada units are replaced by equivalent units found in the
$! Adainclude directory.
$!
$!  ada*.adc (DEC Ada 3.4 and 3.5 only)
$!  calendar_.adc
$!  calendar.adc
$!  io_exceptions_.adc
$!  system_.adc
$!  unchecked_conversion_.adc
$!  unchecked_deallocation_.adc
$!
$! The following DEC Ada units are not used because their functionality has
$! been replaced by generic instantiations.
$!
$!  generic_complex_types.adc
$!  generic_elementary_functions.adc
$!
$! The following units are copyrighted GNAT packages used only for the VMS
$! port. They replace or are needed in addition to equivalent DEC packages.
$!
$!  address_operations.adb
$!  current_exception.ads
$!  dec.ads
$!  dec.adb
$!  generic_fast_elementary_functions.adb
$!  get_task_info.adb
$!  get_task_info.ads
$!  math_lib.adb
$!  synchronize_nonreentrant_access.adb
$!  system_runtime_tuning.adb
$!
$! The following units are not applicable.
$!
$!  generic_primitive_functions.adb
$!  generic_primitive_functions.ads
$!  genericprimitifunctiodigitalsupportpkg1.adb
$!  genericprimitifunctiodigitalsupportpkg1.ads
$!  genericprimitifunctiodigitalsupportpkg2.adb
$!  genericprimitifunctiodigitalsupportpkg2.ads
$!
$! The following units are trivial GNAT packages used only for the VMS
$! port. They replace or are needed in addition to equivalent DEC packages.
$! Basically they are just renamings or instantiations of units from the
$! following section.  The renamings are done for compatibility reasons.
$!
$!  complex_fast_elementary_functions.ads
$!  directio.ads
$!  direct_mixed_io.ads
$!  generic_complex_types.ads
$!  generic_elementary_functions.ads
$!  indexed_io.ads
$!  indexed_mixed_io.ads
$!  long_complex_fast_elementary_functions.ads
$!  relative_io.ads
$!  relative_mixed_io.ads
$!  sequenio.ads
$!  sequential_mixed_io.ads
$!  text_io.ads
$!
$! The rest are chopped and created from DEC packages of the same name.
$! Some have been patched to remove Ada95 incompatibilities. See comments and 
$! code below for details and also the declibgnat.diff file.
$! The following units are renamed from DEC packages w/o the dec- prefix.
$!
$!  dec-direct_io.ads
$!  dec-direct_io.adb
$!  dec-direct_mixed_io.ads
$!  dec-direct_mixed_io.adb
$!  dec-indexed_io.ads
$!  dec-indexed_io.adb
$!  dec-indexed_mixed_io.ads
$!  dec-indexed_mixed_io.adb
$!  dec-relative_io.ads
$!  dec-relative_io.adb
$!  dec-relative_mixed_io.ads
$!  dec-relative_mixed_io.adb
$!  dec-sequential_io.ads
$!  dec-sequential_io.adb
$!  dec-sequential_mixed_io.ads
$!  dec-sequential_mixed_io.adb
$!  dec-text_io.ads
$!  dec-text_io.adb
$!
$ IF P1.EQS.""
$ THEN
$   WRITE SYS$OUTPUT "Required parameter specifying Ada source dir missing"
$   EXIT 0
$ ENDIF
$!
$ define SRCDIR 'P1
$!
$ DECADA="SYS$COMMON:[SYSLIB.ADALIB]"
$ IF P2.NES."" THEN DECADA=P2
$ COPY 'DECADA'*.ADC []
$!
$ TYPE /OUT=declib.fdl SYS$INPUT
RECORD
	BLOCK_SPAN              yes
	CARRIAGE_CONTROL        carriage_return
	FORMAT                  stream_lf
$!
$ TYPE /OUT=x_lib.opt SYS$INPUT
sys$library:decw$xlibshr.exe/share
$ CONVERT/FDL=declib.fdl x_lib.opt x_lib.opt
$!
$ TYPE /OUT=xt.opt SYS$INPUT
sys$library:decw$xtshr.exe/share
$ CONVERT/FDL=declib.fdl xt.opt xt.opt
$!
$ TYPE /OUT=xm.opt SYS$INPUT
sys$library:decw$xmlibshr.exe/share
$ CONVERT/FDL=declib.fdl xm.opt xm.opt
$!
$ DELETE declib.fdl.*
$!
$ IF P3.NES.""
$ THEN
$   DECLIBLOC=P3
$ ELSE
$   DECLIBLOC="GNU:[LIB.GCC-LIB.DECLIB]"
$ ENDIF
$ IF P4.NES.""
$ THEN
$   COPY 'P4:MOTIFBINDINGX5M12*.ADA []
$   OPEN/WRITE OPTOUT xtr5.opt
$   WRITE OPTOUT "sys$library:decw$xtlibshrr5.exe/share"
$   CLOSE OPTOUT
$   OPEN/WRITE OPTOUT xmr5.opt
$   WRITE OPTOUT "sys$library:decw$xmlibshr12.exe/share"
$   WRITE OPTOUT "sys$library:decw$mrmlibshr12.exe/share"
$   WRITE OPTOUT "sys$library:decw$dxmlibshr12.exe/share"
$   CLOSE OPTOUT
$ ENDIF
$ 
$!
$ IF F$SEARCH ("$STANDARD_.ADC") .NES. "" THEN -
  DELETE $STANDARD_.ADC.*
$ IF F$SEARCH ("ADA_*.ADC") .NES. "" THEN -
  DELETE ADA_*.ADC.*
$ IF F$SEARCH ("ADDRESS_OPERATIONS.ADC") .NES. "" THEN -
  DELETE ADDRESS_OPERATIONS.ADC.*
$ IF F$SEARCH ("CALENDAR.ADC") .NES. "" THEN -
  DELETE CALENDAR.ADC.*
$ IF F$SEARCH ("CALENDAR_*.ADC") .NES. "" THEN -
  DELETE CALENDAR_*.ADC.*
$ IF F$SEARCH ("GET_TASK_INFO.ADC") .NES. "" THEN -
  DELETE GET_TASK_INFO.ADC.*
$ IF F$SEARCH ("GET_TASK_INFO_.ADC") .NES. "" THEN -
  DELETE GET_TASK_INFO_.ADC.*
$ IF F$SEARCH ("COMPLEX_FAST_ELEMENTARY_FUNCTIONS.ADC") .NES. "" THEN -
  DELETE COMPLEX_FAST_ELEMENTARY_FUNCTIONS.ADC.*
$ IF F$SEARCH ("CURRENT_EXCEPTION_.ADC") .NES. "" THEN -
  DELETE CURRENT_EXCEPTION_.ADC.*
$ IF F$SEARCH ("GENERI-JE*.ADC") .NES. "" THEN -
  DELETE GENERI-JE*.ADC.*
$ IF F$SEARCH ("GENERI-LE*.ADC") .NES. "" THEN -
  DELETE GENERI-LE*.ADC.*
$ IF F$SEARCH ("GENERI-RC*.ADC") .NES. "" THEN -
  DELETE GENERI-RC*.ADC.*
$ IF F$SEARCH ("GENERI-RD*.ADC") .NES. "" THEN -
  DELETE GENERI-RD*.ADC.*
$ IF F$SEARCH ("GENERI-UD*.ADC") .NES. "" THEN -
  DELETE GENERI-UD*.ADC.*
$ IF F$SEARCH ("GENERI-WD*.ADC") .NES. "" THEN -
  DELETE GENERI-WD*.ADC.*
$ IF F$SEARCH ("GENERIC_COMPLEX_TYPES.ADC") .NES. "" THEN -
  DELETE GENERIC_COMPLEX_TYPES.ADC.*
$ IF F$SEARCH ("GENERIC_COMPLEX_TYPES_.ADC") .NES. "" THEN -
  DELETE GENERIC_COMPLEX_TYPES_.ADC.*
$ IF F$SEARCH ("GENERIC_ELEMENTARY_FUNCTIONS.ADC") .NES. "" THEN -
  DELETE GENERIC_ELEMENTARY_FUNCTIONS.ADC.*
$ IF F$SEARCH ("GENERIC_ELEMENTARY_FUNCTIONS_.ADC") .NES. "" THEN -
  DELETE GENERIC_ELEMENTARY_FUNCTIONS_.ADC.*
$ IF F$SEARCH ("GENERIC_FAST_ELEMENTARY_FUNCTIONS.ADC") .NES. "" THEN -
  DELETE GENERIC_FAST_ELEMENTARY_FUNCTIONS.ADC.*
$ IF F$SEARCH ("GENERIC_PRIMITIVE_FUNCTIONS.ADC") .NES. "" THEN -
  DELETE GENERIC_PRIMITIVE_FUNCTIONS.ADC.*
$ IF F$SEARCH ("IO_EXCEPTIONS_.ADC") .NES. "" THEN -
  DELETE IO_EXCEPTIONS_.ADC.*
$ IF F$SEARCH ("LONG_COMPLEX_FAST_ELEMENTARY_FUNCTIONS.ADC") .NES. "" THEN -
  DELETE LONG_COMPLEX_FAST_ELEMENTARY_FUNCTIONS.ADC.*
$ IF F$SEARCH ("MATH_LIB.ADC") .NES. "" THEN -
  DELETE MATH_LIB.ADC.*
$ IF F$SEARCH ("SYNCHRONIZE_NONREENTRANT_ACCESS.ADC") .NES. "" THEN -
  DELETE SYNCHRONIZE_NONREENTRANT_ACCESS.ADC.*
$ IF F$SEARCH ("SYSTEM_.ADC") .NES. "" THEN -
  DELETE SYSTEM_.ADC.*
$ IF F$SEARCH ("SYSTEM_RUNTIME_TUNING.ADC") .NES. "" THEN -
  DELETE SYSTEM_RUNTIME_TUNING.ADC.*
$ IF F$SEARCH ("UNCHECKED_CONVERSION_.ADC") .NES. "" THEN -
  DELETE UNCHECKED_CONVERSION_.ADC.*
$ IF F$SEARCH ("UNCHECKED_DEALLOCATION_.ADC") .NES. "" THEN -
  DELETE UNCHECKED_DEALLOCATION_.ADC.*
$!
$ if f$trnlnm("SRCDIR").eqs."VMI$KWD:" then -
$   sed="$SRCDIR:sed.exe"
$   timestamp="$SRCDIR:timestamp.exe"
$!
$  IF F$SEARCH ("DECLIB.SED") .NES. "" THEN DELETE DECLIB.SED.*
$  OPEN/WRITE SEDOUT DECLIB.SED
$  WRITE SEDOUT "s/""ADA\$\([A-Z$]\)/""GNAT$\1/g"
$  WRITE SEDOUT "s/\(^[ 	][ 	]*\)PROTECTED/\1PROTECTD/"
$  WRITE SEDOUT "s/ DIRECT_IO\([ ;,.]\)/ DEC.DIRECT_IO\1/"
$  WRITE SEDOUT "s/ DIRECT_MIXED_IO\([ ;,.]\)/ DEC.DIRECT_MIXED_IO\1/"
$  WRITE SEDOUT "s/ INDEXED_IO\([ ;,.]\)/ DEC.INDEXED_IO\1/"
$  WRITE SEDOUT "s/ INDEXED_MIXED_IO\([ ;,.]\)/ DEC.INDEXED_MIXED_IO\1/"
$  WRITE SEDOUT "s/ RELATIVE_IO\([ ;,.]\)/ DEC.RELATIVE_IO\1/"
$  WRITE SEDOUT "s/ RELATIVE_MIXED_IO\([ ;,.]\)/ DEC.RELATIVE_MIXED_IO\1/"
$  WRITE SEDOUT "s/ SEQUENTIAL_IO\([ ;,.]\)/ DEC.SEQUENTIAL_IO\1/"
$  WRITE SEDOUT "s/ SEQUENTIAL_MIXED_IO\([ ;,.]\)/ DEC.SEQUENTIAL_MIXED_IO\1/"
$  WRITE SEDOUT "s/ TEXT_IO\([ ;,.]\)/ DEC.TEXT_IO\1/"
$  WRITE SEDOUT "/package X_LIB is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'x_lib.opt"");"
$  WRITE SEDOUT "/package XT is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'xt.opt"");"
$  WRITE SEDOUT "/package XM is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'xm.opt"");"
$  CLOSE SEDOUT
$!
$ NEXTSED:
$  UNIT = F$SEARCH ("*.ADC")
$  IF UNIT .EQS. "" THEN GOTO DONESED
$  define/user sys$output sed.out
$  sed -f declib.sed 'UNIT
$  delete 'UNIT
$  rename sed.out 'UNIT
$  GOTO NEXTSED
$ DONESED:
$  DELETE DECLIB.SED.*
$!
$  IF F$SEARCH ("DECLIB.SED") .NES. "" THEN DELETE DECLIB.SED.*
$  OPEN/WRITE SEDOUT DECLIB.SED
$  WRITE SEDOUT "/package X_LIB is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'x_lib.opt"");"
$  WRITE SEDOUT "/package XT is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'xtr5.opt"");"
$  WRITE SEDOUT "/package XM is/ a\"
$  WRITE SEDOUT "   pragma Linker_Options (""--for-linker=''DECLIBLOC'xmr5.opt"");"
$  CLOSE SEDOUT
$!
$ NEXTSED1:
$  UNIT = F$SEARCH ("*.ADA")
$  IF UNIT .EQS. "" THEN GOTO DONESED1
$  define/user sys$output sed.out
$  sed -f declib.sed 'UNIT
$  delete 'UNIT
$  rename sed.out 'UNIT
$  GOTO NEXTSED1
$ DONESED1:
$  DELETE DECLIB.SED.*
$!
$ NEXTCHOP:
$  UNIT = F$SEARCH ("*.ADC")
$  IF UNIT .EQS. "" THEN GOTO NEXTCHOP1
$  GNAT CHOP/PRESERVE/QUIET/OVERWRITE 'UNIT
$  GOTO NEXTCHOP
$ NEXTCHOP1:
$  UNIT = F$SEARCH ("*.ADA")
$  IF UNIT .EQS. "" THEN GOTO DONECHOP
$  GNAT CHOP/PRESERVE/QUIET/OVERWRITE 'UNIT
$  GOTO NEXTCHOP1
$ DONECHOP:
$!
$  DELETE *.AD%.*/EXCLUDE=(*.ADB,*.ADS)
$!
$!Copy GNAT files.
$ If f$search("address_operations.adb").eqs."" then -
   copy SRCDIR:address_operations.adb []
$ If f$search("current_exception.ads").eqs."" then -
   copy SRCDIR:current_exception.ads []
$ If f$search("dec.ads").eqs."" then -
   copy SRCDIR:dec.ads []
$ If f$search("dec-io.ads").eqs."" then -
   copy SRCDIR:dec-io.ads []
$ If f$search("dec-io.adb").eqs."" then -
   copy SRCDIR:dec-io.adb []
$ If f$search("generic_fast_elementary_functions.adb").eqs."" then -
   copy SRCDIR:generic_fast_elementary_functions.adb []
$ If f$search("get_task_info.ads").eqs."" then -
   copy SRCDIR:get_task_info.ads []
$ If f$search("get_task_info.adb").eqs."" then -
   copy SRCDIR:get_task_info.adb []
$ If f$search("math_lib.adb").eqs."" then -
   copy SRCDIR:math_lib.adb []
$ If f$search("synchronize_nonreentrant_access.adb").eqs."" then -
   copy SRCDIR:synchronize_nonreentrant_access.adb []
$ If f$search("system_runtime_tuning.adb").eqs."" then -
   copy SRCDIR:system_runtime_tuning.adb []
$ If f$search("Makefile.lib").eqs."" then -
   copy SRCDIR:vMakefile.declib []Makefile.lib
$!
$!Delete files patched from nla0 (otherwise they get multiple units).
$ If f$search("directio.ads").Nes."" then del directio.ads.*
$ If f$search("direct_mixed_io.ads").nes."" then del direct_mixed_io.ads.*
$ If f$search("generic_complex_types.ads").nes."" then del generic_complex_types.ads.*
$ If f$search("generic_elementary_functions.ads").nes."" then del generic_elementary_functions.ads.*
$ If f$search("indexed_io.ads").nes."" then del indexed_io.ads.*
$ If f$search("indexed_mixed_io.ads").nes."" then del indexed_mixed_io.ads.*
$ If f$search("relative_io.ads").nes."" then del relative_io.ads.*
$ If f$search("relative_mixed_io.ads").nes."" then del relative_mixed_io.ads.*
$ If f$search("sequenio.ads").nes."" then del sequenio.ads.*
$ If f$search("sequential_mixed_io.ads").nes."" then del sequential_mixed_io.ads.*
$ If f$search("text_io.ads").nes."" then del text_io.ads.*
$ If f$search("complex_fast_elementary_functions.ads").nes."" then del complex_fast_elementary_functions.ads.*
$ If f$search("long_complex_fast_elementary_functions.ads").nes."" then del long_complex_fast_elementary_functions.ads.*
$!
$!
$  IF F$SEARCH ("DECLIB1.SED") .NES. "" THEN DELETE DECLIB1.SED.*
$  OPEN/WRITE SEDOUT DECLIB1.SED
$  WRITE SEDOUT "/^with SYSTEM[ ,;]/ b foo"
$  WRITE SEDOUT ":foo"
$  WRITE SEDOUT "1 i\"
$  WRITE SEDOUT "pragma Extend_System (Aux_DEC);"
$  CLOSE SEDOUT
$!
$ NEXTSED2:
$  UNIT = F$SEARCH ("*.AD%", 1)
$  IF UNIT .EQS. "" THEN GOTO DONESED2
$!
$  define/user sys$output sed.out
$  sed -f DECLIB1.SED 'UNIT
$  delete 'UNIT
$  rename sed.out 'UNIT
$!
$  IF F$PARSE ("''UNIT'",,,"TYPE") .EQS. ".ADS"
$  THEN
$    IF F$EXTRACT (0, 3, F$PARSE ("''UNIT'",,,"NAME")) .EQS. "DEC"
$    THEN
$      IF F$SEARCH ("DECLIB2.SED") .NES. "" THEN DELETE DECLIB2.SED.*
$      OPEN/WRITE SEDOUT DECLIB2.SED
$      WRITE SEDOUT "/with IO_EXCEPTIONS;/ a\"
$      WRITE SEDOUT "        with DEC.IO; use DEC.IO;"
$      CLOSE SEDOUT
$      define/user sys$output sed.out
$      sed -f DECLIB2.SED 'UNIT
$      delete 'UNIT
$      rename sed.out 'UNIT
$    ELSE
$      UNITNAME = F$PARSE ("''UNIT'",,,"NAME")
$      IF F$SEARCH ("DECLIB2.SED") .NES. "" THEN DELETE DECLIB2.SED.*
$      OPEN/WRITE SEDOUT DECLIB2.SED
$      WRITE SEDOUT "1 a\"
$      WRITE SEDOUT "pragma Warnings (Off);\"
$      WRITE SEDOUT "with DEC;\"
$      WRITE SEDOUT "pragma Warnings (On);"
$      CLOSE SEDOUT
$      define/user sys$output sed.out
$      sed -f DECLIB2.SED 'UNIT
$      delete 'UNIT
$      rename sed.out 'UNIT
$    ENDIF
$    IF F$SEARCH (UNITNAME + ".ADB", 2) .NES. ""
$    THEN
$      IF F$SEARCH ("DECLIB2.SED") .NES. "" THEN DELETE DECLIB2.SED.*
$      OPEN/WRITE SEDOUT DECLIB2.SED
$      WRITE SEDOUT "/package \(''UNITNAME'\) is/ a\"
$      WRITE SEDOUT "   pragma Elaborate_Body;"
$      CLOSE SEDOUT
$      define/user sys$output sed.out
$      sed -f DECLIB2.SED 'UNIT
$      delete 'UNIT
$      rename sed.out 'UNIT
$    ENDIF
$  ENDIF
$!
$  GOTO NEXTSED2
$ DONESED2:
$  DELETE DECLIB1.SED.*
$  DELETE DECLIB2.SED.*
$!
$ write sys$output "Patching files for Ada95 and GNAT compatibility"
$ if f$trnlnm("SRCDIR").eqs."VMI$KWD:" then -
$   gnupatch="$SRCDIR:gnupatch.exe"
$ if f$getsyi("VERSION").eqs."V7.1"
$ then
$   pipe gnupatch --quiet <SRCDIR:declibgnat.diff
$   if f$search("lib.adb").nes.""
$   then
$     del lib.adb.*
$     pipe gnupatch --quiet <SRCDIR:declibgnat2.diff
$   else
$!    Not applicable to DEC Ada 3.4
$     pipe gnupatch --quiet <SRCDIR:declibgnat1.diff
$   endif
$ else
$   gnupatch --quiet <SRCDIR:declibgnat.diff
$   if f$search("lib.adb").nes.""
$   then
$     del lib.adb.*
$     gnupatch --quiet <SRCDIR:declibgnat2.diff
$   else
$!    Not applicable to DEC Ada 3.4
$     gnupatch --quiet <SRCDIR:declibgnat1.diff
$   endif
$ endif
$!
$! Uppercase VMS library externals
$!
$ write sys$output "Uppercasing external symbols in imported subprograms"
$ IF F$SEARCH ("DECLIB3.SED") .NES. "" THEN DELETE DECLIB3.SED.*
$ OPEN/WRITE SEDOUT DECLIB3.SED
$ WRITE SEDOUT "/^package X.* is/ a\"
$ WRITE SEDOUT "   pragma External_Name_Casing (Uppercase, Uppercase);"
$ WRITE SEDOUT "/^package body X.* is/ a\"
$ WRITE SEDOUT "   pragma External_Name_Casing (Uppercase, Uppercase);"
$ CLOSE SEDOUT
$ if F$trnlnm ("source_file").nes."" then close source_file
$!
$ NEXTCOM:
$  UNIT = F$SEARCH ("*.AD%", 1)
$  IF UNIT .EQS. "" THEN GOTO DONECOM
$  search/nowarn/nolog/out=nla0: 'unit "pragma INTERFACE_NAME"
$  if $status.eq.%X00000001
$  then
$    define/user sys$output sed.out
$    sed -f DECLIB3.SED 'UNIT
$    delete 'UNIT
$    rename sed.out 'UNIT
$  endif
$!
$  search/nowarn/nolog/out=nla0: 'unit "pragma IMPORT"
$  if $status.eq.%X00000001
$  then
$    search/exact/nowarn/nolog/out=nla0: 'unit "$a","$b","$c","$d","$e","$f","$g","$h","$i","$j","$k","$l","$m","$n","$o","$p","$q","$r","$s","$t","$u","$v","$w","$z","$y","$z"
$    if $status.eq.%X00000001
$    then
$!      dtk$, lbr$, lib$, mth$, ncs$, ots$, ppl$, smg$, str$
$       open/read/write/share SOURCE_FILE 'unit
$       quote := '"'
$ nextline:
$       read/end=closefile SOURCE_FILE b
$       len = f$length (b)
$!
$       loc = f$locate ("""dtk$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""lbr$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""lib$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""mth$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""ncs$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""ots$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""ppl$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""smg$", b)
$       if loc.lt.len then goto foundone
$!
$       loc = f$locate ("""str$", b)
$       if loc.lt.len then goto foundone
$!
$ goto nextline
$ foundone:
$       b1 = f$extract (loc+1, len-loc-1, b)
$       locend = f$locate ("""", b1)
$       b2 = f$extract (loc+1, locend, b)
$       b[loc+1,locend] := 'b2
$       write/update/symbol SOURCE_FILE b
$       goto nextline
$ closefile:
$       close SOURCE_FILE
$    endif
$  endif
$  GOTO NEXTCOM
$ DONECOM:
$ define/user sys$output gnatbind.out
$ mcr gnu:[bin]gnatbind -v
$ define/user sys$output timestamp.out
$ sed -n -e "/^GNATBIND /s/.*(\(.*\)-.*).*/\1/p" gnatbind.out
$ open/read dumptime timestamp.out
$ read dumptime gnatdate
$ gnatdateyy=f$extract(0,4,gnatdate)
$ gnatdatemm=f$extract(4,2,gnatdate)
$ gnatdatedd=f$extract(6,2,gnatdate)
$ close dumptime
$ months="JAN/FEB/MAR/APR/MAY/JUN/JUL/AUG/SEP/OCT/NOV/DEC"
$ month=f$element (f$integer (gnatdatemm) - 1, "/", months)
$ purge
$ timestamp -d [] -t "''gnatdatedd'-''month'-''gnatdateyy' 20:00:00.00"
