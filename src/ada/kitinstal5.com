!****************************************************************
!                                                               *
!                        GNAT KITINSTAL.COM                     *
!                                                               *
!****************************************************************
$ !
$ !         Take care of interrupts
$ !
$ ON CONTROL_Y THEN VMI$CALLBACK CONTROL_Y
$ !
$ !            Process errors
$ !
$ ON WARNING THEN GOTO ERR_EXIT
$ !
$ !         Determine course of action
$ !
$ IF P1 .EQS. "VMI$_INSTALL" THEN GOTO GNAT_INSTALL
$ IF P1 .EQS. "VMI$_POSTINSTALL" THEN GOTO GNAT_POSTINSTALL
$ IF P1 .EQS. "VMI$_IVP" THEN GOTO GNAT_IVP
$ IF P1 .EQS. "HELP1" THEN GOTO HELP1
$ IF P1 .EQS. "HELP2" THEN GOTO HELP2
$ IF P1 .EQS. "HELP3" THEN GOTO HELP3
$ IF P1 .EQS. "HELP4" THEN GOTO HELP4
$ IF P1 .EQS. "HELP5" THEN GOTO HELP5
$ EXIT VMI$_UNSUPPORTED
$ !
$ HELP1:
$ TYPE SYS$INPUT

  There are basically 2 options for installation:

  1) Install GNAT in a standard location.
     (Note: This includes directories under SYS$COMMON:[SYSLIB].)

  2) Install GNAT in a non-standard location that you will specify.
     (Note: This directory must already exist, the installation procedure
            will create subdirectories under this directory to hold the
            installed files.)

$ EXIT VMI$_SUCCESS
$ !
$ HELP2:
$ TYPE SYS$INPUT

  Add the following lines to your login.com for non-standard startup
  (This information is repeated in the release notes):

$ WRITE SYS$OUTPUT "  @''P2'GNAT_STARTUP"
$ EXIT VMI$_SUCCESS
$ !
$ HELP3:
$ TYPE SYS$INPUT

  The GNAT - DEC Ada compatibility library (DECLIB) is created from
  your existing DEC Ada run-time sources. If you do not have DEC Ada,
  enter "NONE".

$ EXIT VMI$_SUCCESS
$ !
$ HELP4:
$ TYPE SYS$INPUT

  GNAT can be installed in one of 3 ways:
  [1] DEC Ada compatible I/O packages (Text_IO, etc) as the default.
      Ada95 I/O packages available if explicitly imported.

  [2] Ada95 I/O packages (Text_IO, etc) as the default.
      DEC Ada compatible I/O packages available if explicitly imported.

  [3] Ada95 I/O packages (Text_IO, etc) as the default.
      DEC Ada compatible I/O packages not available.


$ EXIT VMI$_SUCCESS
$ !
$ HELP5:
$ TYPE SYS$INPUT

   The Express runtime optimizes tasking by disallowing abort statements,
   asynchronous transfer of control (ATC), and dynamic changes of task
   priorities. This is similar to Ada83 style tasking.

$ EXIT VMI$_SUCCESS
$ !
$ !         Install the product
$ !
$ GNAT_INSTALL:
$ GNAT_DEST0=""
$ GNAT_DECADA0=""
$ !
$ !       Check that OpenVMS version is 7.1 or later for OpenVMS AXP
$ !
$ VMI$CALLBACK CHECK_VMS_VERSION GNAT_VERSION 7.1
$ IF .NOT. GNAT_VERSION
$ THEN
$   VMI$CALLBACK MESSAGE E VERSION -
       "This kit must be installed on OpenVMS/Alpha 7.1 or above system."
$   EXIT VMI$_FAILURE
$ ENDIF
$ !
$ VMI$CALLBACK CHECK_VMS_VERSION GNAT_VERSION 7.1
$ IF .NOT. GNAT_VERSION
$ THEN
$   VMI$CALLBACK MESSAGE W VERSION -
       "This kit should be installed on OpenVMS/Alpha 7.1 or above system."
$ ENDIF
$ !
$ IF F$GETJPI("","PGFLQUOTA").LT.150000
$ THEN
$   VMI$CALLBACK MESSAGE E PGFLQUOTA -
       "Installation account must have a page file quota"
$   VMI$CALLBACK MESSAGE E PGFLQUOTA -
       "of at least 150000"
$   EXIT VMI$_FAILURE
$ ENDIF
$ !
$ !             Check for a sufficient working set
$ !
$ VMI$CALLBACK GET_SYSTEM_PARAMETER GNAT_WS WSMAX
$ IF GNAT_WS .LT. 131072
$ THEN
$    VMI$CALLBACK MESSAGE W WORKINGSET -
         "System parameter WSMAX should be at least 131072 pagelets"
$    VMI$CALLBACK MESSAGE W WORKINGSET -
         "for optimum performance."
$    VMI$CALLBACK MESSAGE I WORKINGSET -
         "Currently it is ''GNAT_WS' pagelets."
$ ENDIF
$ !
$ !             Check for a sufficient cache
$ !
$ VMI$CALLBACK GET_SYSTEM_PARAMETER GNAT_CACHE VCC_MAXSIZE
$ IF GNAT_CACHE .LT. 51200
$ THEN
$    VMI$CALLBACK MESSAGE W CACHESIZE -
         "System parameter VCC_MAXSIZE should be at least 51200 pagelets"
$    VMI$CALLBACK MESSAGE W CACHESIZE -
         "for optimum performance."
$    VMI$CALLBACK MESSAGE I CACHESIZE -
         "Currently it is ''GNAT_CACHE' pagelets."
$ ENDIF
$ !
$ !             Check for a sufficient number of channels
$ !
$ VMI$CALLBACK GET_SYSTEM_PARAMETER GNAT_CHANNELS CHANNELCNT
$ IF GNAT_CHANNELS .LT. 512
$ THEN
$    VMI$CALLBACK MESSAGE W CHANNELCNT -
         "System parameter CHANNELCNT must be at least 512 channels"
$    VMI$CALLBACK MESSAGE W CHANNELCNT -
         "to rebuild run time library."
$    VMI$CALLBACK MESSAGE I CHANNELCNT -
         "Currently it is ''GNAT_CHANNELS' channels."
$ ENDIF
$ !
$ !             Check for disk space, warn if not enough
$ !
$ VMI$CALLBACK CHECK_NET_UTILIZATION GNAT_SPACE 310000 155000 145000
$ IF .NOT. GNAT_SPACE
$ THEN
$   VMI$CALLBACK MESSAGE W DISKSPACE -
        "Insufficient disk space available for GNAT installation on"
$   VMI$CALLBACK MESSAGE W DISKSPACE -
        "ROOT device. You must either install in a non-standard"
$   VMI$CALLBACK MESSAGE W DISKSPACE -
        "directory or choose the option to delete an existing GNAT"
$   VMI$CALLBACK MESSAGE W DISKSPACE -
        "installation. Proceed at your own risk."
$   VMI$CALLBACK ASK GNAT_CONT "Continue" "NO" B
$   IF .NOT. GNAT_CONT THEN EXIT VMI$_FAILURE
$ ENDIF
$ !
$ SET PROC/PRIV=BYPASS
$ VMI$CALLBACK SET SAFETY NO
$ GOTO MUSTEXIST
$ !
$ MAYBEABORT:
$ VMI$CALLBACK ASK GNAT_ABORT "Abort installation" "NO" B
$ IF GNAT_ABORT THEN EXIT VMI$_FAILURE
$ !
$ !             Ask for install directory
$ !
$ MUSTEXIST:
$ IF GNAT_DEST0.EQS."" THEN GNAT_DEST0="SYS$COMMON:[SYSLIB.GNAT]"
$ VMI$CALLBACK ASK GNAT_DEST "Installation Directory" "''GNAT_DEST0'" U -
    "@VMI$KWD:KITINSTAL HELP1"
$ IF GNAT_DEST.EQS."SYS$COMMON:[SYSLIB.GNAT]"
$ THEN
$   GNAT_DEST0=GNAT_DEST
$   GNAT_GCCLIB="SYS$COMMON:[SYSLIB.GNAT]"
$   GNAT_GCCLIBBIN="SYS$COMMON:[SYSLIB.GNAT.BIN]"
$   GNAT_GCCLIBEXEC="SYS$COMMON:[SYSLIB.GNAT.LIB.GCC-LIB]"
$   UNIXGCCLIB="/SYS$COMMON/SYSLIB/GNAT/LIB/GCC-LIB"
$   IF f$parse(GNAT_GCCLIB).NES.""
$   THEN
$     VMI$CALLBACK ASK GNAT_DELETE "Overwrite existing ''GNAT_GCCLIB'" "NO" B
$     IF .NOT. GNAT_DELETE THEN GOTO MAYBEABORT
$   ENDIF
$   GNAT_GCCLIBDOTDOTDOT="SYS$COMMON:[SYSLIB.GNAT...]"
$ ELSE
$   GNAT_DEST0=GNAT_DEST
$   GNAT_BASEDEV=f$parse(GNAT_DEST,,,"DEVICE")
$   GNAT_BASEDIR=f$parse(GNAT_DEST,,,"DIRECTORY")
$   GNAT_GCCLIB=f$string(GNAT_BASEDEV+f$extract(0,f$length(GNAT_BASEDIR)-1,GNAT_BASEDIR)+"]")
$   GNAT_GCCLIBBIN=f$string(GNAT_BASEDEV+f$extract(0,f$length(GNAT_BASEDIR)-1,GNAT_BASEDIR)+".BIN]")
$   GNAT_GCCLIBEXEC=f$string(GNAT_BASEDEV+f$extract(0,f$length(GNAT_BASEDIR)-1,GNAT_BASEDIR)+".LIB.GCC-LIB]")
$   DEFINE/USER SYS$OUTPUT VMI$KWD:UNIXGCCLIB.TMP
$   MCR VMI$KWD:TRANSLATE_VMS 'GNAT_GCCLIB
$   OPEN/READ TMPCHAN VMI$KWD:UNIXGCCLIB.TMP
$   READ TMPCHAN UNIXGCCLIB
$   CLOSE TMPCHAN
$   UNIXGCCLIB=UNIXGCCLIB+"/LIB/GCC-LIB"
$   IF f$parse(GNAT_GCCLIBEXEC).NES.""
$   THEN
$     VMI$CALLBACK ASK GNAT_DELETE "Overwrite existing ''GNAT_GCCLIB'" "NO" B
$     IF .NOT. GNAT_DELETE THEN GOTO MAYBEABORT
$   ENDIF
$   GNAT_GCCLIBDOTDOTDOT=f$string(f$extract(0,f$length(GNAT_GCCLIB)-1,GNAT_GCCLIB)+"...]")
$   VMI$CALLBACK ASK GNAT_CONT "Continue" "" BH "@VMI$KWD:KITINSTAL HELP2 ''GNAT_GCCLIB'"
$   IF .NOT. GNAT_CONT THEN EXIT VMI$_FAILURE
$ ENDIF
$ !
$ GNAT_DECADA0="SYS$COMMON:[SYSLIB.ADALIB]"
$ GNAT_COMPAT0=1
$ IF f$parse (GNAT_DECADA0).EQS.""
$ THEN
$   GNAT_DECADA0="NONE"
$   GNAT_COMPAT0=3
$ ENDIF
$!
$ GET_COMPAT:
$ VMI$CALLBACK ASK GNAT_COMPAT "Compatibility -  1:DEC,Ada95  2:Ada95,DEC  3:Ada95 only?" -
    "''GNAT_COMPAT0'" I "@VMI$KWD:KITINSTAL HELP4"
$ IF GNAT_COMPAT.LT.1.OR.GNAT_COMPAT.GT.3 THEN GOTO GET_COMPAT
$!
$ GET_DECADA:
$ IF GNAT_COMPAT.EQ.1.OR.GNAT_COMPAT.EQ.2
$ THEN
$   VMI$CALLBACK ASK GNAT_DECADA "Existing DEC Ada Directory" "''GNAT_DECADA0'" U -
      "@VMI$KWD:KITINSTAL HELP3"
$   IF GNAT_DECADA.EQS."NONE"
$   THEN
$     GNAT_COMPAT0=3
$     GOTO GET_COMPAT
$   ENDIF
$   IF f$parse (GNAT_DECADA).NES.""
$   THEN
$     VMI$CALLBACK ASK GNAT_SURE "Use ''GNAT_DECADA' to create GNAT DECLIB" "YES" B
$     IF .NOT.GNAT_SURE THEN GOTO GET_DECADA
$   ELSE
$     VMI$CALLBACK MESSAGE E NODIRECTORY "''GNAT_DECADA' does not exist"
$     GOTO GET_DECADA
$   ENDIF
$ ELSE
$   GNAT_DECADA="NONE"
$ ENDIF
$!
$ VMI$CALLBACK ASK GNAT_ALTRUNTIME "Install Express Runtime" "NO" B -
    "@VMI$KWD:KITINSTAL HELP5"
$ IF .NOT. GNAT_ALTRUNTIME THEN GOTO GNAT_RESTORE
$ VMI$CALLBACK ASK GNAT_ALTRUNTIMEDEF "Make Express Runtime the Default" "NO" B
$ !
$ !             Restore the GNAT saveset to the selected directory
$ !
$ GNAT_RESTORE:
$ IF f$parse (GNAT_GCCLIBEXEC).NES.""
$ THEN
$   VMI$CALLBACK MESSAGE I REMOVE "Removing old GNAT ADALIB"
$   LIBTODELETE=UNIXGCCLIB+"/ADALIB"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   LIBTODELETE=UNIXGCCLIB+"/RTS-*/ADALIB"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   VMI$CALLBACK MESSAGE I REMOVE "Removing old GNAT ADAINCLUDE"
$   LIBTODELETE=UNIXGCCLIB+"/ADAINCLUDE"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   LIBTODELETE=UNIXGCCLIB+"/RTS-*/ADAINCLUDE"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   VMI$CALLBACK MESSAGE I REMOVE "Removing old GNAT DECLIB"
$   LIBTODELETE=UNIXGCCLIB+"/DECLIB"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   LIBTODELETE=UNIXGCCLIB+"/RTS-*/DECLIB"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   OLD_GNATCHOP="''GNAT_GCCLIBBIN'GNATCHOP.COM"
$   IF f$search (OLD_GNATCHOP).NES.""
$   THEN
$      VMI$CALLBACK MESSAGE I REMOVE "Removing old GNATCHOP command file"
$      DELETE 'OLD_GNATCHOP.*
$   ENDIF
$   OLD_GNATCHP="''GNAT_GCCLIBBIN'GNATCHP.EXE"
$   IF f$search (OLD_GNATCHP).NES.""
$   THEN
$      VMI$CALLBACK MESSAGE I REMOVE "Removing old GNATCHP program"
$      DELETE 'OLD_GNATCHP.*
$   ENDIF
$ ENDIF
$ !
$ GNAT_RTS_STD=f$string(f$extract(0,f$length(GNAT_GCCLIB)-1,GNAT_GCCLIB)+".LIB.GCC-LIB.RTS-STD]")
$ GNAT_RTS_EXPRESS=f$string(f$extract(0,f$length(GNAT_GCCLIB)-1,GNAT_GCCLIB)+".LIB.GCC-LIB.RTS-EXPRESS]")
$ !
$ VMI$CALLBACK MESSAGE I RESTORE "Installing new files, this may take several minutes"
$ BACKUP VMI$KWD:GNAT.BCK/SAVE/select=[build.gcclib...] 'GNAT_GCCLIBDOTDOTDOT'/NEW_VERSION
$ IF GNAT_COMPAT.EQ.1
$ THEN
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/declib/:/gnu/lib/gcc-lib/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/declib/:/gnu/lib/gcc-lib/adalib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/declib/:/gnu/lib/gcc-lib/rts-std/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/declib/:/gnu/lib/gcc-lib/rts-std/adalib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/declib/:/gnu/lib/gcc-lib/rts-express/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/declib/:/gnu/lib/gcc-lib/rts-express/adalib/"
$   CLOSE PATHFILE
$ ENDIF
$ !
$ IF GNAT_COMPAT.EQ.2
$ THEN
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/adainclude/:/gnu/lib/gcc-lib/declib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/adalib/:/gnu/lib/gcc-lib/declib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/adainclude/:/gnu/lib/gcc-lib/rts-std/declib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/adalib/:/gnu/lib/gcc-lib/rts-std/declib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/adainclude/:/gnu/lib/gcc-lib/rts-express/declib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/adalib/:/gnu/lib/gcc-lib/rts-express/declib/"
$   CLOSE PATHFILE
$ ENDIF
$ !
$ IF GNAT_COMPAT.EQ.3
$ THEN
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_GCCLIBEXEC'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/adalib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_STD'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-std/adalib/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_source_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/adainclude/"
$   CLOSE PATHFILE
$   OPEN/WRITE PATHFILE 'GNAT_RTS_EXPRESS'ada_object_path.
$   WRITE PATHFILE "/gnu/lib/gcc-lib/rts-express/adalib/"
$   CLOSE PATHFILE
$ ENDIF
$ PURGE 'GNAT_GCCLIBDOTDOTDOT'
$ !
$ rename 'GNAT_RTS_STD'declib.dir 'GNAT_GCCLIBEXEC'
$ rename 'GNAT_RTS_STD'adalib.dir 'GNAT_GCCLIBEXEC'
$ rename 'GNAT_RTS_STD'adainclude.dir 'GNAT_GCCLIBEXEC'
$ !
$ GNAT_DECLIB=f$string(f$extract(0,f$length(GNAT_GCCLIB)-1,GNAT_GCCLIB)+".LIB.GCC-LIB.DECLIB]")
$ !
$ IF GNAT_DECADA.EQS."NONE"
$ THEN
$   SET DEF 'GNAT_DECLIB
$   SET FILE/PROT=(S:RWD,O:RWD) *.adb.*,*.ads.*,libdecgnat.olb.*,Makefile.lib
$   DELETE *.adb.*,*.ads.*,libdecgnat.olb.*,Makefile.lib.*
$   LIB/CREATE LIBDECGNAT.OLB
$ ELSE
$   IF GNAT_DEST.NES."SYS$COMMON:[SYSLIB]"
$   THEN
$     @VMI$KWD:GNAT$STARTUP.COM "''GNAT_GCCLIB'"
$   ELSE
$     @VMI$KWD:GNAT$STARTUP.COM
$   ENDIF
$   VMI$CALLBACK MESSAGE I BUILD "Building Standard DECLIB from DEC Ada predefined library"
$   SET DEF 'GNAT_DECLIB
$   @VMI$KWD:DECLIBBUILD VMI$KWD: 'GNAT_DECADA "GNU:[LIB.GCC-LIB.RTS-STD.DECLIB]"
$   @VMI$KWD:DECLIBCOMPILE
$ ENDIF
$ !
$ rename 'GNAT_GCCLIBEXEC'declib.dir 'GNAT_RTS_STD'
$ rename 'GNAT_GCCLIBEXEC'adalib.dir 'GNAT_RTS_STD'
$ rename 'GNAT_GCCLIBEXEC'adainclude.dir 'GNAT_RTS_STD'
$ !
$ IF .NOT. GNAT_ALTRUNTIME
$ THEN
$   LIBTODELETE=UNIXGCCLIB+"/RTS-EXPRESS"
$   MCR VMI$KWD:rm -rf 'LIBTODELETE
$   GOTO SKIP_ALTRUNTIME
$ ENDIF
$ !
$ rename 'GNAT_RTS_EXPRESS'declib.dir 'GNAT_GCCLIBEXEC'
$ rename 'GNAT_RTS_EXPRESS'adalib.dir 'GNAT_GCCLIBEXEC'
$ rename 'GNAT_RTS_EXPRESS'adainclude.dir 'GNAT_GCCLIBEXEC'
$ !
$ IF GNAT_DECADA.EQS."NONE"
$ THEN
$   SET DEF 'GNAT_DECLIB
$   SET FILE/PROT=(S:RWD,O:RWD) *.adb.*,*.ads.*,libdecgnat.olb.*,Makefile.lib
$   DELETE *.adb.*,*.ads.*,libdecgnat.olb.*,Makefile.lib.*
$   LIB/CREATE LIBDECGNAT.OLB
$ ELSE
$   IF GNAT_DEST.NES."SYS$COMMON:[SYSLIB]"
$   THEN
$     @VMI$KWD:GNAT$STARTUP.COM "''GNAT_GCCLIB'"
$   ELSE
$     @VMI$KWD:GNAT$STARTUP.COM
$   ENDIF
$   VMI$CALLBACK MESSAGE I BUILD "Building Express DECLIB from DEC Ada predefined library"
$   SET DEF 'GNAT_DECLIB
$   @VMI$KWD:DECLIBBUILD VMI$KWD: 'GNAT_DECADA "GNU:[LIB.GCC-LIB.RTS-EXPRESS.DECLIB]"
$   @VMI$KWD:DECLIBCOMPILE
$ ENDIF
$ !
$ rename 'GNAT_GCCLIBEXEC'declib.dir 'GNAT_RTS_EXPRESS'
$ rename 'GNAT_GCCLIBEXEC'adalib.dir 'GNAT_RTS_EXPRESS'
$ rename 'GNAT_GCCLIBEXEC'adainclude.dir 'GNAT_RTS_EXPRESS'
$ !
$ SKIP_ALTRUNTIME:
$ !
$ IF .NOT. GNAT_ALTRUNTIME
$ THEN
$    COPY 'GNAT_RTS_STD'ada_source_path. 'GNAT_GCCLIBEXEC
$    COPY 'GNAT_RTS_STD'ada_object_path. 'GNAT_GCCLIBEXEC
$ ELSE
$    IF GNAT_ALTRUNTIMEDEF
$    THEN
$       COPY 'GNAT_RTS_EXPRESS'ada_source_path. 'GNAT_GCCLIBEXEC
$       COPY 'GNAT_RTS_EXPRESS'ada_object_path. 'GNAT_GCCLIBEXEC
$    ELSE
$       COPY 'GNAT_RTS_STD'ada_source_path. 'GNAT_GCCLIBEXEC
$       COPY 'GNAT_RTS_STD'ada_object_path. 'GNAT_GCCLIBEXEC
$    ENDIF
$ ENDIF
$ !
$ !       Specifiy IVP to run
$ !
$ VMI$CALLBACK SET IVP YES
$ !
$ !       Move the help file
$ !
$ VMI$CALLBACK PROVIDE_DCL_HELP GNAT.HLP
$ VMI$CALLBACK PROVIDE_DCL_HELP GDB.HLP
$ !  
$ !       Move the startup command
$ !
$ VMI$CALLBACK PROVIDE_FILE GNAT_STARTUP GNAT$STARTUP.COM -
    VMI$ROOT:[SYS$STARTUP]
$ !
$ !       Create IVP dirctory
$ !
$ VMI$CALLBACK CREATE_DIRECTORY COMMON SYSTEST.GNAT /PROTECTION=(S:RWE,O:RWE,G:RWE,W:RWE)
$ !
$ !       Move IVP file
$ !
$ VMI$CALLBACK PROVIDE_FILE GNAT_IVP GNAT_IVP.COM VMI$ROOT:[SYSTEST]
$ VMI$CALLBACK PROVIDE_FILE GNAT_TEST1 TEST1.ADB VMI$ROOT:[SYSTEST.GNAT]
$ VMI$CALLBACK PROVIDE_FILE GNAT_TEST2 TEST2.ADB VMI$ROOT:[SYSTEST.GNAT]
$ !
$ !       Identify the startup command file
$ !
$ IF GNAT_DEST.NES."SYS$COMMON:[SYSLIB]"
$ THEN
$   VMI$CALLBACK SET STARTUP GNAT$STARTUP.COM "''GNAT_GCCLIB'"
$ ELSE
$   VMI$CALLBACK SET STARTUP GNAT$STARTUP.COM
$ ENDIF
$ !
$ !       Installation completed, exit
$ !
$ EXIT VMI$_SUCCESS
$ !
$ ! Post-install phase
$ !
$ GNAT_POSTINSTALL
$ !
$ !      No postinstall work
$ !
$ EXIT VMS$_SUCCESS
$ !
$ ! Verify installation
$ !
$ GNAT_IVP:
$ !
$ !       run the ivp
$ !
$ @SYS$TEST:GNAT_IVP 'GNAT_COMPAT
$ !
$ !       ivp completed, indicate results
$ !
$ EXIT $STATUS
$ !
$ ERR_EXIT:
$ EXIT VMI$_FAILURE
