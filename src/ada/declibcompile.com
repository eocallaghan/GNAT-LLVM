$ set on
$!Define a symbol so kitinstal can find make
$ make="$gnu:[bin]make.exe"
$!Screwy gyrations of testing for existence necessary to avoid empty string
$! errors from make and to quote uppercase switches
$ if p2.nes.""
$ then
$   make -f Makefile.lib all "''p1'" "''p2'"
$ else
$   if p1.nes.""
$   then
$     make -f Makefile.lib all "''p1'"
$   else
$     make -f Makefile.lib all
$   endif
$ endif
$ set file/prot=(s:rwd,o:rwd,g:r,w:r) *.*
$ if f$search("*.obj").nes.""
$ then
$   lib/replace libdecgnat.olb *.obj
$   delete *.obj.*
$ endif
$ set file/prot=(s:r,o:r,g:r,w:r) *.ali,*.olb
$ if f$search("*.exe").nes.""
$ then
$    set file/prot=(s:re,o:re,g:re,w:re) *.exe
$ endif
