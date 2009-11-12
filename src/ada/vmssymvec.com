$!
$! This is the VMSSYMVEC.COM command file for creating a shared image
$! symbol vector from an object file.
$!
$! Invocation: "@ vmssymvec <object_file>.obj".  Creates <object_file>.sym.
$!
$! The symbol vector contains information which is passed to the linker in
$! order to create universal symbols.  Currently all global symbols are
$! made universal since there is no practical way to limit them to
$! only those referenced outside of the shareable image.
$!
$ if f$search("tmpsyms.out").nes."" then delete tmpsyms.out.*
$ define/user sys$output tmpsyms.out
$ objdump --syms 'p1'
$ if f$search("tmpsyms.out").eqs."" then exit
$!
$ if f$search("tmpglobsyms.out").nes."" then delete tmpglobsyms.out.*
$ search/out=tmpglobsyms.out tmpsyms.out "$LINK$","$DATA$","$BSS$","$READONLY$"
$ delete tmpsyms.out;
$ if f$search("tmpglobsyms.out").eqs."" then exit
$!
$ if f$search("tmpsymvecsyms.out").nes."" then delete tmpsymvecsyms.out.*
$ define/user sys$output tmpsymvecsyms.out
$ sed -e "s/.*\$LINK\$   \(.*\)/\1=PROCEDURE/" -e "s/.*\$DATA\$   \(.*\)/\1=DATA/" -e "s/.*\$BSS\$    \(.*\)/\1=DATA/" -e "s/.*\$READONLY\$ \(.*\)/\1=DATA/" tmpglobsyms.out
$ delete tmpglobsyms.out;
$ if f$search("tmpsymvecsyms.out").eqs.""
$ then
$ outfile=f$parse (p1,,,"name")
$  open/write symvecfile 'outfile'.sym
$  close symvecfile
$  exit
$ endif
$!
$ outfile=f$parse (p1,,,"name")
$ open/write symvecfile 'outfile'.sym
$ write symvecfile "case_sensitive=yes"
$ write symvecfile "SYMBOL_VECTOR=(-"
$ open/read symvecsyms tmpsymvecsyms.out
$ read symvecsyms oneline
$ read_loop:
$  read/end_of_file=endit symvecsyms nextline
$  write symvecfile oneline,",-"
$  oneline=nextline
$  goto read_loop
$ endit:
$ write symvecfile oneline,")"
$ write symvecfile "case_sensitive=NO"
$ close symvecsyms
$ close symvecfile
$ exit
