##############################################################################
##                                                                          ##
##                          GNAT COMPILER COMPONENTS                        ##
##                                                                          ##
##                           V M S _ H A N D L E R                          ##
##                                                                          ##
##                       Assembler Implementation File                      ##
##                                                                          ##
##   Copyright (C) 1996, 1997, 2002, 2003 Free Software Foundation, Inc.    ##
##                                                                          ##
## GNAT is free software; you can  redistribute it  and/or modify it under  ##
## terms of the  GNU General Public License as published  by the Free Soft- ##
## ware  Foundation;  either version 2,  or (at your option) any later ver- ##
## sion. GNARL is distributed in the hope that it will be useful, but WITH- ##
## OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY ##
## or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License ##
## for  more details.  You should have  received  a copy of the GNU General ##
## Public License  distributed with GNARL; see file COPYING.  If not, write ##
## to  the Free Software Foundation,  59 Temple Place # Suite 330,  Boston, ##
## MA 02111#1307, USA.                                                      ##
##                                                                          ##
## As a  special  exception,  if you  link  this file  with other  files to ##
## produce an executable,  this file does not by itself cause the resulting ##
## executable to be covered by the GNU General Public License. This except- ##
## ion does not  however invalidate  any other reasons  why the  executable ##
## file might be covered by the  GNU Public License.                        ##
##                                                                          ##
## GNAT was originally developed  by the GNAT team at  New York University. ##
## Extensive contributions were provided by Ada Core Technologies Inc.      ##
##                                                                          ##
##############################################################################

##  This is an OpenVMS/Alpha file

	.set noreorder
	.set volatile
	.file	1 "vmshandler.asm"
gcc2_compiled.:
__gnu_compiled_c:
.text
	.align 3
	.globl __gnat_error_prehandler
$VFB33:
	.ent __gnat_error_prehandler
__gnat_error_prehandler..en:
	.base $27
#
# Switch to alternate stack only on stack overflow, which is an access
# violation (code 12) where the address of the violation is > 0. Probably
# would be better of the address check was for some number > 4K ???
# Also switch on condition 1364, which is STKOVF on VMS 7.2.
#
	lda $0,1364
	ldl $1,4($16)
	cmpeq $1,$0,$1
	bne $1,stkovf
	ldl $1,4($16)
	cmpeq $1,12,$1
	beq $1,notstkovf
	ldl $1,12($16)
	cmpeq $1,0,$1
	beq $1,stkovf
#
# Not a stack overflow, just do the normal thing.
#
notstkovf:
	lda $30,-48($30)
	stq $26,8($30)
	stq $29,16($30)
	.mask 0x20000000,0
	stq $27,0($30)
	bis $30,$30,$29
	.frame $29,48,$26,8
	.prologue
	ldq $26,__gnat_error_handler..lk
	ldq $27,__gnat_error_handler..lk+8
	jsr $26,__gnat_error_handler
	ldq $27,0($29)
#
# Now return to caller, restoring necessary registers
#
	bis $29,$29,$30
	ldq $26,8($30)
	ldq $29,16($30)
	lda $30,48($30)
	ret $31,($26),1
stkovf:
	bis $30,$30,$0		#save stack pointer in temporary register
	lda $1,__gnat_error_prehandler_stack
	ldq $30,0($1)		#new small stack
	lda $30,-48($30)	#carve off a chunk of stack for local use
	stq $26,8($30)
#
# Save all the important registers
#
	stq $29,16($30)
	stq $0,24($30)		#save original stack pointer
	stq $16,32($30)		#save original arguments to handler
	stq $17,40($30)
	stq $27,0($30)
	bis $30,$30,$29
#
# Now get the per thread stack.
#
 	bis $31,0,$25
 	ldq $26,system__soft_links__get_exc_stack_addr_soft..lk
 	ldq $27,system__soft_links__get_exc_stack_addr_soft..lk+8
 	jsr $26,system__soft_links__get_exc_stack_addr_soft
 	ldq $27,0($29)
 	ldq $26,8($29)
#
# Retrieve all the save data so it can be copied to the new stack
#
	ldq $1,24($29)
	ldq $16,32($29)
	ldq $17,40($29)
	lda $30,0($0)		#new per thread stack.
	ldq $0,16($29)
	lda $30,-48($30)	#carve off a chunk.
#
# Save all the data on the new stack
#
	stq $26,8($30)
	stq $0,16($30)
	stq $1,24($30)
	stq $16,32($30)
	stq $17,40($30)
	stq $27,0($30)
	bis $30,$30,$29
#
# Retrieve the original arguments and call the real error handler.
#
	bis $31,2,$25
	ldq $16,32($29)
	ldq $17,40($29)
	ldq $26,__gnat_error_handler..lk
	ldq $27,__gnat_error_handler..lk+8
	jsr $26,__gnat_error_handler
	ldq $27,0($29)
#
# Return to caller in the unlikely case we get here.
#
	bis $29,$29,$30
	ldq $26,8($30)
	ldq $29,16($30)		#reload original frame pointer
	ldq $30,24($30)		#reload original stack pointer
	ret $31,($26),1
	.end __gnat_error_prehandler
	.comm __gnat_error_prehandler_stack,4,8
$VFE33:
.section	.vmsdebug
	.align 0
	.word	0x24	;# record length
	.word	0xbc	;# record type (modbeg)
	.byte	0x2	;# flags
	.byte	0x0	;# unused
	.long	0x7	;# language
	.word	0x1	;# DST major version
	.word	0xd	;# DST minor version
	.byte	0xa	;# length of module name
	.ascii "VMSHANDLER"	;# module name
	.byte	0xb	;# length of compiler name
	.ascii "GNU C 2.7.2"	;# compiler name
	.word	0x24	;# record length
	.word	0xbe	;# record type (rtnbeg)
	.byte	0x80	;# flags
	.long	__gnat_error_prehandler..en	;# routine entry name
	.long	__gnat_error_prehandler	;# routine procedure descriptor
	.byte	0x17	;# length of routine name
	.ascii "__GNAT_ERROR_PREHANDLER"	;# routine name
	.word	0x8	;# record length
	.word	0xbf	;# record type (rtnend)
	.byte	0x0	;# unused
	.long	$VFE33-$VFB33	;# routine size
	.word	0x3	;# record length
	.word	0xbd	;# record type (modend)
.link
	.align 3
__gnat_error_prehandler:
	.pdesc __gnat_error_prehandler..en,stack
__gnat_error_handler..lk:
	.linkage __gnat_error_handler
system__soft_links__get_exc_stack_addr_soft..lk:
	.linkage system__soft_links__get_exc_stack_addr_soft
