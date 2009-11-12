



.title ADA$MSGDEF ! Ada condition values/message IDs
.ident '1-033'




.facility /SYSTEM Ada, 49




.page 
.base 1


.severity fatal
EXCEPTION/fao=1	"Exception !AC"
NON_ADA_ERROR		"NON_ADA_ERROR"
.page 
.base 100


CONSTRAINT_ERRO	"CONSTRAINT_ERROR"
.page 
.base 200


NUMERIC_ERROR		"NUMERIC_ERROR"
.page 
.base 300


PROGRAM_ERROR		"PROGRAM_ERROR"
RECNOTPOS		"Program is erroneous, error recovery by exception handling is not possible"
ATTUNWTAS		"Attempting to unwind the first stack frame of a task"
ATTUNWREN		"An attempt was made to unwind a rendezvous in progress"
ASTDELTER		"An AST was delivered, but the task is terminated"
EXCDUREXC		"An exception occurred in the DEC Ada run-time library while handling an exception"
TASTERAST		"A task is terminating with an AST pending"
FAIMODTIM		"Unable to modify time-slice setting"
FAISETTIM		"Unable to request another time-slice AST"
UNLCKNOTOWN		"A task tried to unlock the global lock without first locking it"
.base 350
ERRONEOUS		"Program is erroneous"
ALICOLILL		"Requested alignment for a collection is illegal"
ASTNOTCAL		"The task named in an AST_ENTRY attribute is not callable"
ASTPKTQUO		"The AST packet pool has been exhausted"
INTDATCOR		"Internal data in the DEC Ada run-time library is corrupted"
STOSIZILL		"Requested value of STORAGE_SIZE for a collection is illegal"
TOOMANENT		"Task type has too many entries"
TASSTOSMA		"Requested STORAGE_SIZE for a task is illegal"
SUBNOTELA		"The body of the called subprogram has not yet been elaborated"
SIZCOLCRE		"Attempting to get the size of a collection before its creation"
.base 380
SELALTCLS		"All select alternatives are closed and there is no else part"
.page 
.base 400


STORAGE_ERROR		"STORAGE_ERROR"
INSSPAAST		"Insufficient space to deliver an AST"

INSSPATAS		"Insufficient space to create a task"
INSSPACOL		"Insufficient space to create a collection"
INSSPAALL		"Insufficient space to allocate from a collection"
STAOVF			"Attempted stack overflow was detected"
INSSPACUREXC		"Insufficient space for package CURRENT_EXCEPTION allocation"
.page 
.base 500


TASKING_ERROR		"TASKING_ERROR"
TASNOTCAL		"The task named on an entry call is not callable"
CALTASCOM		"Called task completed before accepting the call"
CALTASABO		"Called task completed abnormally during rendezvous"

TASCOMACT		"A task completed during its activation"
TASNOTELA		"A task's body was not elaborated before its activation"
.page 
.base 600


DATA_ERROR		"DATA_ERROR"
.page 
.base 700


DEVICE_ERROR		"DEVICE_ERROR"
.page 
.base 800


END_ERROR		"END_ERROR"
.page 
.base 900


LAYOUT_ERROR		"LAYOUT_ERROR"
.page 
.base 1000


MODE_ERROR		"MODE_ERROR"
.page 
.base 1100


NAME_ERROR		"NAME_ERROR"
.page 
.base 1200


STATUS_ERROR		"STATUS_ERROR"
NOT_OPEN		"File is not open"
ALREADY_OPEN		"File is already open"
.page 
.base 1300


USE_ERROR		"USE_ERROR"

.severity informational
UNSUPPORTED		"The input-output package does not support the intended operation"
FAC_MODE_MISMAT	"The file access does not allow the new mode"
ORG_MISMATCH		"The file organization does not match the organization specified in the FORM parameter"
RFM_MISMATCH		"The file record format does not match the record format specified in the FORM parameter"
RAT_MISMATCH		"The file record attribute does not match the record attribute specified in the FORM parameter"
MRS_MISMATCH		"The file maximum record size does not match the maximum record size specified in the FORM parameter"
MRN_MISMATCH		"The file maximum record number does not match the maximum record number specified in the FORM parameter"
KEY_MISMATCH		"The file key does not match the key value specified in the FORM parameter"
MAXLINEXC		"Maximum line length exceeded"
LINEXCMRS		"Line will exceed external file's maximum record size"
AMBKEYFORM		"Ambiguous keyword in FORM parameter"
INVVALFORM		"Invalid attribute value in FORM parameter"
MISKEYFORM		"Missing or unrecognized keyword in FORM parameter"
SYNERRFORM		"Syntax error in FORM parameter"
.page 
.base 1350


.severity fatal
KEY_ERROR		"Key is inappropriate for the file"
KEYSIZERR		"Size of the key is not a multiple of 8 bits"
.page 
.base 1360


EXISTENCE_ERROR	"The element does not exist"
.page 
.base 1370


LOCK_ERROR		"The element is locked"

.page 
.base 1400


OBJNOTELA		"Object has unexpected value; it may not be fully elaborated"

.severity informational
RTLEVENT		"Debugging event"
ASTDELNOT		"An AST is being delivered, but the task is not callable"
ACTNOTRUN		"The task to be made active could not be run"
EXCABSOTH		"An exception is being absorbed in an OTHERS clause"
EXCOCCREN		"The above exception is propagating out of a rendezvous"
TASTEREXC		"A task is terminating because of the above unhandled exception"
TASTERABO		"A task is terminating because of an abort"

.page 
.base 1500



.severity fatal
FATINTERR		"Fatal internal error in the DEC Ada run-time library"

SEMNOTVAL		"Semaphore not valid"
CODLOCIS/fao=1		"Code locator symbol ADA$$BUG_CHECK is at !XL"
ASSERTFAI		"An assertion failed"
IOSYSFAILED		"Fatal I/O System error"
.page 
.base 1600



.severity informational
MOREINFO/fao=1		"!AC"
MAXALLVAL/fao=1	"The maximum allowed value is !SL"
MINALLVAL/fao=1	"The minimum allowed value is !SL"
EXCRAIPRI/fao=1	"Exception raised prior to PC = !XL"
OUTRNGVAL/fao=1	"The out-of-range value is !SL"
OUTRANVAL/fao=1	"The out-of-range value is !SL"
PCATERR/fao=1		"The PC at the error was !XL"
SHOWPCPSL/fao=1	"PC = !XL, PSL = !XL"
SKIPPCPSL/fao=1	"!+!+"
TASIDIS/fao=1		"The task's id is %TASK !UL"
TASTYPEIS/fao=1	"The task's type is !AC"

.severity fatal
IMAFOREXI		"The image was forced to exit"
UNEERRCAL/fao=1	"Unexpected error status from a call, hex error code = !XL"

.severity informational
TASPROUNH/fao=1	"Task with ID %TASK !UL of type !AC is propagating an unhandled exception"
TASTERUNH/fao=1	"Task with ID %TASK !UL of type !AC has terminated due to unhandled exception"
.base 1700


.severity fatal
TASRUNDOW		"Task run-down signal: task is terminating"
ATTENTION		"Task attention signal: task has received an asynchronous exception"

EXCCOP/fao=1		"Exception was copied at a raise or accept statement !+ !+!+"
EXCCOPLOS/fao=1	"Exception was copied at a raise or accept statement, but some details were lost !+ !+!+"
SIGVECIMP		"Signal vector is improperly formatted -- one or more FAO arguments are missing"


.severity error
FORTRABAC		"Force traceback signal: an unhandled exception is terminating main program"





.severity fatal
EXCCOPX/fao=1		"Exception was copied at a raise or accept statement !+ !+!+"
EXCCOPLOSX/fao=1	"Exception was copied at a raise or accept statement, but some details were lost !+ !+!+"
.page 
.base 1800


TIME_ERROR		"TIME_ERROR"
TIMPARNOT		"TIME_OF parameters do not form a proper date"
YEANOTRAN		"Computed year is not in the range of subtype YEAR_NUMBER"
DURNOTRAN		"Computed duration is not in the range of the type DURATION"

.base 1900


.severity informational
FILENAME		"The external file specification is:"
SEQXINFO/fao=1		"Position in the file buffer is !UL"
TEXTINFO/fao=1		"Column !UL, Line !UL, Page !UL, Line length is !UL, Page length is !UL"
INDXINFO/fao=1		"Position in the file buffer is !UL"
RELXINFO/fao=1		"The Index is !UL and Position in the file buffer is !UL"
RELAINFO/fao=1		"The Index is !UL"
DIRXINFO/fao=1		"The Index is !UL and Position in the file buffer is !UL"
DIREINFO/fao=1		"The Index is !UL"
MODEINFO/fao=1		"The Mode of the file is !AC"
OPERINFO/fao=1		"While performing !AC!AC"

.base 2000


.severity fatal
NOTIMPTAR		"Operation not implemented in the Ada RTL for this target"
NOTASTLEV/fao=1	"!AC cannot be called at AST level"
PACNUMILL		"Illegal number of AST packets was requested"
TIMERFAIL		"Insufficient quota for call to SYS$SETIMR at delay statement"
ZONECORR		<The "zone" used to implement the collection for the object being allocated or deallocated has been corrupted>
.literal ADA$_CONSTRAINT_ERROR = ADA$_CONSTRAINT_ERRO
