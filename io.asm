	keep	obj/io
	mcopy io.macros
	copy	sh.dp
****************************************************************
*
*  Redirect
*
*  This module implements I/O redirection.
*
****************************************************************
*
*  IOCom - Common data area for I/O redirection
*
****************************************************************
*
IOCom	data
	using Common
;
;  Constants
;
formFeed equ	$C	form feed character code
lineFeed equ	$A	lineFeed character code
dFile	equ	2	direction number for a file
dConsole equ	0	direction number for the console
frBuffSize equ 4096	size of the I/O buffer
;
;  Standard devices
;
devStout dc	i'2'	standard out device
	dc	a4'TextConsole'
devErrout dc	i'2'	error out device
	dc	a4'TextConsoleErr'
devInput dc	i'2'	input device
	dc	a4'TextConsole'
;
;  Standard I/O reference numbers
;
~stinRefnum entry	standard in
	ds	2
~stoutRefnum entry	standard out
	ds	2
~erroutRefnum entry	error out
	ds	2
;
;  Global variables
;
consoleRefnum ds 2	output reference number for .CONSOLE
erroutAppend ds 2	append to error out file?
next_fName ds	4	pointer to first file name record
next_IORec ds	4	pointer to next record in chain
stOut_Append ds 2	append to standard out file?

WstIn_fName ds 4*3	temp save area for file names

allTerms dc	i'$007F,128'	for specifying all chars as terminators
ioLine	ds	255	input line
	ds	1	(used by IOInit)

ch	ds	2	character I/O buffer
;
;  I/O redirection stack frame and work frame
;
fr_disp	ds	2	disp into I/O buffer
fr_last	ds	4	pointer to last file record
fr_next	ds	4	pointer to next file record
fr_ref	ds	2	reference number for the opened file
fr_name	ds	4	file name
fr_output ds	2	is the file opened for output?
fr_use	anop		use count (output only)
fr_maxdisp ds	2	max disp allowed (input only)
fr_buff	anop		location in the record of the I/O buffer

fd_disp	equ	0	displacements into the file record
fd_last	equ	fr_last-fr_disp
fd_next	equ	fr_next-fr_disp
fd_ref	equ	fr_ref-fr_disp
fd_name	equ	fr_name-fr_disp
fd_output equ	fr_output-fr_disp
fd_use	equ	fr_use-fr_disp
fd_maxdisp equ fr_maxdisp-fr_disp
fd_buff	equ	fr_buff-fr_disp

io_next	ds	4	pointer to next record
errOut_direction ds 2	direction of error out
stOut_direction ds 2	direction of standard out
stIn_direction ds 2	direction of standard in
errOut_file ds 4	pointer to errout file record
stOut_file ds	4	pointer to stout file record
stIn_file ds	4	pointer to stin file record
stIn_fName dc	a4'console'	stin file name
stOut_fName dc a4'console'	stout file name
errOut_fName dc a4'console'	errout file name
io_end   anop

io_len	equ	io_end-io_next	length of the I/O record

Wio_next ds	io_len	temp save area for io list
;...............................................................
;
;  The remainder of this data area contains DCBs and records
;  used by the .CONSOLE driver.  All DCBs for the .CONSOLE
;  driver should appear in this section.  The device number is
;  initialized by the console driver, and does not have to be
;  reinitialized or checked by any subroutine using the area.
;  All other named fields should be set before any call is made.
;...............................................................
;
;  DCB for setting the terminator list back to the standards
;
tlDCB	anop		DCB for D_Control to set our terminator list
	dc	i'5'	parameter count
tlNum	ds	2	device number
	dc	i'$8001'	control code
	dc	a4'terminators'	control list ptr
	dc	i4'l:termList+4'	request count
	dc	i4'0'	transfer count


terminators dc i'$807F,l:termList/2'
termList dc	i'$001B,$802E,$000D'	ESC, open-apple ., RETURN
;
;  Used to find the .CONSOLE device
;
scDCB	anop		DCB for D_Control to read the screen char
	dc	i'5'	parameter count
scNum	ds	2	device number
	dc	i'$8004'	control code
	dc	a4'ch'	control list ptr
	dc	i4'1'	request count
	dc	i4'0'	transfer count

ntDCB	anop		DCB for D_Control to set our terminator list
	dc	i'5'	parameter count
ntNum	ds	2	device number
	dc	i'$8001'	control code
	dc	a4'allTerms'	control list ptr
	dc	i4'260'	request count
	dc	i4'0'	transfer count
;
;  Variables used by the .CONSOLE driver for input
;
rdDCB	anop		DCB for D_Read call
	dc	i'6'	parameter count
rdNum	ds	2	device number
rdLine	ds	4	control list ptr
rdLen	ds	4	request count
	dc	i4'0'	start block
	dc	i'0'	block size
rdTrans	ds	4	transfer count
;
;  .CONSOLE Input port values
;
dvDCB	anop		DCB for D_Control call to set/read state
	dc	i'5'	parameter count
dvNum	ds	2	device number
code	ds	2	control code
	dc	a4'list'	control list ptr
	dc	i4'12'	request count
	dc	i4'0'	transfer count

list	dc	c' '	fill character
def_cursor dc	i1'$81'	default cursor mode
cursor_mode dc i1'$81'	current cursor mode
	dc	i1'0'	beep flag
entry_type ds	1	initial entry flag
exit_type ds	1	exit type
	dc	c' '	last char read
	dc	i1'0'	last modifier key read
last_Term dc	i'0'	last terminator read w/modifier
cursor_pos dc	i1'0'	cursor position
	dc	i1'0'	length of the returned string
	dc	i1'0'	input field
	dc	i1'0'	horizontal cursor position
	dc	i'0'	UIR origin.x
	dc	i1'0'	vertical cursor position
;
;  Tells the .CONSOLE driver to do a formatted read
;
frDCB	anop		DCB for D_Control to request UIM read
	dc	i'5'	parameter count
frNum	ds	2	device number
	dc	i'$8003'	control code
	dc	a4'zero'	control list ptr
	dc	i4'2'	request count
	dc	i4'0'

zero	dc	i'0'
	end

****************************************************************
*
*  CloseErrout - Close error out
*
*  Inputs:
*	errOut_direction - direction of output
*	errOut_file - pointer to file record
*
****************************************************************
*
CloseErrout start
	using COMMON
	using IOCOM

	lda	errOut_direction	if output is to a file then
	cmp	#dFile
	bne	lb2
	move4 errOut_file,r0	  decrement use level
	ldy	#fd_use
	lda	[r0],Y
	dec	A
	sta	[r0],Y
	bne	lb1	  if use level = 0 then
	move4 r0,iop1	    purge the buffer
	jsr	Purge
	ldy	#fd_ref	    close the file
	lda	[r0],Y
	sta	clRefnum
	OSClose clRec
	bcc	lb0
	sta	o_error
lb0	brl	DispRec	    deallocate the file record
lb1	anop		  endif
lb2	anop		endif
	rts

clRec	dc	i'1'	OSClose record
clRefnum ds	2
	end

****************************************************************
*
*  CloseStin - Close stin
*
*  Inputs:
*	stIn_direction - direction of input
*	stIn_file - pointer to file record
*
****************************************************************
*
CloseStin start
	using COMMON
	using IOCOM

	lda	stIn_direction	if output is to a file then
	cmp	#dFile
	bne	lb1
	move4 stIn_file,r0	  recover file record pointer
	ldy	#fd_ref	  close the file
	lda	[r0],Y
	sta	clRefnum
	OSClose clRec
	bcc	lb0
	sta	o_error
lb0	brl	DispRec	  deallocate the file record
lb1	anop		endif
	rts

clRec	dc	i'1'	OSClose record
clRefnum ds	2
	end

****************************************************************
*
*  CloseStout - Close stout
*
*  Inputs:
*	stOut_direction - direction of output
*	stOut_file - pointer to file record
*
****************************************************************
*
CloseStout start
	using COMMON
	using IOCOM

	lda	stOut_direction	if output is to a file then
	cmp	#dFile
	bne	lb2
	move4 stOut_file,r0	  decrement use level
	ldy	#fd_use
	lda	[r0],Y
	dec	A
	sta	[r0],Y
	bne	lb1	  if use level = 0 then
	move4 r0,iop1	    purge the buffer
	jsr	Purge
	ldy	#fd_ref	    close the file
	lda	[r0],Y
	sta	clRefnum
	OSClose clRec
	bcc	lb0
	sta	o_error
lb0	brl	DispRec	    deallocate the file record
lb1	anop		  endif
lb2	anop		endif
	rts

clRec	dc	i'1'	OSClose record
clRefnum ds	2
	end

****************************************************************
*
*  DoRedirection - Handle input and output redirection
*
*  Inputs:
*	LINE - command line with I/O redirection info
*
*  Outputs:
*	LINE - command line with I/O redirection stripped
*	C - set if error
*
****************************************************************
*
DoRedirection start
	using COMMON
	using IOCOM

	move	io_next,Wio_next,#io_len save info, in case of error
	move	stIn_fName,WstIn_fName,#l:WstIn_fName
	lda	errOut_direction	save current direction
	sta	old_errOut_direction
	lda	stOut_direction
	sta	old_stOut_direction
	lda	stIn_direction
	sta	old_stIn_direction

	malloc #io_len	allocate a new I/O redirection record
	sta	r0
	stx	r2
	ora	r2
	bne	in1
	lda	#outOfMem
	jsl	SysError
	sec
	rts

in1	ldy	#io_len-2	save the old record
lb0	lda	io_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,lb0
	move4 r0,io_next
	ph4	errOut_fName	duplicate the path names
	jsl	DuplicateOSString
	sta	errOut_fName
	stx	errOut_fName+2
	ph4	stOut_fName
	jsl	DuplicateOSString
	sta	stOut_fName
	stx	stOut_fName+2
	ph4	stIn_fName
	jsl	DuplicateOSString
	sta	stIn_fName
	stx	stIn_fName+2
	jsr	ParseIO	parse the redirection
	jcs	err
	lda	#dFile	set error output direction
	sta	errOut_direction
	sta	stOut_direction
	sta	stIn_direction
	ph4	errOut_fName
	ph4	#console
	jsl	CompareOSStrings
	tax
	bne	lb1
	stz	errOut_direction
lb1	ph4	stOut_fName	set standard output direction
	ph4	#console
	jsl	CompareOSStrings
	tax
	bne	lb3
	stz	stOut_direction
lb3	ph4	stIn_fName	set standard input direction
	ph4	#console
	jsl	CompareOSStrings
	tax
	bne	lb5
	stz	stIn_direction
lb5	lda	#dFile	if errout is to a file then
	cmp	errOut_direction
	bne	lb10
	cmp	stOut_direction	  if stout is to a file then
	bne	lb9
	ph4	errOut_fName	    if they are to the same file then
	ph4	stOut_fName
	jsl	CompareOSStrings
	tax
	bne	lb8
	lda	erroutAppend	      if their append status differs
	cmp	stOut_Append		then
	beq	lb7
	puts	#'Cannot append and redirect to the same file',CR=T,ERROUT=T
	brl	err	      else
lb7	lda	#1		if not APPEND then
	sta	erroutAppend		  mark errout for append
;				endif
;			      endif
lb8	anop		    endif
lb9	anop		  endif
lb10	anop		endif
	lda	stOut_direction	if either direction is a file then
	ora	old_stOut_direction
	beq	lb11
	jsr	RedirectStout	  redirect stout
	move4 stOut_fName,setOut+4
	OSSet_Prefix setOut
	bcc	lb11
	sta	o_error
lb11	anop
	lda	errOut_direction	if either direction is a file then
	ora	old_errOut_direction
	beq	lb12
	jsr	RedirectErrout	  redirect errout
	move4 errOut_fName,setErr+4
	OSSet_Prefix setErr
	bcc	lb12
	sta	o_error
lb12	anop
	lda	stIn_direction	if either direction is a file then
	ora	old_stIn_direction
	beq	lb13
	jsr	RedirectStin	  redirect stin
	move4 stIn_fName,setIn+4
	OSSet_Prefix setIn
	bcc	lb13
	sta	o_error
lb13	anop
	lda	#1	mark output files as append
	sta	stOut_Append	  (allows them to be "reopened")
	sta	erroutAppend
	clc
rts	rts
;
;  Handle an I/O redirection error
;
err	free	io_next
	move	Wio_next,io_next,#io_len
	move	WstIn_fName,stIn_fName,#l:WstIn_fName
	sec
	rts
;
;  Local data
;
setIn	dc	i'2,10',a4'0'
setOut	dc	i'2,11',a4'0'
setErr	dc	i'2,12',a4'0'

old_errOut_direction ds 2	old directions, for time skips
old_stOut_direction  ds 2
old_stIn_direction   ds 2
	end

****************************************************************
*
*  DispRec - Dispose of a file record
*
*  Inputs:
*	r0 - pointer to the record
*
****************************************************************
*
DispRec	private
	using COMMON
	using IOCOM

	ldy	#fd_next	fetch next pointer
	lda	[r0],Y
	sta	r8
	ldy	#fd_next+2
	lda	[r0],Y
	sta	r10
	ldy	#fd_last	if last pointer is nil then
	lda	[r0],Y
	sta	r4
	ldy	#fd_last+2
	lda	[r0],Y
	sta	r6
	ora	r4
	bne	lb1
	move4 r8,next_fName	  next_fName = next pointer
	bra	lb2
lb1	anop		else
	ldy	#fd_next	  last^.next = next
	lda	r8
	sta	[r4],Y
	ldy	#fd_next+2
	lda	r10
	sta	[r4],Y
lb2	anop		endif
	lda	r8	if next <> nil then
	ora	r10
	beq	lb3
	ldy	#fd_last	  next^last = last
	lda	r4
	sta	[r8],Y
	ldy	#fd_last+2
	lda	r6
	sta	[r8],Y
lb3	anop		endif
	free	r0	dispose of record
	rts
	end

****************************************************************
*
*  FileBuff - Check for an existing file buffer
*
*  Inputs:
*	A - 0 for input, 1 for output
*	X - append flag
*	r4 - address of the file name
*
*  Outputs:
*	C - set if name found
*	V - set if there is a clash (output on a file opened for
*		input, input on an open file, or redirect to an
*		open file)
*	r0 - pointer to buffer, if one exists
*
****************************************************************
*
FileBuff private
	using COMMON
	using IOCOM

	sta	output
	stx	append
	move4 next_fName,r8	for each existing file record do
lb1	lda	r8
	ora	r10
	jeq	lb6
	add4	r8,#fd_name,r0	  if names match then
	ph4	[r0]
	ph4	r4
	jsl	CompareOSStrings
	tax
	jne	lb5
	clv
	move4 r8,r0	    set addr of file record
	lda	output	    if file is being opened for input
	bne	lb2	      then
	ldy	#fd_output	      if the file is open for output
	lda	[r8],Y		then
	beq	lb1A
	puts	#'Cannot redirect input from an open file',CR=T,ERROUT=T
	stz	stIn_direction	      fake direction to console
lb1A	sep	#$40	      flag error
	bra	lb4
lb2	ldy	#fd_output	    else if old file open for input
	lda	[r8],Y	      or this is not an append then
	beq	lb3
	lda	append
	bne	lb4
lb3	puts	#'Cannot append to an open file',CR=T,ERROUT=T
	sep	#$40	      flag error
lb4	anop		    endif
	sec		    return
	rts
lb5	anop		  endif
	ldy	#fd_next	next
	lda	[r8],Y
	tax
	ldy	#fd_next+2
	lda	[r8],Y
	sta	r10
	stx	r8
	brl	lb1
lb6	clc
	clv
	rts

output	ds	2	output flag
append	ds	2	append flag
	end

****************************************************************
*
*  FileInit - Common initialization for file records
*
*  Outputs:
*	next_fName - pointer to record's area
*	fr_last - nil
*	fr_next - pointer to next file record
*	fr_use - set to 1
*	C - set if an error occurred
*
****************************************************************
*
FileInit private
	using COMMON
	using IOCOM

	malloc #fd_buff+frBuffSize
	sta	r4
	stx	r6
	ora	r6
	bne	lb1
	la	o_error,outOfMem
	sec
	rts

lb1	stz	fr_last	last pointer is nil
	stz	fr_last+2
	lda	next_fName	next pointer := next_fName
	sta	fr_next	if next pointer is not nil then
	sta	r0
	lda	next_fName+2
	sta	fr_next+2
	sta	r2
	ora	r0
	beq	lb2
	ldy	#fd_last	  set old record's last pointer
 	lda	r4
	sta	[r0],Y
	ldy	#fd_last+2
	lda	r6
	sta	[r0],Y
lb2	anop		endif
	move4 r4,next_fName	set list pointer to new record
	lda	#1	set use count to 1
	sta	fr_use
	clc
	rts
	end

****************************************************************
*
*  InitConsole - Set I/O redirection to standard in/out
*
****************************************************************
*
InitConsole start
	debug InitConsole
	using Common
	using Reccom
	using IOCOM
;
;  Make sure the shell knows what the standard devices are
;
	SetIODevices devStout
;
;  Open the console files
;
	OSSet_Level lvRec	make sure the level is 0
	bcc	lb1
	sta	o_error
lb1	OSClose clRec	close all open files
	bcc	lb2
	sta	o_error
lb2	OSOpen opRec	open .console
	bcc	lb3
	sta	o_error
	brl	rts
lb3	lda	opRefnum	set the console reference number
	sta	consoleRefnum
	sta	~stinRefnum	initialize the I/O reference numbers
	sta	~stoutRefnum
	sta	~erroutRefnum
	sta	srRefnum	set the standard refnums
	lda	#10
	sta	srPrefixNum
	SetStdRefnumGS srRec
	inc	srPrefixNum
	SetStdRefnumGS srRec
	inc	srPrefixNum
	SetStdRefnumGS srRec
;
;  Initialize the text toolkit drivers
;
	ph2	devInput	set input device
	ph4	devInput+2
	_setinputdevice
	bcs	rts
	ph2	devStout	set output device
	ph4	devStout+2
	_setoutputdevice
	bcs	rts
	ph2	devErrout	set the error device
	ph4	devErrout+2
	_seterrordevice
	bcs	rts

	ph2	#$7F	set input globals
	ph2	#$00
	_setinglobals
	bcs	rts
	ph2	#$7F	set output globals
	ph2	#$00
	_setoutglobals
	bcs	rts
	ph2	#$7F	set output globals for error device
	ph2	#$00
	_seterrglobals
	bcs	rts

	ph2	#0	init input device
	_inittextdev
	bcs	rts
	ph2	#1	init output device
	_inittextdev
	bcs	rts
	ph2	#2	init error device
	_inittextdev
	bcs	rts

	clc
rts	rts
;
;  Local data
;
clRec	dc	i'1'	close record for closing all files
	dc	i'0'

lvRec	dc	i'1,0'	set the level to 0

opRec	dc	i'2'	for Open call
opRefNum ds	2
	dc	a4'console'

srRec	dc	i'2'	for SetStdRefnum call
srPrefixnum ds	2
srRefnum	ds	2
	end

****************************************************************
*
*  IOInit - Initialize the I/O redirection module
*
****************************************************************
*
IOInit	start
	using Common
	using IOCom
	debug IOInit
;
;  Get ready for device reads of the .CONSOLE driver
;
	lda	#1	find out which device is the console
	sta	diNum	 driver
in1	~OSD_Info diDCB
	bcs	err
	ldx	dName+2
	cpx	console
	bne	in3
	short I,M
in2	lda	console+1,X
	cmp	dName+3,X
	bne	in3
	dex
	bne	in2
	long	I,M
	bra	in4
in3	long	I,M
	inc	diNum
	bra	in1

in4	lda	diNum	save the device number
	sta	tlNum
	sta	frNum
	sta	ntNum
	sta	rdNum
	sta	dvNum
	sta	scNum

	~OSD_Control tlDCB	use our terminator list
	rts
;
;  Terminal error handler
;
err	pha		could not start the driver: this is bad!
	ph4	str
	_SysFailMgr

str	dw	'Could not start the console device: '
;
;  Local data
;
diDCB	dc	i'2'	for D_Info calls to find the .console device
diNum	ds	2
	dc	a4'dName'

dName	dc	i'35,0',31c' '	buffer for device name
	end

****************************************************************
*
*  OutputInit - Complete initialization of an output file buffer
*
*  Inputs:
*	fr_name - name of the file
*	A - append flag
*
*  Outputs:
*	fr_output - set to 1
*	fr_ref - reference number for open file
*	fr_disp - set to zero bytes into the buffer
*	r0 - pointer to file record
*	C - set if error
*
*  Notes:
*	If an error ocurrs, the file record (which is assumed
*	to be the first one in the list) is disposed of, and
*	all pointers properly patched.
*
****************************************************************
*
OutputInit private
	using COMMON
	using IOCOM
;
;  Initialization common to character and block devices
;
	sta	append	save append flag
	move4 next_fName,r0	set file pointer
	lda	#1	set output flag to true
	sta	fr_output
	la	fr_disp,fd_buff	set disp to 0 bytes into buffer
;
;  Handle character devices
;
	lda	fr_name	set the path name in the file I/O
	ldx	fr_name+2	 records
	sta	giPathname
	sta	opPathName
	sta	crPathname
	stx	giPathname+2
	stx	opPathName+2
	stx	crPathname+2
	OSGet_File_Info giRec	branch if the name is an existing file
	bcc	la1
	cmp	#$58	branch if this is a block device
	bne	la0

	OSOpen opRec	open the file
	jcs	err
	lda	opRefnum	set the reference number
	sta	fr_ref
	bra	cc1
;
;  Handle block devices
;
la0	OSCreate crRec	create a new file (one does not exist)
	bcs	err
la1	OSOpen opRec	open the file
	bcs	err
	lda	opRefnum	set the reference #
	sta	mkRefnum
	sta	efRefnum
	sta	fr_ref
	lda	append	if append then
	beq	la2
	OSGet_EOF efRec	  set mark to EOF
	bcc	la1a
	sta	o_error
la1a	move4 efEOF,mkMark
	OSSet_Mark mkRec
	bcc	la3
	sta	o_error
	bra	la3	else
la2	stz	mkMark	  set EOF to 0
	stz	mkMark+2
	OSSet_EOF mkRec
	bcc	la3
	sta	o_error
la3	anop		endif
;
;  Common code for character and block devices
;
cc1	ldy	#fd_buff-2	move the record to its spot
cc2	lda	fr_disp,Y
	sta	[r0],Y
	dey
	dbpl	Y,cc2
	clc
	rts
;
;  Error handling
;
err	sta	o_error	handle an error
	ldy	#fd_buff-2	move the record to its spot
la5	lda	fr_disp,Y	 so it will dispose properly
	sta	[r0],Y
	dey
	dbpl	Y,la5
	jsr	DispRec	dispose of record
	sec
	rts
;
;  Local data
;
append	ds	2

giRec	dc	i'2'	Get_File_Info record
giPathname ds	4
	ds	2

crRec	dc	i'6'	create record
crPathname ds	4
	dc	I'$C3'
	dc	I'4'
	dc	I4'0'
	dc	I'1'
	dc	2I'0'

opRec	dc	i'2'	Open record
opRefnum ds	2
opPathname ds	4

efRec	dc	i'2'	Get_EOF record
efRefnum ds	2
efEOF	ds	4

mkRec	dc	i'3'	Set_Mark, Set_EOF record
mkRefnum ds	2
	dc	i'0'
mkMark	ds	4
	end

****************************************************************
*
*  ParseIO - Parse I/O redirection
*
*  Inputs:
*	LINE - command line
*
*  Outputs:
*	LINE - command line with I/O redirection stripped
*	C - set if error occurred
*	errOut_fName - file name for error out
*	erroutAppend - append to output file? (replace if false)
*	stOut_ - same variables for standard out
*	STIN - same variables for standard in
*
****************************************************************
*
ParseIO	private
	using COMMON
	using IOCOM
;
;  Initialization
;
	stz	out	initialize redirection flags
	stz	in
	stz	errout
	stz	o_error	initialize redirection error
	short M	reads work best with short A
	ldx	#0	start at beginning of line
;
;  Detect and handle >>& (append to error out)
;
lb1	jsr	NextChar	get next char
	cmp	#'>'	if char = '>' then
	jne	lb5
	jsr	RemoveChar	  remove it
	jsr	NextChar	  get next char
	cmp	#'>'	  if char = '>' then
	jne	lb3
	jsr	RemoveChar	    remove it
	jsr	NextChar	    get next char
	cmp	#'&'	    if char = '&' then
	bne	lb2
	lda	errout	      if errout then
	jne	err1		flag error
	inc	errout	      errout = true
	jsr	RemoveChar	      remove it
	jsr	GetName	      get file name
	jcs	err3
         long	M
	free	errOut_fName                   free the old name
	move4 exPathname,errOut_fName	      set up redirection record
	short	M
	lda	#1
	sta	erroutAppend
	bra	lb1
;
;  handle >> (append to standard out)
;
lb2	lda	out	    if out then
	jne	err1	      flag error
	inc	out	    out = true
	dex
	jsr	GetName	    get file name
	jcs	err3
	long	M
	free	stOut_fName                  free the old name
	move4 exPathname,stout_fName	    set up redirection record
	short	M
	lda	#1
	sta	stout_Append
	brl	lb1
;
;  Detect and handle >& (write to error out)
;
lb3	cmp	#'&'	  if char = '&' then
	bne	lb4
	jsr	RemoveChar	    remove it
	jsr	GetName	    get file name
	jcs	err3
	ldx	errout	    if error then
	jne	err1	      flag error
	inc	errout	    error = true
	long	M
	free	errOut_fName                 free the old name
	stz	erroutAppend	    set up redirection record
	move4 exPathname,errout_fName
	short	M
	brl	lb1
;
;  Handle > (write to standard out)
;
lb4	dex
	jsr	GetName	  get file name
	jcs	err3
	ldx	out	  if out then
	bne	err1	    flag error
	inc	out	  out = true
	long	M
	free	stOut_fName                free the old name
	stz	stout_Append	  set up redirection record
	move4 exPathname,stout_fName
	short	M
	brl	lb1
;
;  Check for and handle < (redirect input)
;
lb5	cmp	#'<'	else if char = '<' then
	bne	lb6
	jsr	RemoveChar	  remove character
	jsr	GetName	  get file name
	jcs	err3
	ldx	in	  if in then
	bne	err1	    flag error
	inc	in	  in = true
	long	M
	free	stIn_fName                 free the old name
	move4 exPathname,stIn_fName	  set up redirection record
	short	M
	brl	lb1
;
;  Next character
;
lb6	cmp	#'"'	skip quoted strings
	bne	lb8
lb7	jsr	NextChar
	cmp	#RETURN
	beq	err2
	cmp	#'"'
	bne	lb7

lb8	cmp	#RETURN	if not at end of line, loop
	jne	lb1
	long	M
	clc
	rts
;
;  Flag multiple redirection error
;
err1	long	M
	puts	#'Multiple redirection not allowed',CR=T,ERROUT=T
	sec
	rts
	longa off

err2	long	M
	puts	#'Missing closing quote',CR=T,ERROUT=T
	sec
	rts
	longa off

err3	long	M
	puts	#'Missing device name',CR=T,ERROUT=T
	sec
	rts
	longa off

;---------------------------------------------------------------
;
;  GetName - read a file name from the command line
;
GetName	long	M
	phx		get a line buffer
	jsr	LineLength
	inc	A
	inc	A
	inc	A
	jsl	Malloc
	sta	r0
	stx	r2
	plx
	ora	r2
	beq	gn4

	ldy	#2	remove leading spaces
	short M
gn1	jsr	NextChar
	cmp	#RETURN
	beq	gn3
	jsl	IsWhite
	bcc	gn2
	jsr	RemoveChar
	bra	gn1

gn2	sta	[r0],Y	save a char in the file name buffer
	iny
	phy
	phx
	long	M
	jsr	RemoveChar	remove it from the command line
	jsr	NextChar	next char
	jsl	IsWhite
	short M
	plx
	ply
	bcs	gn3
	cmp	#RETURN
	bne	gn2
	dex
gn3	long	M	set the length of the file name
	dey
	dey
	tya
	sta	[r0]
	short M
	beq	gn4	branch if there was no name
	phx		expand devices and prefixes
	long	M
	move4 r0,exName
	ph4	#exRec
	jsr	ExpandPath
	tax
	bne	gn4
	free	r0	free the file name buffer
	add4	exPathname,#2	change the string to an input string
	short M	return to caller
	plx
	clc		successful return
	rts

gn4	long	M	error - free the buffer (if any)
	free	r0
	short M
	plx
	sec
	rts
;
;  Local data
;
out	ds	2	output redirected?
in	ds	2	input redirected?
errout	ds	2	error output redirected?

exRec	dc	i'3'	ExpandPath record
exName	ds	4
exPathName ds	4
	dc	i'0'

	longi on
	longa on
	end

****************************************************************
*
*  PopRedirection - Pull the last I/O redirection record
*
*  Inputs:
*	io_next - first entry in current I/O record
*
*  Outputs:
*	io_next - first entry in newly restored I/O record
*	C - clear if an error occurred
*
****************************************************************
*
PopRedirection start
	using COMMON
	using IOCOM

	lda	o_error	if an error was flagged then
	beq	lb0
	error o_error	  print it
	stz	o_error	  zero the error counter
lb0	anop		endif

	lda	io_next	if we are at the command level then
	ora	io_next+2
	bne	lb1
	clc		  cannot pop any more records
	rts

lb1	lda	errOut_direction	save current direction
	sta	old_errOut_direction
	lda	stOut_direction
	sta	old_stOut_direction
	lda	stIn_direction
	sta	old_stIn_direction
	jsr	CloseStout	close stout
	jsr	CloseStin	close stin
	jsr	CloseErrout	close errout
	free	stIn_fName	free the name buffers
	free	stOut_fName
	free	errOut_fName
	move4 io_next,r0	recover the old record
	ldy	#io_len-2
lb2	lda	[r0],Y
	sta	io_next,Y
	dey
	dbpl	Y,lb2
	free	r0	dispose of the record

	lda	stIn_direction	if stInDirection = console then
	bne	si1
	move	consoleRefnum,~stinRefnum  set the console reference number
	lda	old_stIn_direction	  if the old direction was to a file then
	beq	si2
	ph2	devInput	    redirect to text driver
	ph4	devInput+2
	_SetInputDevice
	move4 stIn_fName,setIn+4	    set the prefix
	OSSet_Prefix setIn
	bra	si2	else
si1	move4 stIn_file,r0	  set the file reference number
	ldy	#fd_ref
	lda	[r0],Y
	sta	~stinRefnum
	ph2	#2	  redirect to RAM driver
	ph4	#RAMStIn
	_SetInputDevice
	move4 stIn_fName,setIn+4	  set the prefix
	OSSet_Prefix setIn
si2	anop		endif

	lda	stOut_direction	if stOutDirection = console then
	bne	so1
	move	consoleRefnum,~stoutRefnum set the console reference number
	lda	old_stOut_direction	  if the old direction was to a file then
	beq	so2
	ph2	devStout	    redirect to text driver
	ph4	devStout+2
	_SetOutputDevice
	move4 stOut_fName,setOut+4	    set the prefix
	OSSet_Prefix setOut
	bra	so2	else
so1	move4 stOut_file,r0	  set the file reference number
	ldy	#fd_ref
	lda	[r0],Y
	sta	~stoutRefnum
	ph2	#2	  redirect to RAM driver
	ph4	#RAMStOut
	_SetOutputDevice
	move4 stOut_fName,setOut+4	  set the prefix
	OSSet_Prefix setOut
so2	anop		endif

	lda	errOut_direction	if errOutDirection = console then
	bne	eo1
	move	consoleRefnum,~erroutRefnum set the console reference number
	lda	old_errOut_direction	  if the old direction was to a file then
	beq	eo2
	ph2	devErrout	    redirect to text driver
	ph4	devErrout+2
	_SetErrorDevice
	move4 errOut_fName,setErr+4	    set the prefix
	OSSet_Prefix setErr
	bra	eo2	else
eo1	move4 errOut_file,r0	  set the file reference number
	ldy	#fd_ref
	lda	[r0],Y
	sta	~erroutRefnum
	ph2	#2	  redirect to RAM driver
	ph4	#RAMErrOut
	_SetErrorDevice
	move4 errOut_fName,setErr+4	  set the prefix
	OSSet_Prefix setErr
eo2	anop		endif

	lda	~stinRefnum	set the standard refnums
	sta	srRefnum
	lda	#10
	sta	srPrefixnum
	SetStdRefnumGS srRec
	lda	~stoutRefnum
	sta	srRefnum
	inc	srPrefixnum
	SetStdRefnumGS srRec
	lda	~erroutRefnum
	sta	srRefnum
	inc	srPrefixnum
	SetStdRefnumGS srRec
	sec
	rts
;
;  Local data
;
setIn	dc	i'2,10',a4'0'
setOut	dc	i'2,11',a4'0'
setErr	dc	i'2,12',a4'0'

old_errOut_direction ds 2	old directions, for time skips
old_stOut_direction  ds 2
old_stIn_direction   ds 2

srRec	dc	i'2'	SetStdRefnum record
srPrefixnum ds	2
srRefnum	ds	2
	end

****************************************************************
*
*  Purge - Purge an output buffer
*
*  Inputs:
*	iop1 - addr of file buffer
*
****************************************************************
*
Purge	private
	using Common
	using IOCom

	add4	iop1,#fd_buff,wr_buff	set the write location
	ldy	#fd_ref	set the file reference number
	lda	[iop1],Y
	sta	wr_ref
	sec		set the request count
	lda	[iop1]
	sbc	#fd_buff
	beq	rts
	sta	wr_req
	write wr_DCB	write the info
	bcc	lb1
	sta	o_error
lb1	lda	#fd_buff	set the disp to the start of the buffer
	sta	[iop1]
rts	rts

wr_DCB	anop		write DCB
wr_ref	ds	2
wr_buff	ds	4
wr_req	dc	I4'0'
	ds	4
	end

****************************************************************
*
*  PurgeAll - purge all of our output buffers
*
****************************************************************
*
PurgeAll start
	debug PurgeAll
	using IOCom

	lda	stOut_Direction	if output is to a file then
	beq	lb1
	move4 stOut_file,iop1	  purge it
	jsr	Purge
lb1	lda	errOut_Direction	if errout is to a file then
	beq	lb2
	move4 errOut_file,iop1	  purge it
	jsr	Purge
lb2	rts
	end

****************************************************************
*
*  RAMErrOut- RAM driver for errout
*
****************************************************************
*
RAMErrOut private
	longi on
	longa on
	using COMMON
	using IOCOM
;
;  Entry points
;
	brl	rts
	brl	rts
	brl	write
	brl	rts
	brl	rts
;
;  Write a character
;
write	phb		save user data bank
	phk		set local data bank
	plb
	phd		save his DP
	sta	ch	save the character
	lda	SH_MY_DP	get mine
	tcd
	phx		save his X, Y
	phy
	lda	ch	quit if this is a line feed
	and	#$007F
	cmp	#lineFeed
	beq	wr2
	move4 errOut_file,iop1	get addr of file record
	lda	[iop1]	if disp >= maxbuff then
	cmp	#frBuffSize+fd_buff
	blt	wr1
	jsr	Purge	  purge the old buffer
wr1	anop		endif
	lda	[iop1]	save the character
	tay
	inc	A
	sta	[iop1]
	short M
	lda	ch
	sta	[iop1],Y
	long	M
wr2	ply
	plx
	pld
	plb
rts	rtl
	end

****************************************************************
*
*  RAMStIn - RAM driver for standard in
*
*  Note:
*	Entry at GetRAMCh can be used by shell subroutines
*	to avoid the overhead of a tool call when it is
*	known that the input has been redirected.  Call
*	GetRAMCh with a JSL; the result is returned in A,
*	with X and Y preserved.  You must call with long
*	registers.
*
****************************************************************
*
RAMStIn	private
	longi on
	longa on
	using COMMON
	using IOCOM
;
;  Entry points
;
	brl	rts
	brl	read
	brl	rts
	brl	rts
	brl	rts
;
;  Read a character
;
GetRAMCh entry
read	phb		save user data bank
	phk		set local data bank
	plb
	phd		save his DP
	lda	SH_MY_DP	get mine
	tcd
	phx		save his X, Y
	phy
	move4 stIn_file,iop1	get addr of file record
rd1	ldy	#fd_maxdisp	if disp >= maxbuff then
	lda	[iop1],Y
	sta	R4
	lda	[iop1]
	cmp	r4
	blt	rd3
	lda	r4	  if maxbuff = frBuffSize then
	cmp	#fd_buff+frBuffSize
	bne	rd2
	jsr	ReadBuffer	    read next buffer
	bra	rd1	    try again
rd2	anop		  endif
	lda	#0	  return 0 (end of file)
	bra	rd4	else
rd3	tay		  return [disp++]
	inc	A
	sta	[iop1]
	lda	[iop1],Y
	and	#$00FF
rd4	anop		endif
	ply
	plx
	pld
	plb
rts	rtl
	end

****************************************************************
*
*  RAMStOut - RAM driver for stout
*
*  Note:
*	Entry at PutRAMCh can be used by shell subroutines
*	to avoid the overhead of a tool call when it is
*	known that the output has been redirected.  Call
*	PutRAMCh with a JSL and the character to write in A;
*	the subroutine will return with X and Y preserved.
*	You must call with long registers.
*
****************************************************************
*
RAMStOut private
	longi on
	longa on
	using COMMON
	using IOCOM
;
;  Entry points
;
	brl	rts
	brl	rts
	brl	write
	brl	rts
	brl	rts
;
;  Write a character
;
PutRAMCh entry
write	phb		save user data bank
	phk		set local data bank
	plb
	phd		save his DP
	sta	ch	save the character
	lda	SH_MY_DP	get mine
	tcd
	phx		save his X, Y
	phy
	lda	ch	quit if this is a line feed
	and	#$007F
	cmp	#lineFeed
	beq	wr2
	move4 stOut_file,iop1	get addr of file record
	lda	[iop1]	if disp >= maxbuff then
	cmp	#frBuffSize+fd_buff
	blt	wr1
	jsr	Purge	  purge the old buffer
wr1	anop		endif
	lda	[iop1]	save the character
	tay
	inc	A
	sta	[iop1]
	short M
	lda	ch
	sta	[iop1],Y
	long	M
wr2	ply
	plx
	pld
	plb
rts	rtl

ch	ds	2	character
	end

****************************************************************
*
*  ReadBuffer - Read the next input buffer
*
*  Inputs:
*	r0 - pointer to input file record
*
****************************************************************
*
ReadBuffer private
	using COMMON
	using IOCOM

	ldy	#fd_ref	set the ref # for a read
	lda	[iop1],Y
	sta	rdRefnum
	add4	iop1,#fd_buff,rdBuff	set the read address
	OSRead rdRec	read the block
	bcs	err
	ldy	#fd_maxdisp	set the buffer size
	clc
	lda	rdTrans
	adc	#fd_buff
	sta	[iop1],Y
	lda	#fd_buff	set initial displacement
	sta	[iop1]
	rts

err	sta	o_error	set the error #
	lda	#0	set max disp to 0
	ldy	#fd_maxdisp
	sta	[iop1],Y
	rts

rdRec	dc	i'4'	READ DCB
rdRefnum ds	2
rdBuff	ds	4
	dc	I4'frBuffSize'
rdTrans	ds	4
	end

****************************************************************
*
*  RedirectErrout - Redirect error output
*
*  Inputs:
*	errOut_direction - direction to redirect error out to
*	erroutAppend - append to existing file?
*
*  Outputs:
*	C - set if error
*	errOut_file - if the direction is a file, this points to
*		the file record
*
****************************************************************
*
RedirectErrout start
	using COMMON
	using IOCOM

	lda	errOut_direction	if the device is the console then
	bne	lb3
	lda	consoleRefnum	  set the reference number
	sta	~erroutRefnum
	sta	srRefnum
	SetStdRefnumGS srRec
	ph2	devErrout	  redirect to the console
	ph4	devErrout+2
	_SetErrorDevice
	clc		  return
	rts

lb3	move4 errOut_fName,r4	else if a file record exists then
	lda	#1
	ldx	erroutAppend
	jsr	FileBuff
	bvs	rts	  (branch if files clash)
	bcc	cr1
	ldy	#fd_use	  inc use count
	lda	[r0],Y
	inc	A
	sta	[r0],Y
	bra	cr2	endif
cr1	jsr	FileInit	do common file buffer initialization
	bcs	rts
	move4 errOut_fName,fr_name	move in file name
	lda	erroutAppend	do common output buffer initialization
	jsr	OutputInit
	bcs	rts
cr2	move4 r0,errOut_file	set file pointer
	ldy	#fd_ref	set the file reference number
	lda	[r0],Y
	sta	~erroutRefnum
	sta	srRefnum	set the standard reference number
	SetStdRefnumGS srRec
	ph2	#2	set the output device
	ph4	#RAMErrOut
	_SetErrorDevice
	clc
rts	rts

srRec	dc	i'2'	SetStdRefnum record
	dc	i'12'
srRefnum	ds	2
	end

****************************************************************
*
*  RedirectStin - Redirect standard in
*
*  Inputs:
*	stIn_direction - direction to redirect from
*
*  Outputs:
*	C - set if error
*	stIn_file - if the direction is a file, this points to
*		the file record
*
****************************************************************
*
RedirectStin start
	using COMMON
	using IOCOM

	lda	stIn_direction	if stIn_direction = 0 then
	bne	lb4
	lda	consoleRefnum	  set the reference number
	sta	~stinRefnum
	sta	srRefnum	  set the standard reference number
	SetStdRefnumGS srRec
	ph2	devInput	  redirect input from the console
	ph4	devInput+2
	_SetInputDevice
	rts

lb4	move4 stIn_fName,opPathname	else
	OSOpen opRec	  open the file
	jcs	err
	move4 stIn_fName,r4
	lda	#0	  if a file record exists then
	jsr	FileBuff
	bvs	rts	    error
	bcs	rts
	jsr	FileInit	  do common file buffer initialization
	bcs	rts
	move4 stIn_fName,fr_name	  move in file name
	stz	fr_output	  mark file for input only
	lda	opRefnum	  save reference number
	sta	fr_ref
	sta	srRefnum	  set the standard reference number
	SetStdRefnumGS srRec
	move4 next_fName,stIn_file	  set file pointer
	move4 next_fName,r0	  move the record to its spot
	ldy	#fd_buff-2
lb1	lda	fr_disp,Y
	sta	[r0],Y
	dey
	dbpl	Y,lb1
	ldy	#fd_ref	  set the file reference number
	lda	[r0],Y
	sta	~stinRefnum
	ph2	#2	  set the input device
	ph4	#RAMStIn
	_SetInputDevice
	move4 r0,iop1	  set the buffer address
	ldy	#fd_maxdisp	  input buffer is empty
	lda	#fd_buff+frBuffSize
	sta	[r0]
	sta	[r0],Y
	clc
rts	rts		endif

err	sta	o_error	save error code
	stz	stIn_direction	error opening file: switch to stin
	jsr	RedirectStin	redirect to console
	sec
	rts

opRec	dc	i'3'	open record
opRefnum ds	2
opPathname ds	4
	dc	i'$0001'

srRec	dc	i'2'	SetStdRefnum record
	dc	i'10'
srRefnum	ds	2
	end

****************************************************************
*
*  RedirectStout - Redirect standard output
*
*  Inputs:
*	stOut_direction - direction to redirect standard out to
*	stOut_Append - append to existing file?
*
*  Outputs:
*	C - set if error
*	stOut_file - if the direction is a file, this points to
*		the file record
*
****************************************************************
*
RedirectStout start
	using COMMON
	using IOCOM

	lda	stOut_direction	if the device is the console then
	bne	lb3
	lda	consoleRefnum	  set the reference number
	sta	~stoutRefnum
	sta	srRefnum	  set the standard reference number
	SetStdRefnumGS srRec
	ph2	devStout	  redirect to the console
	ph4	devStout+2
	_SetOutputDevice
	clc		  return
	rts

lb3	move4 stOut_fName,r4	if a file record exists then
	lda	#1
	ldx	stOut_Append
	jsr	FileBuff
	bvs	rts	  (branch if files clash)
	bcc	cr1
	ldy	#fd_use	  inc use count
	lda	[r0],Y
	inc	A
	sta	[r0],Y
	bra	cr2	endif
cr1	jsr	FileInit	do common file buffer initialization
	bcs	rts
	move4	stOut_fName,fr_name	move in file name
	lda	stOut_Append	do common output buffer initialization
	jsr	OutputInit
	bcs	rts
cr2	move4 r0,stOut_file	set file pointer
	ldy	#fd_ref	set the file reference number
	lda	[r0],Y
	sta	~stoutRefnum
	sta	srRefnum	set the standard reference number
	SetStdRefnumGS srRec
	ph2	#2	set the output device
	ph4	#RAMStOut
	_SetOutputDevice
	clc
rts	rts

srRec	dc	i'2'	SetStdRefnum record
	dc	i'11'
srRefnum	ds	2
	end

****************************************************************
*
*  TextConsole - stdout and stdin driver for the text toolkit
*
****************************************************************
*
TextConsole private
	using COMMON
	using IOCOM
;
;  Entry points
;
	brl	rts
	brl	read
	brl	write
	brl	rts
	brl	rts
;
;  Read a character
;
Read	phb		save user data bank
	phk		set local data bank
	plb
	phx		save his X, Y
	phy

	~OSD_Status scDCB	get the screen character
	lda	#$8001	get the current input port values
	sta	code
	~OSD_Status dvDCB
	short M	use the screen char as the fill char
	lda	ch
	and	#$7F
	sta	list
	long	M	set the parameters
	dec	code
	~OSD_Control dvDCB

	ldy	#254	all characters are terminators
	lda	#127
rd1	sta	ioLine,Y
	dey
	dey
	dec	A
	bpl	rd1
	~OSD_Control ntDCB
	~OSD_Control frDCB	do a formatted read
	stz	entry_type	initial entry
	lla	rdLine,ch	read a character
	lla	rdLen,1
	~OSD_Read rdDCB
	~OSD_Control tlDCB	restore standard terminators
	lda	#$8001	get the input port values
	sta	code
	~OSD_Status dvDCB
	lda	last_term	return the character
	and	#$00FF

	ply		restore regs & quit
	plx
	plb
rts	rtl
;
;  Write a character to stdout
;
write	and	#$00FF	save the character
	jsl	~stdout
	rtl
	end

****************************************************************
*
*  TextConsoleErr - errout driver for the text toolkit
*
****************************************************************
*
TextConsoleErr private
	using COMMON
	using IOCOM
;
;  Entry points
;
	brl	rts
	brl	rts
	brl	write
	brl	rts
	brl	rts
;
;  Write a character to errout
;
write	and	#$00FF	save the character
	jsl	~errout
rts	rtl
	end

****************************************************************
*
*  ~PutS - String Output
*
*  Inputs:
*	adr - address of string to write
*	F1 - field width
*	CR - carriage return flag
*	err - error output flag
*
****************************************************************
*
~PutS	start
	using Common
adr	equ	10	address of string to write
F1	equ	8	field width
CR	equ	6	carriage return flag
err	equ	4	error output flag

	tsc		set up DP
	phd
	tcd
	phb		establish local addressing
	phk
	plb
	inc4	adr
	lda	[adr]	A = - # lead blanks
	and	#$00FF
	sec
	sbc	F1
	bpl	lb1	if A < 0 then
	eor	#$FFFF	  A = -A
	inc	A
	ldy	err	  print A blanks
	jsl	~PRBL
lb1	anop		endif
	lda	[adr]	write the string
	and	#$00FF
	tax
	beq	lb4
	ldy	#1
	lda	err
	bne	lb3
lb2	lda	[adr],Y
	and	#$00FF
	phy
	phx
	jsl	~stdout
	plx
	ply
	iny
	dex
	bne	lb2
	bra	lb4
lb3	lda	[adr],Y
	and	#$00FF
	phy
	phx
	jsl	~errout
	plx
	ply
	iny
	dex
	bne	lb3

lb4	lda	CR	if CR then
	beq	lb6
	lda	#RETURN
	ldx	err
	bne	lb5
	jsl	~stdout
	bra	lb6
lb5	jsl	~errout

lb6	move4 0,adr	patch return addr
	plb		restore caller's B
	pld		fix DP
	clc		remove extra stack space
	tsc
	adc	#10
	tcs
	rtl
	end

****************************************************************
*
*  ~stdin - Keyboard Input
*
*  This subroutine replaces a library routine to avoid overhead.
*  This routine hooks directly into the standard I/O facility,
*  while the library routine must do a great deal of work on
*  its own, since it cannot assume that the shell is present.
*
*  Outputs:
*	A - character read
*
*  Notes:
*	Entry at ~crpb puts a character back.
*
****************************************************************
*
~stdin	start
	using IOCom
	using Common
;
;  read a character
;
	lda	putBack	if there is a character in the putback
	jne	lb1	 buffer, return it

	ldx	disp	get the index into the line
	cpx	lineLen
	jlt	rd2	branch if there are characters left
;
;  None left - read a line from redirected input
;
	lda	stIn_direction	branch if input is not redirected
	cmp	#dConsole
	beq	la0
	stz	lineLen	no characters read, yet
ri1	jsl	GetRAMCh	get a character
	tax
	jeq	rd1	quit if at EOF
	sta	ch	echo the character to stdout
	putc	ch
	cmp	#RETURN	quit if at EOLN
	jeq	rd1
	short I,M	save it in the input buffer
	ldx	lineLen
	sta	ioLine,X
	inc	lineLen
	long	I,M
	bra	ri1	next char
;
;  Input is not redirected - read a line from the console driver
;
la0	lda	#$8001	get the current input port values
	sta	code
	OSD_Status dvDCB
	bcc	la1
	sta	o_error
la1	short M	use a blank fill
	lda	#' '
	sta	list
	stz	cursor_pos	don't set a default cursor
	lda	list+2	use overstrike, disable control charactes
	ora	#$01
	and	#$BF
	sta	list+1
	sta	list+2
	stz	entry_type	initial entry
	long	M	set the parameters
	lda	#$8000
	sta	code
	OSD_Control dvDCB
	bcc	la2
	sta	o_error
la2	OSD_Control frDCB	do a formatted read
	bcc	la3
	sta	o_error
la3	lla	rdLine,ioLine	read a line
	lla	rdLen,l:ioLine-1
	OSD_Read rdDCB
	bcc	la4
	sta	o_error
la4	lda	#13	write a CR
	jsl	~stdout
	lda	rdTrans	set the line length
	sta	lineLen

	lda	#$8001	if the termination was abnormal then
	sta	code
	OSD_Status dvDCB
	bcc	la5
	sta	o_error
la5	lda	last_Term
	cmp	#$000D
	beq	rd1
	stz	lineLen	  dump the line
	SetStopFlag flDCB
;
;  Finish up line input
;
rd1	short I,M	place a terminating CR at the end
	ldx	lineLen
	lda	#$0D
	sta	ioLine,X
	inc	lineLen
	long	I,M
	stz	disp	no characters read, yet
	ldx	#0

rd2	lda	ioLine,X	get the character
	inc	disp	update the line displacement

lb1	stz	putBack	clear the putback buffer
	and	#$007F	make sure the character is ASCII
	rtl
;
;  Put back a character
;
~stdpb	entry		put back a character
	sta	putBack
	rtl
;
;  Local data
;
ch	ds	2	temp storage for a character
disp	ds	2	disp in input line
lineLen	ds	2	length of the input line
putBack	ds	2	putback buffer

flDCB	dc	i'1'	for SetStopFlag call
	end

****************************************************************
*
*  ~stdout - write a character to standard out
*
*  Inputs:
*	A - character to write
*
****************************************************************
*
~stdout	start
	using IOCom
	using Common

	php		save the regs
	long	I,M
	phb
	phk
	plb
	pha
	phx
	phy
	sta	ch
	cmp	#lineFeed	quit if this is a line feed
	beq	wr2
	ldx	stOut_direction	if output is redirected then
	cpx	#dConsole
	beq	wr1
	jsl	PutRAMCh	  put a character to RAMout
	bra	wr2	else
wr1	lda	~stoutRefnum	  write the character
	sta	wrRefnum
	OSWrite wrRec
	bcc	wr2
	sta	o_error
wr2	ply		restore regs and return
	plx
	pla
	plb
	plp
	rtl
;
;  Local data
;
wrRec	dc	i'4'	write DCB
wrRefnum ds	2
	dc	a4'ch'
	dc	i4'1'
	ds	4

ch	ds	2	character
	end

****************************************************************
*
*  ~errout - write a character to error out
*
*  Inputs:
*	A - character to write
*
****************************************************************
*
~errout	start
	using IOCom
	using Common

	php		save the regs
	long	I,M
	pha
	phx
	phy
	sta	>ch
	cmp	#lineFeed	quit if this is a line feed
	beq	wr2
	lda	>~erroutRefnum	write the character
	sta	>wrRefnum
	OSWrite wrRec
	bcc	wr2
	sta	o_error
wr2	ply		restore regs and return
	plx
	pla
	plp
	rtl
;
;  Local data
;
wrRec	dc	i'4'	write DCB
wrRefnum ds	2
	dc	a4'ch'
	dc	i4'1'
	ds	4
	end
