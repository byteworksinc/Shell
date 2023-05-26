	keep	obj/call
	mcopy call.macros
	copy	sh.dp
****************************************************************
*
*  ShellCall - Shell Command Dispatcher
*
*  Inputs:
*	TOS - command code
*	TOS-2 - address of DCB
*
****************************************************************
*
ShellCall start
	debug ShellCall
	using Common
num_calls equ	$1F	number of calls
;
;  See if the call belongs to us - if so, high byte is $01.
;
	phb		save user's data bank
	php
	sta	>la	save user's A (for QUIT call only)
	long  I		   use long regs
	short M	make sure data bank matches program bank
	lda	5,s
	pha
	plb
	long	M
	phy		save his Y
	ldy	#1	get the call number
	lda	(5,S),Y
	cmp	#$0029	branch if QUIT call
	jeq	Quit
	cmp	#$2029	branch if QUIT call
	jeq	OSQuit
	cmp	#$2027	branch if OSGetName call
	jeq	OSGetName
;
;  Any calls that can change a file must be intercepted and checked against
;  the files in the FastFile system.  If any match, the file is removed from
;  the FastFile system.
;
	and	#$DFFF	if the call is DESTROY, CHANGE_PATH,
	cmp	#$0002	 SET_FILE_INF0, or OPEN, wipe out any
	beq	dp1	 files in FASTFILES list
	cmp	#$0004
	beq	dp1
	cmp	#$0005
	beq	dp1
	cmp	#$0010
	jne	dp3
	lda	#2
	bra	dp2
dp1	lda	#0
dp2	phx		see if this is a class 1 call
	pha
	ldy	#1
	lda	(9,S),Y
	and	#$2000
	sta	>isClass1
	pla
	clc		get addr of the call
	ldy	#3
	adc	(7,S),Y
	tax
	ldy	#5
	lda	(7,S),Y
	adc	#0
	tay
	lda	>isClass1	if this is a class 1 call, add 2 bytes
	beq	pc2	 to account for the pCount field
	inx
	bne	pc1
	iny
pc1	inx
	bne	pc2
	iny
pc2	phd		set up our DP
	phb		set up local addressing
	phk
	plb
	lda	sh_my_dp
	tcd
	ph4	r8	get some work space
	ph4	r4
	ph4	r0
	sty	r6	save the addr of the file name ptr
	stx	r4
	lda	filelist	skip if nothing in file list
	ora	filelist+2
	jeq	ff5
	ldy	#2	get the file name ptr
	lda	[r4]
	sta	r0
	lda	[r4],Y
	sta	r2
	stz	tp1	tp1 = nil
	stz	tp1+2
	lda	isClass1	if this is a class 0 call then
	bne	pc3
	ph4	r0	  convert the name to an OS string
	jsl	PtoOSString
	sta	tp1
	sta	r0
	stx	tp1+2
	stx	r2
	ora	r2
	jeq	ff5
pc3	move4 r0,epIn	expand the path name
	ph4	#epRec
	jsr	ExpandPath
	free	tp1	if we allocated a buffer, trash it
	add4	epPathname,#2,r0	get a file pointer
	move4 filelist,r4	get initial fastfile pointer
ff1	lda	r4	while there are files to check do
	ora	r6
	beq	ff5
	ldy	#18	  get the address of the path name
	lda	[r4],Y
	pha
	ldy	#16
	lda	[r4],Y
	pha
	ph4	r0	  if the file names match then
	jsl	CompareOSStrings
	tay
	bne	ff3
	ldy	#10	    if the file may be purged then
	lda	[r4],Y
	and	#$4000
	beq	ff5
	ldy	#14	      dispose of the handle
	lda	[r4],Y
	pha
	lda	#0
	sta	[r4],Y
	ldy	#12
	lda	[r4],Y
	pha
	lda	#0
	sta	[r4],Y
	_DisposeHandle
	bra	ff5	      exit while loop
ff3	long	M
ff4	ldy	#2	next file...
	lda	[r4]
	tax
	lda	[r4],Y
	sta	r6
	stx	r4
	bra	ff1
ff5	pl4	r0	restore regs, DP
	pl4	r4
	pl4	r8
	free	epPathname
	plb
	pld
	plx
	lda	#0	make sure we make the call
;
;  Handle a call to us
;
dp3	ply
	and	#$FF00	branch if is a call to ORCA/HOST
	cmp	#$0100
	jeq	oh1
	plp		patch things up
	plb
ProDOS	entry		pass the call to ProDOS
	ds	4	(JML to ProDOS placed here by TrapProDOS)
;
;  Handle a ProDOS QUIT call
;
OSQuit	sta	>quitNum
	lda	#$FFFF
	sta	>quitAnd
	sec
	bra	os1
Quit	sta	>quitNum
	lda	#$00FF
	sta	>quitAnd
	clc
os1	ldy	#5	get the dcb address
	lda	(5,S),Y
	tax
	dey
	dey
	lda	(5,S),Y
	phx
	pha
	phd		set up DP addressing to it
	tsc
	tcd
	bcc	os2	if this is an OSQuit then
	lda	[3]	  if there are no parms then
	beq	qt1	    quit now
	inc	3	  skip the pCount field
	inc	3
os2	ldy	#2	if there is a file name, quit to it
	lda	[3]
	ora	[3],Y
	beq	qt1
	lda	[3],Y
	tax
	lda	[3]
	tay
	sta	3
	stx	5
	lda	[3]
	and	>quitAnd
	beq	qt1
	phk		set up our data bank, DP
	plb
	lda	sh_my_dp
	tcd
	sty	r0	save addr of file name
	stx	r2
	lda	quitNum	if this is a class 1 quit then
	cmp	#$2029
	bne	qq2
	ldy	#254	  save the GS/OS path name
qq1	lda	[r0],Y
	sta	buff1,Y
	dey
	dey
	bpl	qq1
	bra	qq4	else
qq2	ldy	#64	  move the ProDOS path name
qq3	lda	[r0],Y
	sta	buff1+1,Y
	dey
	dey
	bpl	qq3
	lda	buff1+1	  convert to a class 1 name
	and	#$00FF
	sta	buff1
qq4	anop		endif
	jsl	ShutDownApp	shut down the active application
	lda	user_call_stack	restore our stack
	tcs
	lda	#buff1	quit to the file
	ldx	#^buff1
	jmp	sys16_2

qt1	pld
	pla
	pla
	lda	>user_call_stack
	tcs
	lda	>la
	rtl
;
;  Handle a ProDOS OSGetName call
;
OSGetName anop
	ldy	#5	get the dcb address
	lda	(5,S),Y
	tax
	dey
	dey
	lda	(5,S),Y
	phx
	pha
	phd		set up DP addressing to it
	tsc
	tcd
	phk		use local addressing
	plb
	lda	[3]	check the pCount field
	dec	A
	bne	ge1
	ldy	#4	get the pathname address
	lda	[3],Y
	tax
	dey
	dey
	lda	[3],Y
	sta	3
	stx	5
	ora	3
	beq	ge2
	clc		check the output buffer size
	lda	appName
	adc	#3
	cmp	[3]
	bge	ge3
	ldy	appName	move the name to the output buffer
	iny
	tyx
	iny
	iny
	short M
gn1	lda	appName,X
	sta	[3],Y
	dey
	dex
	bpl	gn1
	long	M
	stz	err	no error

gn2	add4	11,#6	return to caller
	pld
	pla
	pla
	ply
	plp
	plb
	clc
	lda	>err
	beq	gn3
	sec
gn3	rtl

ge1	lda	#4	parameter count out of range
	bne	ge4
ge2	lda	#$53	invalid parameter
	bne	ge4
ge3	lda	#$4F	buffer too small
ge4	sta	err
	bra	gn2

err	ds	2	error code
;
;  Call the ORCA/HOST handler.
;
oh1	phx		save the caller's registers
	phy		  (B has already been pushed)
	phd
	phk		set data bank to our program bank
	plb
	lda	sh_my_dp	switch to our direct page
	tcd
	cld		enforce sanity
	move4 r0,lr0	save r0
	clc		get & update return addr
	lda	9,s
	sta	r0
	adc	#6
	sta	9,s
	lda	11,s
	sta	r2
	adc	#0
	sta	11,s
	ldy	#1	get call number
	lda	[r0],Y
	sta	sh_fnum
	iny		get addr of dcb
	iny
	lda	[r0],Y
	sta	sh_dadr
	iny
	lda	[r0],Y
	sta	sh_dadr+1
	lda	lr0	restore r0
	sta	r0
	lda	lr2
	sta	r2
	ldy	#max_dcb-2	copy over DCB to local area
oh2	lda	[sh_dadr],Y
	tyx
	sta	sh_dcb,X
	dey
	dbpl	Y,oh2
	lda	sh_fnum	get address of command handler
	and	#$003F
	cmp	#num_calls
	bgt	bad
	asl	A
	tax
	jmp	(shellcom-2,X)	handle the command
;
;  Return with bad command number error
;
bad	return #1	ProDOS Invalid call number
;
;  Local data
;
ch	ds	2	uppercase character buffer
isClass1 ds	2	is this a class 1 call?
quitAnd	ds	2	Quit call AND mask
quitNum	ds	2	Quit call number

shellcom anop
	dc	a'GetLInfo'	get linfo
	dc	a'SetLInfo'	set linfo
	dc	a'GetLanguage'	get language
	dc	a'SetLanguage'	set language
	dc	a'Error'	error handler
	dc	a'SetVariable'	set a variable
	dc	a'Version'	version number
	dc	a'ReadIndexed'	indexed read of a variable
	dc	a'InitWildcard'	initialize wildcard
	dc	a'NextWildcard'	next wildcard
	dc	a'ReadVariable'	read a variable
	dc	a'ChangeVector'	change a vector
	dc	a'Execute'	execute
	dc	a'FastFile'	fast file handler
	dc	a'Direction'	direction of I/O
	dc	a'Redirect'	redirect I/O
	dc	a'BAD'
	dc	a'BAD'
	dc	a'Stop'	stop
	dc	a'ExpDevice'	expand devices
	dc	a'Unset'	unset a variable
	dc	a'Export'	export a variable
	dc	a'PopVariables'	pop a variable table
	dc	a'PushVariables'	push a variable table
	dc	a'SetStop'	set the stop flag
	dc	a'CharOut'	pascal console output
	dc	a'SetDevices'	set console devices
	dc	a'GetDevices'	get console devices
	dc	a'GetCommand'	get a command table entry
	dc	a'KeyPress'	see if a keypress is available
	dc	a'ReadKey'	read the next available key

la	ds	2	A reg
lr0	ds	2	local copies of r0, r2
lr2	ds	2

tp1	ds	4	temp pointer
epRec	dc	i'3'	expand path record
epIn	ds	4
epPathname ds	4
	dc	i'0'
	end

****************************************************************
*
*  StackShellCall - Stack Based Shell Command Dispatcher
*
****************************************************************
*
StackShellCall start
	debug StackShellCall
	using Common

	php		save the reg sizes
	long	I,M	use long registers
	lda	5,S
	and	#$FF00	if this call should be intercepted then
	cmp	#$0100
	beq	lb3
	cmp	#$2000
	bne	lb1
	lda	5,S
	and	#$00FF
	bra	lb2
lb1	lda	5,S
lb2	cmp	#$0029
	beq	lb3
	cmp	#$0002
	beq	lb3
	cmp	#$0004
	beq	lb3
	cmp	#$0005
	beq	lb3
	cmp	#$0027
	beq	lb3
	cmp	#$0010
	bne	lb4
lb3	plx		  pull P & the return addr from the stack
	ply
	pla		  set up an in-line call
	sta	>callNum
	pla
	sta	>rec
	pla
	sta	>rec+2
	phy		  restore P & the address
	phx
	plp		  switch back to the caller's size
	jsl	ShellCall	  go handle the intercepted call
callNum	ds	2	  (call number)
rec	ds	4	  (ptr to call record)
	rtl		  return to the caller

lb4	plp		let GS/OS do it
StackProDOS entry	ProDOS stack entry point
	ds	4	(JML to ProDOS placed here by TramProDOS)
	end

****************************************************************
*
*  ChangeVector - Change one of the user modifiable vectors
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
*  DCB:
*       Disp	 Length  Name    Use
*       ====	 ======  ====    ===
*       0 	 2       RESERVED for future expansion of vector types
*       2 	 2       VECTOR  number of the vector to change
*       4 	 4       PROCPTR pointer to the new location
*       8 	 4       OLDPROCPTR old value, so user can change back
*
****************************************************************
*
ChangeVector private
	using Common
	using reccom
d_reserved equ 0	disp to reserved field
d_vector equ	2	disp to vector number
d_procptr equ	4	disp to proc ptr
d_oldprocptr equ 8	disp to old proc ptr field

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb3
	lda	sh_dcb	  if pCount <> 4 then
	cmp	#4
	beq	lb1
	return #4	    return invalid pCount error
lb1	ldx	#0	  change the call to a class 0 call
	ldy	#6
lb2	lda	sh_dcb+2,X
	sta	sh_dcb,X
	inx
	inx
	dey
	bne	lb2
	add4	sh_dadr,#2

lb3	lda	sh_dcb+d_vector	compute a disp into the vector table
	asl	a
	asl	a
	tax
	lda	edit_vector+3,X	save the old vector
	and	#$00FF
	ldy	#d_oldprocptr+2
	sta	[sh_dadr],Y
	dey
	dey
	lda	edit_vector+1,X
	sta	[sh_dadr],Y
	lda	sh_dcb+d_procptr	set the new vector
	sta	edit_vector+1,X
	lda	sh_dcb+d_procptr+1
	sta	edit_vector+2,X
	return #0
	end

****************************************************************
*
*  CharOut - Console Output Routine
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
CharOut	start
	using Common
	using IOCom
d_char	equ	0	disp to character

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb2
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error
lb1	lda	sh_dcb+2	  change the call to a class 0 call
	sta	sh_dcb

lb2	lda	sh_dcb+d_char	skip cursor on/off
	and	#$00FF
	cmp	#5
	beq	no2
	cmp	#6
	beq	no2
	cmp	#10	skip a line feed
	beq	no2

	pha
	jsl	COut_Vector

no2	return #0
;
;  ChOut - actual output routine
;
ChOut	entry
	php
	long	I,M
	plx
	ply
	pla
	phy
	phx
	sta	ch	save the character
	lda	consoleRefnum	write the character
	sta	wrRefnum
	~OSWrite wrRec
	plp
	rtl

wrRec	dc	i'4'	write DCB
wrRefnum ds	2
	dc	a4'ch'
	dc	i4'1'
	ds	4

ch	ds	2
	end

****************************************************************
*
*  Direction - Detect direction
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Direction private
	using Common
	using IOCom
d_devc	equ	0	disp to device number
d_dir	equ	2	disp to direction

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	cl2
	lda	sh_dcb	  if pCount <> 2 then
	cmp	#2
	beq	cl1
	return #4	    return invalid pCount error
cl1	lda	sh_dcb+2	  change the call to a class 0 call
	sta	sh_dcb
	lda	sh_dcb+4
	sta	sh_dcb+2
	add4	sh_dadr,#2

cl2	lda	sh_dcb+d_devc	 load A with proper direction based
	cmp	#3	 in the device
	bge	err
	cmp	#1
	bge	lb1
	lda	stin_direction
	bra	lb3
lb1	bne	lb2
	lda	stout_direction
	bra	lb3
lb2	lda	errout_direction
lb3	ldy	#2	set direction
	sta	[sh_dadr],y
	return #0

err	return #$53	parameter out of range
	end

****************************************************************
*
*  Error - Error Handler
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Error	private
	using Common
d_err	equ	0	disp to error number

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb2
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error

lb1	lda	sh_dcb+d_err+2
	bra	lb3
lb2	lda	sh_dcb+d_err
lb3	jsl	SysError
	return #0
	end

****************************************************************
*
*  Execute - Execute a command processor file
*
*  DCB:
*       Disp	 Length  Name    Use
*       ====	 ======  ====    ===
*       0 	 2       FLAG    flags (MSB == EXECUTE flag)
*       2 	 4       ADDR    address of the commands string
*
*  Notes:
*       The commands string is a one word length followed by
*       an ASCII command file.
*
****************************************************************
*
Execute	private
	using Common
	using RecCom

d_flag	equ	0	disp to flags
d_addr	equ	2	disp to pointer to EXEC file
;
;  Create the new EXEC record
;
	jsr	ClearLine	clear any old stuff in the line
	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	ex1
	lda	sh_dcb	  if pCount <> 2 then
	cmp	#2
	beq	lb1
	return #4	    return invalid pCount error
lb1	lda	sh_dcb+2	  change the call to a class 0 call
	sta	sh_dcb
	lda	sh_dcb+4
	sta	sh_dcb+2
	lda	sh_dcb+6
	sta	sh_dcb+4

ex1	tsx		make sure there is enough stack space
	cpx	sh_my_stack
	bgt	ex1a
	cpx	sh_low_stack
	blt	ex1a
	cpx	sh_min_stack
	bge	ex1a
	return #outofmem	memory full error

ex1a	jsr	PushExec	push the current exec file entry
	bcs	ex2
	jsr	PushInstruction	push the current instruction stack
	bcs	ex2
	move4 sh_dcb+d_addr,exec_ptr	set the new exec file pointer
	stz	exec_disp	set disp to begining
	stz	exec_exit	initialize variables
	stz	exec_return
	lda	sh_dcb+d_flag
	asl	A
	lda	#0
	rol	A
	sta	executeCmd
	malloc #ins_stack_size	get an instruction buffer
	sta	ins_ptr
	stx	ins_ptr+2
	ora	ins_ptr+2
	bne	ex3
ex2	return #$FFFF	probably no memory for exec node

ex3	stz	ins_disp	set initial displacement
	la	ins_size,ins_stack_size	set initial size
;
;  Process a line
;
pl1	stz	rcode	no error so far
	jsr	Spin	spin the spinner
	jsr	NextCLine	get the next line
	bcs	dn1
	jsr	ShellCommand	process the line
	cmp	#0	quit if monitor reports an error
	beq	pl1a
	sta	rcode
	lla	r12,exitStr	  and EXIT is non-null
	jsr	FindVariable
	lda	r0
	ora	r2
	bne	dn1
pl1a	lda	stop_flag	if stopflag is set then
	beq	pl2
	lda	sh_my_stack	  restore stack pointer
	tcs
	lda	sh_my_dp	  restore direct page
	tcd
	stz	stop_flag	  reset the stop flag
	jsr	CleanUp	  clean up any memory buffers
	jsl	mntr_vector	  goto command level

pl2	ldx	exec_exit	quit if exit requested
	beq	pl1
;
;  Done: get rid of EXEC entry
;
dn1	stz	exec_exit	clear early return values
	stz	exec_return
	stz	exec_level
	jsr	ClearLine	clear any old stuff in the line
	jsr	PopExec	pop the exec file entry
	jsr	PopInstruction	pop the instruction stack
	return rcode

rcode	ds	2	return code
	end

****************************************************************
*
*  ExpDevice - Expand devices
*
*  Inputs: (level $00)
*	sh_dadr - address of original DCB
*	sh_dcb - copy of call DCB
*	sh_dcb+d_name - address of the name buffer
*
*  Inputs: (level $40)
*	sh_dadr - address of original DCB
*	sh_dcb - copy of call DCB
*	sh_dcb+e_parms - number of parameters
*	sh_dcb+e_inName - address of the input name buffer
*	sh_dcb+e_outName - address of the output name buffer
*
****************************************************************
*
ExpDevice private
	using Common
d_name	equ	0	disp to name pointer

e_parms	equ	0	disp to parameters
e_name	equ	2	disp to input name pointer
e_out	equ	6	disp to output buffer (level $40 only)

maxDP	equ	20	# bytes of general purpose DP used
;
;  Form a local, GS/OS style copy of the input name
;
	ldx	#maxDP-2	save DP work space
sp1	lda	r0,X
	sta	regs,X
	dex
	dex
	bpl	sp1
	stz	err	no error, yet
	stz	pr_name	no pr_name buffer allocated, yet
	stz	pr_name+2
	stz	outBuff	no ouput buffer allocated, yet
	stz	outBuff+2

	lda	sh_fnum	get the length & start addr of the string
	and	#$0040
	bne	lc1
	add4	sh_dcb+d_name,#1,r0
	lda	[sh_dcb+d_name]
	and	#$00FF
	bra	lc2
lc1	lda	sh_dcb+e_parms
	cmp	#2
	beq	lc1a
	return #4
lc1a	add4	sh_dcb+e_name,#2,r0
	lda	[sh_dcb+e_name]

lc2	sta	r4	save the length
	inc	A	get memory for the local copy
	inc	A
	jsl	Malloc
	sta	inBuff
	sta	r8
	stx	inBuff+2
	stx	r10
	ora	r10	done if out of memory
	jeq	oom
	add4	r8,#2,r12	form the character destination address
	lda	r4	set the length of the buffer
	sta	[r8]
	tay		move the characters
	beq	dw1
	dey
	short M
lc3	lda	[r0],Y
	sta	[r12],Y
	dey
	bpl	lc3
	long	M
;
;  If the name starts with .. followed by a delimiter then form full path
;
dw1	move4 inBuff,r0	skip if the prefix does not start with
	lda	[r0]	 ..
	cmp	#2
	jlt	ep1
	ldy	#2
	lda	[r0],Y
	cmp	#'..'
	jne	ep1
	stz	pr_num	read prefix 0
	ph4	#pr_dcb
	jsr	GetPrefix
	sta	err
	jcs	db1
	move4 pr_name,r0	if the prefix is null then
	ldy	#2
	lda	[r0],Y
	bne	dw2
	free	pr_name	  dispose of the prefix 0 buffer
	lda	#8	  read prefix 8
	sta	pr_num
	ph4	#pr_dcb
	jsr	GetPrefix
	sta	err
	jcs	db1

dw2	move4 inBuff,r0	determine what the delimiter is
	lda	[r0]
	beq	dw5
	tax
	short M
	ldy	#2
dw3	lda	[r0],Y
	beq	dw4
	cmp	#'/'
	beq	dw4
	cmp	#':'
	beq	dw5
	iny
	dex
	bne	dw3
	bra	dw5
dw4	sta	r4	save the delimiter
cd1	lda	#':'	change the delimiters to ':'
	sta	[r0],Y
	lda	r4
cd2	iny
	dex
	beq	dw5
	cmp	[r0],Y
	beq	cd1
	bra	cd2
dw5	long	M

	move4 inBuff,r0	r0 == ptr to 1st char in path
	lda	[r0]	r4 == length of path
	sta	r4
	add4	r0,#2
	move4 pr_name,r8	r8 == ptr to 1st char in prefix -1
	ldy	#2	r6 == length of prefix
	lda	[r8],Y
	sta	r6
	add4	r8,#3

dw6	lda	r4	while path starts with .. do begin
	cmp	#2
	blt	dw8
	lda	[r0]
	cmp	#'..'
	bne	dw8
	ldy	r6	  erase the last directory in the
	short M	    prefix
	lda	#':'
dw7	dey
	bpl	dw7a
	lda	#$40	  (invalid pathname syntax)
	sta	err
	brl	db1
dw7a	cmp	[r8],Y
	bne	dw7
	long	M
	tya
	sta	r6
	add4	r0,#2	  skip the ..
	dec	r4
	dec	r4
	beq	dw6
	lda	[r0]	  if the next char is a delimiter then
	and	#$00FF
	cmp	#':'
	bne	dw6
	inc4	r0	    skip the delimiter
	dec	r4
	bra	dw6	  end

dw8	clc		find the length of the prefix + path
	lda	r4
	adc	r6
	sta	r16
	inc	A	reserve memory for the combined name
	inc	A
	jsl	Malloc
	sta	r12
	stx	r14
	ora	r14
	bne	dw9
oom	lda	#outOfMem
	sta	err
	brl	db1
dw9	lda	r16	set the length of the combined name
	sta	[r12]
	add4	r12,#1,r16	move in the prefix
	ldy	r6
	beq	dw10a
	short M
dw10	lda	[r8],Y
	sta	[r16],Y
	dey
	bne	dw10
	long	M
dw10a	sec		move in the path
	lda	r6
	adc	r16
	sta	r16
	bcc	dw11
	inc	r18
dw11	ldy	r4
	beq	dw13
	dey
	short M
dw12	lda	[r0],Y
	sta	[r16],Y
	dey
	bpl	dw12
	long	M
dw13	free	inBuff	dispose of the old path
	move4 r12,inBuff	set the pointer to the new path name
;
;  Expand the path name
;
ep1	ph4	#epRec	expand the path
	jsr	ExpandPath
	sta	err
	tax
	jne	db1

	move4 outBuff,r0	get the output buffer size
	lda	sh_fnum
	and	#$0040
	bne	ep2
	lda	#68
	bra	ep3
ep2	lda	[sh_dcb+e_out]
ep3	cmp	[r0]	if the output buffer is too small then
	bge	ep4
	lda	#$004F	  return a $4F error
	sta	err
	lda	sh_fnum	  if this is a class 1 call then
	and	#$0040
	beq	db1
	lda	[sh_dcb+e_out]	    if the buffer is 4 bytes or longer
	cmp	#4	      then
	blt	db1
	ldy	#2	      set the output buffer length
	lda	[r0],Y
	sta	[sh_dcb+e_out],Y
	bra	db1

ep4	lda	sh_fnum	save the expanded path
	and	#$0040
	bne	ep6

	clc		save a ProDOS 8 path name
	lda	r0
	ldx	r2
	adc	#2
	bcc	ep4a
	inx
ep4a	phx
	pha
	ph4	sh_dcb+d_name
	ph2	#1
	jsr	ReturnPString
	sta	error
	bra	db1

ep6	ldy	#2	save a GS/OS path name
	lda	[r0],Y
	sta	[sh_dcb+e_out],Y
	tax
	beq	db1
	ldy	#4
	short M
ep7	lda	[r0],Y
	sta	[sh_dcb+e_out],Y
	iny
	dex
	bne	ep7
	long	M
;
;  Dispose of dynamic buffers & return
;
db1	free	pr_name	dispose(pr_name)
	free	inBuff	dispose(inBuff)
	free	outBuff	dispose(outBuff)
	ldx	#maxDP-2	restore DP work space
db2	lda	regs,X
	sta	r0,X
	dex
	dex
	bpl	db2
ret	return err	return the error code
;
;  Local data
;
epRec	dc	i'3'	ExpandPath record
inBuff	ds	4
outBuff	ds	4
	dc	i'0'

err	ds	2	error code

pr_dcb	dc	i'2'	GetPrefix record
pr_num	ds	2
pr_name	ds	4
	end

****************************************************************
*
*  Export - Export a variable
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Export	private
	using Common
d_name	equ	0	disp to name pointer
d_export equ	4	disp to export flag

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb2
	lda	sh_dcb	  if pCount <> 2 then
	cmp	#2
	beq	lb1
	return #4	    return invalid pCount error
lb1	lda	sh_dcb+d_name+2	  change the call to a class 0 call
	sta	sh_dcb+d_name
	lda	sh_dcb+d_name+4
	sta	sh_dcb+d_name+2
	lda	sh_dcb+d_export+2
	sta	sh_dcb+d_export
	stz	allocated	  d_name was not allocated by us
	bra	ex1

lb2	lda	#1	d_name is allocated by us
	sta	allocated
	ph4	sh_dcb+d_name	convert the p-string name to an os string
	jsl	PtoOSString
	sta	d_name
	stx	d_name+2
	ora	d_name+2
	bne	ex1
	return #outOfMem

ex1	stz	error	no error so far
	jsr	FindVariable	find the variable
	lda	r0
	ora	r0+2
	bne	ex2
	dec	error
	bra	ex3
ex2	ldy	#12	set the export flag
	lda	sh_dcb+d_export
	sta	[r0],Y
ex3	lda	allocated	if allocated then
	beq	ex4
	free	d_name	  free(d_name)
ex4	return error

allocated ds	2	was d_name allocated locally?
error	ds	2	error number
	end

****************************************************************
*
*  FastFile - Fast file handler
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
*  DCB:
*       Disp	 Length  Name    Use
*       ====	 ======  ====    ===
*       0 	 2       ACTION  code:
*		                 00 - Load a file
*		                 01 - Indexed Load
*		                 02 - Load from Memory Only
*		                 03 - Save a file
*		                 04 - Add a file to the file list
*		                 05 - Delete a file from the file list
*		                 06 - Remove a file without disposing of handle
*		                 07 - Purge a file
*		                 08 - Set Information
*       2 	 2       INDEX   index number; for indexed load only
*       4 	 2       FLAGS   status flags
*		                 $8000 - set if the file exists on disk
*		                 $4000 - set if the file may be purged
*       6 	 4       HANDLE  handle of the file
*       10	 4       FILELENGTH length of the file, in bytes
*       14	 4       PATHNAME pointer to the path name
*       18	 2       ACCESS  ProDOS access code
*       20	 2       FILETYPE ProDOS file type code
*       22	 4       AUXTYPE ProDOS aux type value
*       26	 2       STORAGETYPE ProDOS storage type
*       28	 2       CREATEDATE ProDOS create date
*       30	 2       CREATETIME ProDOS create time
*       32	 2       MODDATE ProDOS mod date
*       34	 2       MODTIME ProDOS mod time
*       36	 4       BLOCKSUSED ProDOS blocks used count
*
****************************************************************
*
FastFile private
	using Common
;
;  DCB locations
;
d_action equ	0	action to take
d_index	equ	2	index number
d_flags	equ	4	status flags
d_handle equ	6	handle of the file
d_filelength equ 10	legth of the file, in bytes
d_pathname equ 14	pointer to the path name
d_access equ	18	ProDOS access code
d_filetype equ 20	ProDOS file type code
d_auxtype equ	22	ProDOS aux type value
d_storagetype equ 26	ProDOS storage type
d_createdate equ 28	ProDOS create date
d_createtime equ 30	ProDOS create time
d_moddate equ	32	ProDOS mod date
d_modtime equ	34	ProDOS mod time
d_blocksused equ 36	ProDOS blocks used count

d_total	equ	40	total length of the record

e_pCount equ	0	parameter count
e_action equ	2	action to take
e_index	equ	4	index number
e_flags	equ	6	status flags
e_handle equ	8	handle of the file
e_pathname equ 12	pointer to the path name
e_access equ	16	access code
e_filetype equ 18	file type code
e_auxtype equ	20	aux type value
e_storagetype equ 24	storage type
e_createdate equ 26	create date
e_moddate equ	34	mod date
e_option equ	42	option list (unused)
e_filelength equ 46	length of the file, in bytes
e_blocksused equ 50	blocks used count
e_checkSum equ 54	check sum

e_total	equ	56	total length of the record
;
;  If this is a class 0 call, convert the parameters
;
	stz	error	no error so far
	lda	sh_fnum	branch if this is a class 1 call
	and	#$40
	bne	pc1

!			blocks used
	move4 sh_dcb+d_blocksUsed,sh_dcb+e_blocksUsed
!			file length (EOF)
	move4 sh_dcb+d_fileLength,sh_dcb+e_fileLength
	stz	sh_dcb+e_option	option list
	stz	sh_dcb+e_option+2
	move4 sh_dcb+d_modDate,r0	mod date
	jsr	ProDOStoOSDate
	ldx	#6
cl1	lda	r4,X
	sta	sh_dcb+e_modDate,X
	dex
	dex
	bpl	cl1
	ldx	#8	action, index, flags, handle
cl2	lda	sh_dcb,X
	sta	sh_dcb+2,X
	dex
	dex
	bpl	cl2
	stz	sh_dcb+e_pCount	pCount
	ldx	#e_pathName	pathName, access, fileType, auxType,
cl3	lda	sh_dcb+2,X	 storageType
	sta	sh_dcb,X
	inx
	inx
	cpx	#e_createDate
	bne	cl3
	move4 sh_dcb+d_createDate,r0	create date
	jsr	ProDOStoOSDate
	ldx	#6
cl4	lda	r4,X
	sta	sh_dcb+e_createDate,X
	dex
	dex
	bpl	cl4
	lda	sh_dcb+e_action	if action = IndexedLoad then
	dec	A
	beq	in1	  skip file name conversions
	ph4	sh_dcb+e_pathName	convert to an OS name
	jsl	PtoOSString
	sta	sh_dcb+e_pathName
	stx	sh_dcb+e_pathName+2
	ora	sh_dcb+e_pathName+2
	bne	in1
	return #outOfMem	out of memory error
;
;  For class 1 calls, make sure pCount is valid
;
pc1	lda	sh_dcb+e_pCount	if not (pCount in [5..14]) then
	cmp	#5
	blt	pc2
	cmp	#15
	blt	in1
pc2	return #4	  flag the error
;
;  Initialization
;
in1	lda	sh_dcb+e_action	if action = IndexedLoad then
	dec	A
	bne	in1a
	stz	sh_dcb+e_pathName	  e_pathName := nill
	stz	sh_dcb+e_pathName+2
	brl	in3	else
in1a	stz	epPathname	  expand the file name to a full path name
	stz	epPathname+2
	ph4	#epRec
	move4 sh_dcb+e_pathname,epInName
	jsr	ExpandPath
	sta	error
	lda	sh_dcb+e_pCount	  if this is a class 0 call then
	bne	in2
	free	sh_dcb+e_pathName	    free the OS string buffer

in2	add4	epPathname,#2,sh_dcb+e_pathname move in the new path name
	lda	error	  quit if there was an error
	jne	lb6

in3	anop
;
;  Handle the command
;
	lda	sh_dcb+e_action	if ACTION = Load then begin
	bne	lb1
	jsr	FindFile	  if not FindFile then begin
	bcs	ll1
	jsr	NewRecord	    NewRecord;
	bcs	ll3
ll0	stz	sh_dcb+e_handle	    Handle := nil;
	stz	sh_dcb+e_handle+2
!			    end; {if}
ll1	jsr	LoadFile	  LoadFile;
	bcs	ll3
	ph2	#0	  don't allow the file to be purged
	ph4	sh_dcb+e_handle
	_SetPurge
	ph4	sh_dcb+e_handle	  Hlock(Handle);
	_HLock
ll3	brl	lb6	  end {if}

lb1	dec	a	else if ACTION = IndexedLoad then begin
	bne	lb1a
	move4 fileList,r0	  r0 := fileList;
	ldx	sh_dcb+e_index	  for i := 0 to Index do
	beq	ri2
	ldy	#2
ri1	lda	r0	    if R0 <> nil then
	ora	r2
	beq	ri2
	lda	[r0]	      R0 := R0^.next;
	pha
	lda	[r0],Y
	sta	r2
	pla
	sta	r0
	dex
	bne	ri1
ri2	lda	r0	  if R0 = nil then
	ora	r2
	jeq	fnf	    Error = $46 {file not found}
	ldy	#e_total+2	    SH_DCB := R0^;
	ldx	#e_total-2
ri4	lda	[r0],Y
	sta	sh_dcb,X
	dey
	dey
	dex
	dex
	cpx	#2
	bne	ri4
ri5	ph2	#0	  don't allow the file to be purged
	ph4	sh_dcb+e_handle
	_SetPurge
	ph4	sh_dcb+e_handle	  Hlock(Handle);
	_HLock
lb6a	brl	lb6	  end {else if}

lb1a	dec	A	else if ACTION = LoadMemory then begin
	bne	lb1b
	jsr	FindFile	  if not FindFile then
	bcs	ri5
fnf	lda	#$46	    Error = $46; {file not found}
	sta	error
	brl	lb6	  end {else if}

lb1b	dec	A	else if ACTION = Save then begin
	jne	lb1c
	jsr	Delete2	  remove any old file
	ph4	sh_dcb+e_pathName	  if the file does not exist then
	jsr	GetFileType
	inc	A
	bne	sv1
	ldx	#12	    Create(PathName);
cr1	lda	sh_dcb+e_pathname,X
	sta	crRec+2,x
	dex
	dex
	bpl	cr1
	~OSCreate crRec
	sta	error
	bcs	lb6a
sv1	move4 sh_dcb+e_pathName,opPathname Open(PathName);
	stz	opAccess
	~OSOpen opRec
	sta	error
	jcs	lb6	  if ToolError = 0 then begin
	lda	opRefnum	    SetEOF(0);
	sta	wrRefnum
	sta	clRefnum
	sta	efRefnum
	~OSSet_EOF efRec
	ph4	sh_dcb+e_handle	    Write(FileLength);
	_HLock
	ldy	#2
	lda	[sh_dcb+e_handle]
	sta	wrDataBuffer
	lda	[sh_dcb+e_handle],y
	sta	wrDataBuffer+2
	move4 sh_dcb+e_filelength,wrRequestCount
	~OSWrite wrRec
	sta	error
	bcs	sv2
	ph4	sh_dcb+e_handle
	_HUnlock
	ldy	#4	    make sure the handle is moveable
	lda	[sh_dcb+e_handle],Y
	and	#$3FFF
	sta	[sh_dcb+e_handle],Y
sv2	~OSClose clRec	    Close;
	move4 sh_dcb+e_pathname,giPathname GI_DCB.NAME := PathName;
	~OSGet_File_Info giRec	      GetFileInfo(GI_DCB);
	ldx	#e_blocksused+2-e_pathname   save the file info in the record
sv3	lda	giPathname,X
	sta	sh_dcb+e_pathname,X
	dex
	dex
	bpl	sv3
!			    end; {if}
	bra	ad1	  end {else if}

lb1c	dec	A	else if ACTION = AddFile then begin
	bne	lb2
	jsr	Delete2	  remove any old file
ad1	jsr	NewRecord	  NewRecord;
	move4 sh_dcb+e_handle,r4	  compute the checksum
	move4 sh_dcb+e_filelength,r8
	jsr	CheckSum
	sta	sh_dcb+e_checksum
	ldy	#e_total+2	  save the file info
ad2	tyx
	lda	sh_dcb-4,X
	sta	[r0],Y
	dey
	dey
	cpy	#8
	bne	ad2
	ldy	#6	  change the user ID to ours
	lda	~user_id
	sta	[sh_dcb+e_handle],Y
	ldy	#4	  make sure the handle is moveable
	lda	[sh_dcb+e_handle],Y
	and	#$3FFF
	sta	[sh_dcb+e_handle],Y
	stz	epPathname	  make sure the path is not freed
	stz	epPathname+2
	bra	lb6	  end {else if}

lb2	dec	A	else if ACTION = Delete then begin
	bne	lb2a
	lda	#1
	jsr	Delete
	bra	lb6	  end {else if}

lb2a	dec	A	else if ACTION = Remove then begin
	bne	lb3
	lda	#0
	jsr	Delete
	bra	lb6	  end {else if}

lb3	dec	A	else if ACTION = Purge then begin
	bne	lb4
	jsr	FindFile	  if FindFile then
	bcc	lb6
	ph4	sh_dcb+e_handle	    HUnlock(Handle);
	_HUnlock
	lda	sh_dcb+e_flags	    if the file may be purged then
	asl	A
	bpl	lb6
	ph2	#2	      SetPurge(2,Handle);
	ph4	sh_dcb+e_handle
	_SetPurge
	sta	error
	bra	lb6	  end {else if}

lb4	dec	A	else if ACTION = SetInfo then begin
	bne	lb5
	ph2	sh_dcb+e_flags	  save volitile parameters
	ph4	sh_dcb+e_handle
	jsr	FindFile	  if file not found then
	bcs	si1
	pla		    pull stack info
	pla
	pla
	bra	lb6	    quit
si1	ldy	#e_handle+4
	pla
	sta	[r0],Y
	pla
	iny
	iny
	sta	[r0],Y
	pla
	ldy	#e_flags
	sta	[r0],Y
	bra	lb6	    end; {if}
!			  end; {else if}

lb5	lda	#$53	else
	sta	error	  illegal parameter
lb6	lda	error	if Error <> 0 then
	jne	lb8
;
;  Return class 0 information
;
	lda	sh_dcb+e_pCount	branch if this is a class 1 call
	jne	rc3

	ldy	#d_flags	flags
	lda	sh_dcb+e_flags
	sta	[sh_dadr],Y
	ldy	#d_handle	handle
	lda	sh_dcb+e_handle
	sta	[sh_dadr],Y
	iny
	iny
	lda	sh_dcb+e_handle+2
	sta	[sh_dadr],Y
	ldy	#d_filelength	filelength
	lda	sh_dcb+e_filelength
	sta	[sh_dadr],Y
	iny
	iny
	lda	sh_dcb+e_filelength+2
	sta	[sh_dadr],Y
	ldy	#d_access	access
	lda	sh_dcb+e_access
	sta	[sh_dadr],Y
	ldy	#d_filetype	file type
	lda	sh_dcb+e_filetype
	sta	[sh_dadr],Y
	ldy	#d_auxType	aux type
	lda	sh_dcb+e_auxType
	sta	[sh_dadr],Y
	iny
	iny
	lda	sh_dcb+e_auxType+2
	sta	[sh_dadr],Y
	ldy	#d_storageType	storage type
	lda	sh_dcb+e_storageType
	sta	[sh_dadr],Y
	ldx	#6	create date
rc1	lda	sh_dcb+e_createDate,X
	sta	r0,X
	dex
	dex
	bpl	rc1
	jsr	OStoProDOSDate
	ldy	#d_createDate
	sta	[sh_dadr],Y
	iny
	iny
	txa
	sta	[sh_dadr],Y
	ldx	#6	mod date
rc2	lda	sh_dcb+e_modDate,X
	sta	r0,X
	dex
	dex
	bpl	rc2
	jsr	OStoProDOSDate
	ldy	#d_modDate
	sta	[sh_dadr],Y
	iny
	iny
	txa
	sta	[sh_dadr],Y
	ldy	#d_blocksUsed	blocks used
	lda	sh_dcb+e_blocksUsed
	sta	[sh_dadr],Y
	iny
	iny
	lda	sh_dcb+e_blocksUsed+2
	sta	[sh_dadr],Y
	ph4	sh_dcb+e_pathname	path name
	ph4	#p16pathname
	ph2	#1
	jsr	ReturnPString
	sta	error
	ldy	#d_pathname
	lda	#p16pathname
	sta	[sh_dadr],Y
	iny
	iny
	lda	#^p16pathname
	sta	[sh_dadr],Y
	bra	lb8
;
;  Return class 1 info
;
rc3	lda	sh_dcb+e_action	if action = IndexedLoad then
	dec	A
	bne	rc4
	ph4	sh_dcb+e_pathname	  set the return string
	ldy	#e_pathname+2
	lda	[sh_dadr],Y
	pha
	dey
	dey
	lda	[sh_dadr],Y
	pha
	jsr	ReturnOSString
	sta	error
rc4	ldy	#e_pathname	restore the path name pointer
	lda	[sh_dadr],Y
	sta	sh_dcb+e_pathname
	iny
	iny
	lda	[sh_dadr],Y
	sta	sh_dcb+e_pathname+2
	lda	sh_dcb+e_pCount	SH_DADR^ := SH_DCB;
	asl	A
	tax
	lda	lengths,X
	tay
lb7	tyx
	lda	sh_dcb,X
	sta	[sh_dadr],Y
	dey
	dey
	bne	lb7
;
;  Return to the caller
;
lb8	free	epPathname	free the expanded path name buffer
	return error	return any error code
;...............................................................
;
;  CheckSum - compute the checksum
;
;  Inputs:
;	r4 - file handle
;	r8 - file length
;
;  Outputs:
;	A - checksum
;
;...............................................................
;
CheckSum anop

	ldy	#4	see if the handle starts locked
	lda	[sh_dcb+e_handle],Y
	sta	isLocked
	ph4	r4	lock the handle
	_HLock
	ldy	#2	dereference the handle
	lda	[r4],Y
	sta	r14
	lda	[r4]
	sta	r12
	lda	r8	initialize the checksum
	lsr	A
	bcc	cs1
	lda	[r12]
	and	#$00FF
	inc4	r12
	dec	r8
	bra	cs2
cs1	lda	#0
cs2	ldx	r10	check full blocks
	beq	cs4
	ldy	#0
cs3	eor	[r12],Y
	iny
	iny
	bne	cs3
	inc	r14
	dex
	bne	cs3
cs4	ldy	r8	check partial blocks
	beq	cs7
	bra	cs6
cs5	eor	[r12],Y
cs6	dey
	dey
	bne	cs5
	eor	[r12]
cs7	pha		unlock the handle
	lda	isLocked
	bmi	cs8
	ph4	r4
	_HUnlock
cs8	pla
	rts		done

isLocked ds	2	did the handle start locked?
;...............................................................
;
;  Delete - Remove a file from the file list
;
;  Inputs:
;	A - 1 to delete the file buffer, else 0
;
;...............................................................
;
Delete	pha
	jsr	FindFile	  if FindFile then begin
	pla
	bcc	dl2
	beq	dl1	    if deleteFile then
	lda	sh_dcb+e_handle
	ora	sh_dcb+e_handle+2
	beq	dl1
	ph4	sh_dcb+e_handle	      DisposeHandle(Handle);
	_DisposeHandle
dl1	stz	sh_dcb+e_handle	    Handle := nil;
	stz	sh_dcb+e_handle+2
	jsr	Delete3                      Delete the record and path name
	stz	sh_dcb+e_pathname            not that the name is gone
	stz	sh_dcb+e_pathname+2
dl2	rts		    end; {if}
;...............................................................
;
;  Delete2 - remove any old file for Save and AddFile
;
;...............................................................
;
Delete2	ldx	#e_total-2	save the old record
del1	lda	sh_dcb,X
	sta	tRec,X
	dex
	dex
	bpl	del1
	lda	#1	remove any old file
	jsr	Delete
	ldx	#e_total-2	restore the old record
del2	lda	trec,X
	sta	sh_dcb,X
	dex
	dex
	bpl	del2
	rts
;...............................................................
;
;  Delete3 - Delete a file that has already been found by FindFile
;
;  Inputs:
;	A - 1 to delete the file buffer, else 0
;	r0 - pointer to the file record
;
;...............................................................
;
Delete3	anop
	ldy	#6	    if r0^.next <> nil then
dt1	lda	[r0],Y
	tyx
	sta	r4,X
	dey
	dey
	bpl	dt1
	lda	r4
	ora	r6 
	beq	dt2
	ldy	#4	      r0^.next^.last := r0^.last;
	lda	r8
	sta	[r4],Y
	iny
	iny
	lda	r10
	sta	[r4],Y
dt2	lda	r8	    if r0^.last = nil then
	ora	r10
	bne	dt3
	move4 r4,fileList	      FileList := r0^.next
	bra	dt4	    else
dt3	ldy	#2	      r0^.last^.next := r0^.next;
	lda	r4
	sta	[r8]
	lda	r6 
	sta	[r8],Y
dt4	ldy	#e_pathname+4	    Dispose(FileName);
	lda	[r0],Y
	sta	r4
	iny
	iny
	lda	[r0],Y
	sta	r6
	free	r6
	free	r0	    Dispose(r0);
	rts
;...............................................................
;
;  FindFile - See if a file is in the memory list
;
;  Outputs:
;	C - set if the file was found
;	R0 - address of the file entry (if found)
;	SH_DCB - DCB info from found file
;
;...............................................................
;
FindFile anop
	move4 fileList,r0	R0 := FileList;
ff1	lda	r0	while R0 <> nil do begin
	ora	r2
	bne	ff2
	clc
	rts
ff2	ldy	#e_pathname+6	  if R0^.pathName^ = pathName^ then
	lda	[r0],Y	    begin
	pha
	dey
	dey
	lda	[r0],Y
	pha
	ph4	sh_dcb+e_pathname
	jsl	CompareOSStrings
	tax
	bne	ff6
	ldy	#e_handle+4	    if there is a handle then
	lda	[r0],Y
	sta	r4
	iny
	iny
	lda	[r0],Y
	sta	r6
	ora	r4
	beq	ff2a
	ldy	#2	      if the memory exists then
	lda	[r4]
	ora	[r4],Y
	beq	ff2a
	ldy	#e_filelength+4		check the checksum
	lda	[r0],Y
	sta	r8
	iny
	iny
	lda	[r0],Y
	sta	r10
	jsr	CheckSum
	ldy	#e_checkSum+4
	cmp	[r0],Y
	beq	ff3
	free	r4	    failed checksum - delete the file
ff2a	jsr	Delete3	      remove the record
	clc		      return not found code
	rts

ff3	add4	r0,#4,r4	    SH_DCB := R0^;
	ldy	#e_total-2
ff5	lda	[r4],Y
	tyx
	sta	sh_dcb,X
	dey
	dey
	cpy	#e_index
	bne	ff5
	sec		    return C=1;
	rts

ff6	anop		    end; {if}
	ldy	#2	  R0 := R0^.next;
	lda	[r0]
	tax
	lda	[r0],Y
	sta	r2
	stx	r0
	brl	ff1	  end; {while}
!			return C=0;
;...............................................................
;
;  DumpRecord - get rid of a file record (used for error conditions)
;
;  Inputs:
;	fileList - points to the record to be removed
;	sh_dcb+e_pathname - points to the path name
;
;...............................................................
;
DumpRecord anop
	move4 fileList,r0	r0 := fileList;
	ldy	#2	fileList := r0^.next;
	lda	[r0],Y
	sta	fileList+2
	lda	[r0]
	sta	fileList
	ora	fileList+2	if fileList <> nil then
	beq	dr1
	move4 fileList,r4	   fileList^.last := nil;
	lda	#0
	ldy	#4
	sta	[r4],Y
	iny
	iny
	sta	[r4],Y
dr1	free	sh_dcb+e_pathName	dispose(sh_dcb+e_pathName);
	free	r0	dispose(r0);
	rts
;...............................................................
;
;  NewRecord - allocate a new file record
;
;  Outputs:
;	C - set if an error occurred
;	ERROR - error number, if any
;
;...............................................................
;
NewRecord anop
	malloc #4+e_total	    r0 = calloc(4+e_total);
	sta	r0
	stx	r2
	ora	r2
	bne	rd1
	lda	#outOfMem
	sta	error
	sec
	rts
rd1	ldy	#2+e_total
	lda	#0
rd2	sta	[r0],Y
	dey
	dey
	bpl	rd2

	lda	fileList	    r0^.next := fileList;
	sta	[r0]
	sta	r4
	ldy	#2
	lda	filelist+2
	sta	[r0],Y
	sta	r6
	ora	fileList	    if fileList <> nil then
	beq	rd4
	ldy	#4	      fileList^.last := r0;
	lda	r0
	sta	[r4],Y
	iny
	iny
	lda	r2
	sta	[r4],Y
rd4	move4 r0,fileList	    fileList := r0;
	clc
	rts
;...............................................................
;
;  LoadFile - Load a file (if necessary)
;
;  Inputs:
;	R0 - pointer to the file record
;
;  Outputs:
;	SH_DCB - filled in with file's info
;	C - set if the file could not be loaded
;
;...............................................................
;
LoadFile anop
	ldy	#e_pathname+4	  if there is no file name then
	lda	[r0],Y
	iny
	iny
	ora	[r0],Y
	bne	cf0
	ldy	#e_pathname+4	    claim the file name for our use
	lda	sh_dcb+e_pathname
	sta	[r0],Y
	iny
	iny
	lda	sh_dcb+e_pathname+2
	sta	[r0],Y
	stz	epPathname	    make sure it is not disposed of on
	stz	epPathname+2	     exit

cf0	lda	sh_dcb+e_flags	if the file is a disk file then
	bpl	cf1
	move4 sh_dcb+e_pathname,giPathname recover the file's information
	~OSGet_File_Info giRec
	bcc	cf1
err	sta	error
	lda	#1
	jsr	Delete
	sec
	rts

cf1	lda	sh_dcb+e_handle	if (handle = nil) or
	ora	sh_dcb+e_handle+2
	beq	lf3
	ldy	#2	  (handle^ = nil) or
	lda	[sh_dcb+e_handle]
	ora	[sh_dcb+e_handle],Y
	beq	lf2
	lda	sh_dcb+e_flags	  {skip mod date check for RAM files}
	bpl	lf4a
	lda	sh_dcb+e_moddate	  (giModDate > sh_dcb.mod_date)
	cmp	giModDate       
	bne	lf1
	lda	sh_dcb+e_moddate+2
	cmp	giModDate+2     
	bne	lf1
	lda	sh_dcb+e_moddate+4
	cmp	giModDate+4     
	bne	lf1
	lda	sh_dcb+e_moddate+6
	cmp	giModDate+6     
lf1	blt	lf2
lf4a	brl	lf4	  then begin
!			  if handle <> nil then
lf2	ph4	sh_dcb+e_handle	    DisploseHandle(handle);
	_DisposeHandle
lf3	lda	sh_dcb+e_flags	  {error if a RAM file is gone}
	bmi	cf2
	lda	#$46	  {file not found}
	sta	error
	sec
	rts

cf2	anop		  copy the file's info
	ldx	#e_blocksused-e_access+2
lf3a	lda	giRec+6,X
	sta	sh_dcb+e_access,X
	dex
	dex
	bpl	lf3a
	move4 sh_dcb+e_pathname,opPathname open the file
	lda	#$0001
	sta	opAccess
	~OSOpen oprec
	jcs	err
	lda	opRefnum
	sta	wrRefnum
	sta	clRefnum
	pha		  allocate a file buffer
	pha
	lda	sh_dcb+e_filelength
	ora	sh_dcb+e_filelength+2
	bne	lf3c
	ph4	#1
	bra	lf3d
lf3c	ph4	sh_dcb+e_filelength
lf3d	ph2	~user_id
	ph2	#$8000
	ph4	#0
	_NewHandle
	sta	error
	pl4	sh_dcb+e_handle
	bcs	err3
	move4 sh_dcb+e_filelength,wrRequestCount read the file
	ldy	#2
	lda	[sh_dcb+e_handle]
	sta	wrDataBuffer
	lda	[sh_dcb+e_handle],Y
	sta	wrDataBuffer+2
	~OSRead wrRec
	bcs	err2
	~OSClose clRec	  close the file
	move4 sh_dcb+e_handle,r4	  compute the checksum
	move4 sh_dcb+e_filelength,r8
	jsr	CheckSum
	sta	sh_dcb+e_checksum
	ldy	#e_total+2	  save the file info
lf3b	tyx
	lda	sh_dcb-4,X
	sta	[r0],Y
	dey
	dey
	cpy	#6
	bne	lf3b
lf4	anop		  end; {if}
	clc
	rts

err2	sta	error
err3	lda	#1
	jsr	Delete
	~OSClose clRec
	sec
	rts
;
;  Local data
;
error	ds	2	tool/ProDOS error code
trec	ds	e_total	temp record
p16pathname ds 65	ProDOS 16 path name buffer

lengths	dc	i'2,2,4,6,10,14,16,18,22,24,32,40,44,48,52'

epRec	dc	i'3'	ExpandPath record
epInName ds	4	input path name
epPathname ds	4	output path name
	dc	i'0'	don't uppercase the name

giRec	dc	i'10'	OSGetFileInfo record
giPathName ds	4
	ds	2
	ds	2
	ds	4
	ds	2
	ds	8
giModDate ds	8
	ds	4
	ds	4
	ds	4

opRec	dc	i'3'	OSOpen record
opRefnum ds	2
opPathname ds	4
opAccess ds	2

wrRec	dc	i'4'	OSWrite record
wrRefnum ds	2
wrDataBuffer ds 4
wrRequestCount ds 4
	ds	4

clRec	dc	i'1'	OSClose record
clRefnum ds	2

efRec	dc	i'3'	OSSetEOF
efRefnum ds	2
	dc	i'0'
	dc	a4'0'

crRec	dc	i'5'	OSCreate record
	ds	4
	ds	2
	ds	2
	ds	4
	ds	2
	end

****************************************************************
*
*  GetCommand - Get a comand table entry
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
*  DCB:
*	Disp	Length  Name	Use
*	====	======  ====	===
*	0	2       index	command index, indexing from 1
*	2	2       restart	restart flag
*	4	2       userID	last known User ID
*	6	2       cNum	command number - 0 for utility
*	8	1       nameLen	length of the command name;
*			0 -> no command
*	9	15      name	characters of the name
*
****************************************************************
*
GetCommand private
	using Common

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb3
	lda	sh_dcb	  if pCount <> 5 then
	cmp	#5
	beq	lb1
	return #4	    return invalid pCount error
lb1	ldx	#0	  change the call to a class 0 call
	ldy	#12
lb2	lda	sh_dcb+2,X
	sta	sh_dcb,X
	inx
	inx
	dey
	bne	lb2
	add4	sh_dadr,#2

lb3	add2	sh_dadr,#2	inc past the index
	ldy	#disp_name	assume no entry
	lda	#0
	sta	[sh_dadr],Y
	move4 ctpointer,r0	init command table address
lb4	ldy	#disp_cnum+1	quit if we're at the end of the list
	lda	[r0],Y
	beq	none
	dec	sh_dcb	quit if we've got it
	beq	lb5
	ldy	#disp_name	next entry
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	lb4
	inc	r2
	bra	lb4
lb5	short M	move the entry in
	ldy	#disp_name+15
lb6	lda	[r0],Y
	sta	[sh_dadr],Y
	dey
	bpl	lb6
	long	M
none	anop
	return #0
	end

****************************************************************
*
*  GetDevices - Get console I/O device info
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
GetDevices private
	using Common
	using IOCom

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	gt1
	lda	sh_dcb	  if pCount <> 6 then
	cmp	#6
	beq	lb1
	return #4	    return invalid pCount error
lb1	add4	sh_dadr,#2

gt1	ldy	#16	copy the info
gt2	lda	devstout,Y
	sta	[sh_dadr],Y
	dey
	dey
	bpl	gt2
	return #0
	end

****************************************************************
*
*  GetLanguage - Get language number
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
GetLanguage private
	using Common

d_lnum	equ	0	disp to language number

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	gt1
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error
lb1	add4	sh_dadr,#2

gt1	ldy	#d_lnum
	lda	clang
	sta	[sh_dadr],Y
	return #0
	end

****************************************************************
*
*  GetLInfo - Get L Info
*
*  DCB:
*       Disp	 Length  Name    Use
*       ====	 ======  ====    ===
*       0 	 4       SADDR   addr of source file name
*       4 	 4       OADDR   addr of output file name
*       8 	 4       PADDR   addr of parameter list
*       12	 4       LADDR   addr of language specific input string
*       16	 1       MERR    max error allowed
*       17	 1       MERRF   max error found
*       18	 1       OPS     operation flag
*       19	 1       KEEP    keep flag
*       20	 4       MINUS   set of letters selected with -
*       24	 4       PLUS    set of letters selected with +
*       28	 4       ORG     origin
*
****************************************************************
*
GetLInfo private
	using Common
d_saddr	equ	0	displacements info LInfo field
d_oaddr	equ	4	
d_paddr	equ	8	
d_laddr	equ	12	
d_merr	equ	16	
d_merrf	equ	17	
d_ops	equ	18	
d_keep	equ	19	
d_minus	equ	20	
d_plus	equ	24	
d_org	equ	28
len	equ	32	length of DCB
;
;  Handle the names in a class 1 call
;
	stz	error	error = 0

	lda	sh_fnum	branch if the call is class 0
	and	#$40
	beq	gt2

	lda	sh_dcb	make sure there are 11 parameters
	cmp	#11
	beq	gt1
	lda	#4
	sta	error
	brl	in4
gt1	add4	sh_dadr,#2	skip the parm count
	ph4	li_saddr	return the names
	ph4	sh_dcb+d_saddr+2
	jsr	ReturnOSString
	sta	error
	ph4	li_oaddr
	ph4	sh_dcb+d_oaddr+2
	jsr	ReturnOSString
	ora	error
	sta	error
	ph4	li_paddr
	ph4	sh_dcb+d_paddr+2
	jsr	ReturnOSString
	ora	error
	sta	error
	ph4	li_laddr
	ph4	sh_dcb+d_laddr+2
	jsr	ReturnOSString
	ora	error
	sta	error
	brl	gt3
;
;  Handle names in a class 0 call
;
gt2	ph4	li_saddr	return the source name
	ph4	sh_dcb+d_saddr
	ph2	#1
	jsr	ReturnLPString
	sta	error
	ph4	li_oaddr	return the output name
	ph4	sh_dcb+d_oaddr
	ph2	#1
	jsr	ReturnPString
	ora	error
	sta	error
	ph4	li_paddr	return the partial compile list
	ph4	sh_dcb+d_paddr
	ph2	#0
	jsr	ReturnLPString
	ora	error
	sta	error
	short I,M	...and make sure it ends with a space
	lda	[sh_dcb+d_paddr]
	tay
	beq	gt2a
	lda	[sh_dcb+d_paddr],Y
	cmp	#' '
	beq	gt2a
	iny
	lda	#' '
	sta	[sh_dcb+d_paddr],Y
	tya
	sta	[sh_dcb+d_paddr]
gt2a	long	I,M
	ph4	li_laddr	return the language dependent stuff
	ph4	sh_dcb+d_laddr
	ph2	#0
	jsr	ReturnLPString
	ora	error
	sta	error
;
;  Save common parameters
;
gt3	short M	save the parameters
	ldy	#d_org+3
in3	lda	li_saddr,Y
	sta	[sh_dadr],Y
	dey
	cpy	#d_merr
	bge	in3
	long	M

in4	return error

error	ds	2	error number
	end

****************************************************************
*
*  InitWildcard - Initialize wildcard
*
*  Inputs: (level $00)
*	sh_dadr - address of original DCB
*	sh_dcb - ptr to input file name
*	sh_dcb+4 - selection rules flag (action taken if set):
*		$8000 - prompting is not allowed
*		$4000 - only one match is needed
*		$2000 - return files within a directory
*		$1000 - (only used if $2000 set) return directory last
*
*  Inputs: (level $40)
*	sh_dadr - address of original DCB
*	sh_dcb - number of parameters
*	sh_dcb+2 - ptr to input file name
*	sh_dcb+6 - selection rules flag (action taken if set):
*		$8000 - prompting is not allowed
*		$4000 - only one match is needed
*		$2000 - return files within a directory
*		$1000 - (only used if $2000 set) return directory last
*
*  Outputs:
*	wcFileList - points to a linked list of mathcing files
*
****************************************************************
*
InitWildcard private
	using Common
DIR	equ	$0F	directory file type

d_name	equ	0	disp to name pointer
d_flags	equ	4	disp to input flags

e_parms	equ	0	parameter count
e_name	equ	2	disp to name pointer
e_flags	equ	6	disp to input flags
;...............................................................
;
;  This section of the subroutine does the preparation needed
;  to build a list of file names.
;
;...............................................................
;
;  Recover the flags
;
	stz	epOut	no output buffer reserved, yet
	stz	epOut+2
	stz	epIn	no file name allocated, yet
	stz	epIn+2
	stz	epInAllocated	epInAllocated = false
!	stz	directoryList	no directory list, yet
!	stz	directoryList+2
	stz	prompt	don't prompt on matches
 	stz	wildcards	no wildcards found in file name
	stz	stop	search has not been aborted
	stz	gdRefNum	no open directories, yet

	ldx	sh_dcb+d_flags	set x to the flags word
	lda	sh_fnum
	and	#$0040
	beq	rf1
	ldx	sh_dcb+e_flags
rf1	txa		set the various flags
	and	#$8000
	xba
	sta	promptFlag
	txa
	and	#$4000
	sta	oneName
	txa
	and	#$2000
	sta	doDirectories
	txa
	and	#$1000
	sta	doDirLast
;
;  Build a GS/OS full path name
;
	lda	sh_fnum	if the call is a level $40 call then
	and	#$40
	beq	rf2
	lda	sh_dcb	  if parms <> 2 then
	cmp	#2
	beq	rf1a
	lda	#4	    return error 4
	sta	err
	brl	Error
rf1a	move4 sh_dcb+e_name,epIn	  epIn = e_name
	bra	rf5	else
rf2	lda	[sh_dcb+d_name]	  epIn = Malloc(length(d_name)+2)
	and	#$00FF
	inc	A
	inc	A
	jsl	Malloc
	sta	epIn
	sta	r0
	stx	epIn+2
	stx	r2
	ora	r0
	jeq	oom
	inc	epInAllocated	  epInAllocated := true
	lda	[sh_dcb+d_name]	  set the length of the string
	and	#$00FF
	tax
	sta	[r0]
	inc4	r0	  move in the characters
	ldy	#1
	short M
rf4	lda	[sh_dcb+d_name],Y
	sta	[r0],Y
	iny
	dex
	bne	rf4
	long	M
rf5	anop		endif
	ph4	#epRec	expand the path name
	jsr	ExpandPath
	sta	err
	lda	epInAllocated	if epIn was reserved, free it
	beq	rf6
	free	epIn
rf6	lda	err	quit if there was an error
	jne	Error

	move4 epOut,r0	replace all ? chars with = chars,
	ldy	#2	 setting prompt if a ? is found
	lda	[r0],Y	 and promptFlag is true
	beq	rf9
	tax
	ldy	#4
	short M
rf7	lda	[r0],Y
	cmp	#'?'
	bne	rf8
	lda	#'='
	sta	[r0],Y
	lda	promptFlag
	bne	rf8
	lda	#1
	sta	prompt
rf8	lda	[r0],Y
	cmp	#'='
	bne	rf8a
	lda	#1
	sta	wildcards
rf8a	iny
	dex
	bne	rf7
	long	M

rf9	move4 epOut,r0	if the path name is not a full path
	ldy	#4	  name then
	lda	[r0],Y
	cmp	#'..'
	beq	rf10
	and	#$00FF
	cmp	#':'
	beq	dl1
	cmp	#'.'
	beq	dl1
	ldy	#2	  change the current name from an
	tya		    output name to an input name
	clc		    starting with 0:
	adc	[r0],Y
	sta	[r0]
	lda	#':0'
	sta	[r0],Y
rf10	add4	r0,#2,epIn	  re-expand the path name
	dec	epRec	  (parm count is 2 for ExpandDevice)
	ph4	#epRec
	jsr	ExpandDevice
	inc	epRec
	sta	err	  save the error number
	free	epIn	  dispose of the input name buffer
	lda	err	  quit if an error occurred
	jne	Error
;
;  If there is an existing list of files, delete it
;
dl1	jsr	DisposeOfFileList
;
;  If the file is a single file, and is either not a directory or directory
;  expansion has not been requested, then skip the process of scanning the
;  directory to look for matching files.
;
	lda	wildcards	if not wildcards then
	jne	of4
	lda	#12	  get the info about the file
	sta	wcNext+2
	stz	wcOptionList
	stz	wcOptionList+2
	add4	epOut,#2,wcPath
	~OSGet_File_Info wcNext+2
	bcs	of4
         lda   wcFileType                 if (wcFileType <> DIR)
	cmp	#DIR
	bne	of1
         lda   doDirectories                or (not doDirectories) then
	bne	of3
of1	malloc #wcLength	      get a record for the file name
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
	move4 wcFileList,wcNext	      fill in the record
	move4	epOut,wcPath
	stz	epOut                          reserve epOut for our use
	stz	epOut+2
	ldy	#wcLength-2	      move the record to the dynamic area
of2	lda	wcNext,Y
	sta	[r0],Y
	dey
	dey
	bpl	of2
	move4 r0,wcFileList	      place the file in the list
         brl	fn1                            return
of3      anop                             endif
of4	anop	                         endif
;
;  Skip file removal for volume names
;
	move4 epOut,r0	if the name starts with a ':' then
	ldy	#2
	lda	[r0],Y
	tax
	ldy	#4
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	jne	sf1
	dex		  if there are no imbeded slashes then
	beq	sf1
	dex
	beq	sf1
	short M
	lda	#':'
vn1	iny
	cmp	[r0],Y
	beq	vn2
	dex
	bne	vn1

	long	M	    this is a volume name:
	lda	doDirectories	      if not doDirectories then
	jeq	Error		quit with a null list
	stz	fileNamePtr	      fileNamePtr = null
	stz	fileNamePtr+2
	ldy	#2	      if the last char is not ':' then
	lda	[r0],Y
	clc
	adc	#3
	tay
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	jeq	pd1
	lda	[r0]		add a ':' to the name
	inc	A
	jsl	Malloc
	sta	r4
	stx	r6
	ora	r6
	jeq	oom
	lda	[r0]
	tay
	short M
	lda	#':'
	sta	[r4],Y
	dey
vn1a	lda	[r0],Y
	sta	[r4],Y
	dey
	bne	vn1a
	long	M
	lda	[r0]
	inc	A
	sta	[r4]
	ldy	#2
	lda	[r0],Y
	inc	A
	sta	[r4],Y
	free	r0
	move4 r4,epOut

	brl	pd1	      go expand the names

vn2	long	M
;
;  Separate the file name from the prefix
;
sf1	ldy	#2	if there is a trailing ':' then
	lda	[r0],Y
	clc
	adc	#3
	tay
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	bne	sf2
	ldy	#2	  remove it
	lda	[r0],Y
	dec	A
	sta	[r0],Y
sf2	ldy	#2	find the length of the file name
	lda	[r0],Y
	clc
	adc	#3
	tay
	ldx	#0
	short M
	lda	#':'
sf3	cmp	[r0],Y
	beq	sf4
	inx
	dey
	bra	sf3
sf4	long	M
	sty	r4	save the disp to the ':'
	tya		truncate the path name
	sec		 (removes the file name)
	sbc	#3
	ldy	#2
	sta	[r0],Y
	stx	r6	save the length of the file name
	inx		reserve memory for the file name
	inx
	txa
	jsl	Malloc
	sta	r8
	sta	fileNamePtr
	stx	r10
	stx	fileNamePtr+2
	ora	r10
	jeq	oom

	lda	r6	set the length of the file name
	sta	[r8]
	clc		set up the disp to the file name
	lda	r4
	dec	A
	adc	r0
	sta	r0
	bcc	sf5
	inc	r2
sf5	ldy	#2	move the file name characters
	ldx	r6
	short M
sf6	lda	[r0],Y
	sta	[r8],Y
	iny
	dex
	bne	sf6
	long	M
;...............................................................
;
;  This section builds a list of matching file names from the
;  input directory epOut and the input file name fileNamePtr.
;
;  If fileNamePtr is null, epOut must be a volume name or
;  directory name, and doDirectories must be true.  The directory
;  and its files will be added to the list.
;
;...............................................................
;
;  If we are doing a directory, and directories appear first in
;  the file list, add the directory to the file list
;
pd1	lda	fileNamePtr	if we are doing a directory then
	ora	fileNamePtr+2
	bne	pd2
	lda	doDirLast	  if directories come first then
	bne	pd2
	jsr	AddDirectory	    add it to the list
pd2	anop
;
;  Open the directory
;
	add4	epOut,#2,opPathname	open the directory
	~OSOpen opRec
	sta	err
	jcs	Error
;
;  Set up the GetDirEntry record to read the directory
;
	lda	#1	set displacement to 1
	sta	gdDisplacement
	lda	opRefNum	set the file reference number
	sta	gdRefNum
;
;  Scan the directory, handling matching files
;
sd1	jsr	Pause	if open-apple . pressed then
	bcc	st1
	inc	stop	  stop = true
st1	lda	stop	if stop then
	jne	cl1	  quit the search
	jsr	Spin	spin the spinner
	ph4	#gdRec	get info about this file
	jsr	GetDirEntry
	tax
	jne	cl1
	jsr	NamesMatch	if the name does not match a legal
	bcs	sd1b	  one then
	free	gdName	  dispose of the name buffer
	free	gdOptionList               duspose of the option list
sd1a	inc	gdDisplacement	next entry
	bra	sd1

sd1b	lda	gdFileType	if the name is a directory then
	cmp	#DIR
	jne	sd3
	lda	doDirectories	  if we are doing directories then
	jeq	sd3

	malloc #dlLength	    get space for a directory list
	sta	r0	      record
	stx	r2
	ora	r2
	jeq	oom
	move4 directoryList,dlNext	    fill in volatile information
	move4 epOut,dlDirName
	move4 fileNamePtr,dlFileName
	move4 gdBlockCount,dlBlocks
	lda	gdDisplacement
	sta	dlDisplacement
	lda	gdRefNum
	sta	dlRefNum
	move4 gdName,dlGDRecName
	free	gdOptionlist                 free the option list
	stz	gdOptionList
	stz	gdOptionList+2
	ldy	#dlLength-2	    r0^ = record info
sd2	lda	dlNext,Y
	sta	[r0],Y
	dey
	dey
	bpl	sd2
	move4 r0,directoryList	    directoryList = r0

	jsr	AppendNames	    append gdName to epOut
	jcs	oom
	ldy	#2	    add in the trailing ':'
	lda	[r8],Y
	inc	A
	sta	[r8],Y
	move4 r8,epOut	    epOut = r8
	stz	fileNamePtr	    fileNamePtr = null
	stz	fileNamePtr+2
	brl	pd1	    go do the directory

sd3	jsr	AppendNames	form the file name
	jcs	oom
	malloc #wcLength	get a record for the file name
	sta	r0
	stx	r2
	ora	r2
	bne	sd4
	free	r8
	brl	oom
sd4	move4 wcFileList,wcNext	fill in the record
	move4 r8,wcPath
	lda	gdAccess
	sta	wcAccess
	lda	gdFileType
	sta	wcFileType
	ldx	#$0D
	cmp	#DIR
	beq	sd5
	ldx	#1
	lda	gdFlags
	bpl	sd5
	ldx	#5
sd5	stx	wcStorageType
	move4 gdAuxType,wcAuxType
	move	gdCreateDateTime,wcCreateDateTime,#16
	move4 gdOptionList,wcOptionList
	move4 gdEOF,wcEOF
	move4 gdBlockCount,wcBlocksUsed
	move	gdResourceEOF,wcResourceEOF,#8
	ldy	#wcLength-2	move the record to the dynamic area
sd6	lda	wcNext,Y
	sta	[r0],Y
	dey
	dey
	bpl	sd6
	move4 r0,wcFileList	place the file in the list
	lda	wildcards	loop if there are wildcards
	ora	doDirectories	  or doing directories
	jne	sd1a
;
;  Finished with this directory:  close it
;
cl1	lda	gdRefNum	close the directory
	sta	clRefNum
	~OSClose clRec
;
;  If we are doing a directory, and directories appear first in
;  the file list, add the directory to the file list
;
	lda	fileNamePtr	if we are doing a directory then
	ora	fileNamePtr+2
	bne	pd3
	lda	doDirLast	  if directories come last then
	beq	pd3
	jsr	AddDirectory	    add it to the list
pd3	anop
;
;  Dispose of the dynamic records
;
	free	epOut	Free(epOut)
	free	fileNamePtr	Free(fileNamePtr)
;
;  If there is a previous directory, pop its record and finish it
;
	lda	directoryList
	ora	directoryList+2
	beq	fn1
	jsr	PopDirectory
	brl	sd1a
;...............................................................
;
;  Terminal Processing
;
;...............................................................
;
;  Reverse the file list
;
fn1	move4 wcFileList,r0	r0 = wcFileList
	stz	r4	r4 = nil
	stz	r4+2
fn2	lda	r0	while r0 <> nil do
	ora	r0+2
	beq	fn3
	move4 r0,r8	  r8 = r0
	ldy	#2	  r0 = r8^.next
	lda	[r8]
	sta	r0
	lda	[r8],Y
	sta	r0+2
	lda	r4	  r8^.next = r4
	sta	[r8]
	lda	r4+2
	sta	[r8],Y
	move4 r8,r4	  r4 = r8
	bra	fn2	endwhile
fn3	move4 r4,wcFileList	wcFileList = r4
;
;  Return to the caller
;
	jsr	StopSpin
	return #0
;...............................................................
;
;  Return an out of memory error
;
;  Note: drops into the error return code
;
;...............................................................
;
oom	lda	#outOfMem
	sta	err
!	bra	error
;...............................................................
;
;  Clean up and do an error return
;
;...............................................................
;
error	free	epOut	Free(epOut)
	free	fileNamePtr	Free(fileNamePtr)

er2	lda	directoryList	while directoryList <> nil do
	ora	directoryList+2
	beq	er3
	lda	gdRefNum	  close the directory
	sta	clRefNum
	~OSClose clRec
	jsr	PopDirectory	  PopDirectory
	bra	er2	endwhile

er3	lda	gdRefNum	if a directory is open then
	beq	er4
	sta	clRefNum
	~OSClose clRec

er4	jsr	Spin	spin the spinner
	return err	return the error code
;...............................................................
;
;  AddDirectory - Add a directory to the list of matching file
;		names.  epOut points to the name of the
;		directory to add.
;
;...............................................................
;
AddDirectory anop
	lda	stop	if stop then
	beq	ad1
	rts		  return now

ad1	move4 epOut,r0	reserve memory for the directory name
	lda	[r0]
	jsl	Malloc
	sta	r8
	stx	r10
	ora	r10
	bne	ad2
	pla
	brl	oom
ad2	lda	[r0]	move the name to the new buffer
	tay
	dey
	short M
ad3	lda	[r0],Y
	sta	[r8],Y
	dey
	bpl	ad3
	long	M

	malloc #wcLength	get a record for the file name
	sta	r0
	stx	r2
	ora	r2
	bne	ad4
	free	r8
	pla
	brl	oom
ad4	lda	#12	get the info about the directory
	sta	wcNext+2
	stz	wcOptionList
	stz	wcOptionList+2
	add4	r8,#2,wcPath
	~OSGet_File_Info wcNext+2
	move4 dlBlocks,wcBlocksUsed	set the correct block count

	move4 wcFileList,wcNext	wcNext = wcFileList
	move4 r8,wcPath	wcPath = r8 (output format string)
	ldy	#wcLength-2	move the record to the dynamic area
ad5	lda	wcNext,Y
	sta	[r0],Y
	dey
	dey
	bpl	ad5
	move4 r0,wcFileList	place the file in the list
	rts
;...............................................................
;
;  AppendNames - appends the GS/OS output name pointed to by
;		gdName to the GS/OS output name pointed to by
;		epOut.  The buffer for gdName is then released.
;		The result is returned in r8.
;
;		If a memory allocation error occurs, the carry
;		flag is set on return.
;
;		A trailing ':' is added to the name for use in
;		directory allocations, but the length word is
;		set so that the trailing ':' is not normally
;		in the file name.
;
;...............................................................
;
AppendNames anop

	move4 epOut,r0	get space for new directory name
	move4 gdName,r4
	clc
	ldy	#2
	lda	[r0],Y
	adc	[r4],Y
	sta	r14
	adc	#5
	sta	r12
	jsl	Malloc
	sta	r8
	stx	r10
	ora	r10
	beq	anErr
	lda	r12	set the buffer length
	sta	[r8]
	ldy	#2	set the file length
	lda	r14
	sta	[r8],Y
	lda	[r0],Y	copy in the directory name
	tax
	ldy	#4
	short M
an1	lda	[r0],Y
	sta	[r8],Y
	iny
	dex
	bne	an1
	long	M
	ldy	#2	append the file name
	clc
	lda	[r0],Y
	adc	r8
	sta	r12
	lda	#0
	adc	r10
	sta	r14
an2	lda	[r4],Y
	tax
	ldy	#4
	short M
an3	lda	[r4],Y
	sta	[r12],Y
	iny
	dex
	bne	an3
	lda	#':'	append a trailing ':'
	sta	[r12],Y
	long	M
	free	gdName	Free(gdName)
	stz	gdName
	stz	gdName+2
	clc
	rts

anErr	sec		return with error
	rts
;...............................................................
;
;  DisposeOfFileList - Dump all existing files in the bit
;		bucket.
;
;...............................................................
;
DisposeOfFileList anop
do1	lda	wcFileList	while wcFileList <> nil do
	ora	wcFileList+2
	beq	do2
	lda	wcFileList+2	  r0 = wcFileList
	sta	r2	  Free(wcFileList^.pathName)
	lda	wcFileList
	sta	r0
	ldy	#6
	lda	[r0],Y
	tax
	dey
	dey
	lda	[r0],Y
	jsl	Free
	ldy	#$32-4	  Free(wcFileList^.optionList)
	lda	[r0],Y
	tax
	dey
	dey
	lda	[r0],Y
	jsl	Free
	ldy	#2	  wcFileList = wcFileList^.next
	lda	[r0]
	sta	wcFileList
	lda	[r0],Y
	sta	wcFileList+2
	free	r0	  Free(r0)
	brl	do1	endwhile
do2	rts
;...............................................................
;
;  NamesMatch - See if the file name pointed to by gdName
;		matches the wildcard specifier pointed to by
;		fileNamePtr.
;
;		If fileNamePtr is nil, all names match.
;
;		Returns carry set if the name matches.
;
;...............................................................
;
ptr1	equ	r0	pointer to pattern characters
ptr2	equ	r4	pointer to file name characters
l1	equ	r8	length of gdName
l2	equ	r10	length of fileNamePtr
p1	equ	r12	disp into gdName
p2	equ	r14	disp into fileNamePtr
tp1	equ	r16	temp disp into gdName
tp2	equ	r18	temp disp into fileNamePtr

NamesMatch anop
	lda	fileNamePtr	if (fileNamePtr == nil)
	ora	fileNamePtr+2
	jeq	nl10a	   goto YES;

	move4 gdName,ptr2	l2 = strlen(gdName);
	move4 fileNamePtr,ptr1	l1 = strlen(fileNamePtr);
	lda	[ptr1]
	sta	l1
	ldy	#2
	lda	[ptr2],Y
	sta	l2
	add4	ptr1,#2	ptr2 = gdName^.chars
	add4	ptr2,#4	ptr1 = fileNamePtr^.chars
	stz	p1	p1 = p2 = 0;
	stz	p2
	short M
nl1	anop		do {1:
	ldy	p1	  if (ptr1[p1] == '=')
	lda	[ptr1],Y
	cmp	#'='
	bne	nl7
	iny		    {++p1;
	sty	p1
	cpy	l1	    if (p1 == l1)
	jeq	nl10a	      goto YES;
nl2	anop		    do
	ldy	p1	      {tp1 = p1;
	sty	tp1
	ldy	p2	      tp2 = p2;
	sty	tp2
nl3	ldy	tp1	      while ((tp1!=l1)
	cpy	l1
	beq	nl4
	ldy	tp2		&&(tp2!=l2)
	cpy	l2
	beq	nl4
	lda	[ptr2],Y		&&(ptr1[tp1] == ptr2[tp2]))
	jsl	ToUpper
	sta	ch
	ldy	tp1
	lda	[ptr1],Y
	jsl	ToUpper
	cmp	ch
	bne	nl4
	iny			{++tp1; ++tp2;}
	sty	tp1
	ldy	tp2
	iny
	sty	tp2
	bra	nl3
nl4	ldx	tp1	      if ((tp1 == l1)
	cpx	l1
	bne	nl5
	ldy	tp2		&& (tp2 == l2))
	cpy	l2
	beq	nl10a		goto YES;
nl5	ldy	tp1	      else if (ptr1[tp1] == '=')
	lda	[ptr1],Y
	cmp	#'='
	bne	nl6
	sty	p1		{p1 = tp1; p2 = tp2; goto 1;)
	ldy	tp2
	sty	p2
	bra	nl1
nl6	ldy	p2	      ++p2;}
	iny
	sty	p2
!	ldy	p2	    while (p2 <= l2);
	cpy	l2
	ble	nl2
	brl	retfalse	    return FALSE;
nl7	ldy	p1	  else if (ptr1[p1] == ptr2[p2])
	lda	[ptr1],Y
	jsl	ToUpper
	sta	ch
	ldy	p2
	lda	[ptr2],Y
	jsl	ToUpper
	cmp	ch
	jne	retfalse
	ldy	p1	    {++p1; ++p2;}
	iny
	sty	p1
	ldy	p2
	iny
	sty	p2
!			  else return FALSE
nl9	anop		  }
	ldx	p1	while((p1 != l1) && (p2 != l2));
	cpx	l1
	beq	nl10
	ldx	p2
	cpx	l2
	jne	nl1
	ldy	p1	if (p1+1 == l1) && (ptr1[p1] == '=')
	lda	[ptr1],Y
	cmp	#'='
	bne	retfalse
	iny
	cpy	l1
	beq	nl10a	  goto YES;
retfalse long	M
	clc
	rts

nl10	long	M
	ldx	p2
	cpx	l2
	bne	retfalse
nl10a	long	M
	lda	prompt	if (prompt)
	jeq	nl13a
	ph4	epOut	  write file name;
	ph2	#$8000
	jsl	PrintOSName
	ph4	gdName
	ph2	#$8000
	jsl	PrintOSName
	sec
	lda	l2
	sbc	#17
	bmi	nl10b
	lda	#-1
nl10b	tax
nl11	phx
	lda	#' '
	jsl	~stdout
	plx
	inx
	bne	nl11
nl11a	gets	line2,cr=t	  get a character
	lda	line2+1
	and	#$00FF
	bne	nl11b
	jsr	DisposeOfFileList
	inc	stop
	putcr
	bra	nl12
nl11b	lda	line2+2
	and	#$007F
	jsl	ToUpper
	cmp	#'N'	  if its an N, reject
	beq	nl12
	cmp	#'Q'	  if its a Q, stop the search
	bne	nl13
nl11c	lda	#1
	sta	stop
nl12	clc
	rts
nl13	cmp	#'Y'	  if its a Y, accept
	bne	nl14
nl13a	lda	oneName	accept the file
	sta	stop
	sec
	rts
nl14	putcr
	puts	#'Please answer Y, N or Q: '
	brl	nl11a
;...............................................................
;
;  PopDirectory - pop a directory entry from the directory list
;
;...............................................................
;
PopDirectory anop
	move4 directoryList,r0	move the entry to the work record
	ldy	#dlLength-2
pp1	lda	[r0],Y
	sta	dlNext,Y
	dey
	dey
	bpl	pp1
	free	r0	free(r0)
	move4 dlNext,directoryList	restore in volatile information
	move4 dlDirName,epOut
	move4 dlFileName,fileNamePtr
	lda	dlDisplacement
	sta	gdDisplacement
	lda	dlRefNum
	sta	gdRefNum
	move4 dlGDRecName,gdName
	rts   
;...............................................................
;
;  Local variables
;
;...............................................................
;
ch	ds	2	uppercase character buffer
doDirectories ds 2	expand directories?
doDirLast ds	2	return direcory names after the files?
epInAllocated ds 2	was epIn dynamically allocated?
err	ds	2	error code
fileNamePtr ds 4	ptr to file name part of input name
line2	dc	i'2,0,0'	reply line
oneName	ds	2	return only the first name?
prompt	ds	2	prompt the user when a match is found?
promptFlag ds	2	is prompting allowed?
stop	ds	2	abort flag
wildcards ds	2	are there any wildcards in the file name?

epRec	dc	i'3'	expand path record
epIn	ds	4	input name pointer
epOut	ds	4	output name pointer
	dc	i'0'

opRec	dc	i'3'	GSOpen record
opRefNum ds	2
opPathName ds	4
	dc	i'$0001'	(read only access)

clRec	dc	i'1'	OSClose record
clRefNum ds	2

gdRec	dc	i'17'	OSGetDirEntry record
gdRefNum ds	2
gdFlags	ds	2
gdBase	ds	2
gdDisplacement ds 2
gdName	ds	4
gdEntryNum ds	2
gdFileType ds	2
gdEOF	ds	4
gdBlockCount ds 4
gdCreateDateTime ds 8
gdModDateTime ds 8
gdAccess ds	2
gdAuxType ds	4
gdFileSysID ds 2
gdOptionList ds 4
gdResourceEOF ds 4
gdResourceBlocks ds 4
;
;  directory list work record
;
directoryList ds 4	ptr to 1st directory list entry

dlLength equ	24	length of a record

dlNext	ds	4	next entry
dlDirName ds	4	directory name pointer
dlFileName ds	4	file name pointer
dlDisplacement ds 2	GetDirEntry displacement
dlRefNum ds	2	GetDirEntry refNum
dlGdRecName ds 4	ptr to gdName entry
dlBlocks ds	4	block count from GetDirEntry call
	end

****************************************************************
*
*  KeyPress - see if a keypress is available
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
KeyPress	private
	using Common
d_key	equ	0	disp to key code
d_modifiers equ 2	disp to modifiers code
d_available equ 4	disp to available flag

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	sp2
	lda	sh_dcb	  if pCount <> 3 then
	cmp	#3
	beq	sp1
	return #4	    return invalid pCount error
sp1	add4	sh_dadr,#2

sp2	ldy	#d_available	assume there is no key available
	lda	#0
	sta	[sh_dadr],Y
	pha		if the event manager is active then
	_EMStatus
	pla
	bcs	lb0
	beq	lb0
	pha		  if a keypress event is available then
	ph2	#$0028
	ph4	#event
	_EventAvail
	pla
	jeq	lb3
	ldy	#d_key	    set the keypress
	lda	message
	and	#$00FF
	sta	[sh_dadr],Y
	ldy	#d_modifiers	    set the modifiers flag
	lda	modifiers
	sta	[sh_dadr],Y
	ldy	#d_available	    set the available flag
	lda	#1
	sta	[sh_dadr],Y
	bra	lb3	  return

lb0	short M
	lda	>$C000	see if a key has been pressed
	bpl	lb1
	pha
	pha
	lda	>$C025
	long	M
	sta	modifiers	set up the modifiers flag
	lda	#0
	lsr	modifiers
	bcc	mm1
	ora	#$0200
mm1	lsr	modifiers
	bcc	mm2
	ora	#$1000
mm2	lsr	modifiers
	bcc	mm3
	ora	#$0400
mm3	lsr	modifiers
	lsr	modifiers
	bcc	mm4
	ora	#$2000
mm4	lsr	modifiers
	lsr	modifiers
	bcc	mm5
	ora	#$0800
mm5	lsr	modifiers
	bcc	mm6
	ora	#$0100
mm6	ldy	#d_modifiers
	sta	[sh_dadr],Y
	pla		set up the keypress
	and	#$007F
	ldy	#d_key
	sta	[sh_dadr],Y
	ldy	#d_available	set the available flag
	lda	#1
	sta	[sh_dadr],Y

lb1	long	M
lb3	return #0

event	anop		event record
what	ds	2
message	ds	4
when	ds	4
where	ds	4
modifiers ds	2
	end

****************************************************************
*
*  NextWildcard - Next wildcard
*
*  Inputs:
*	wcFileList - list of wildcard files
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
NextWildcard private
	using Common

!			level $00 call format
d_name	equ	0	disp to name

!			level $40 call format
e_pcount		  equ $00	parameter count
e_PathName	  equ $02	ptr to full path name
e_Access		  equ $06	access attribute
e_FileType	  equ $08	file type
e_AuxType 	  equ $0A	aux type
e_StorageType	  equ $0E	storage type
e_CreateDateTime equ $10	create date
e_ModDateTime	  equ $18	mod date
e_OptionList	  equ $20	option list pointer
e_EOF		  equ $24	EOF
e_BlocksUsed	  equ $28	blocks used
e_ResourceEOF	  equ $2C	resource fork EOF
e_ResourceBlocks equ $30	resource fork block count
;
;  Handle level 0 calls
;
	stz	err	assume no errors

	lda	sh_fnum	branch if this is not a level 0 call
	and	#$40
	bne	os1

	lda	wcFileList	if no files left then
	ora	wcFileList+2
	bne	nx1
	lda	#0	  return empty string
	sta	[sh_dcb+d_name]
	brl	tr2

nx1	jsr	GetEntry	get the file entry
	free	wcOptionList	we don't need this...
	ph2	wcPath+2	substitute / for :
	lda	wcPath
	inc	A
	inc	A
	pha
	jsl	ColonToSlash
	move4 wcPath,r0	flag an error if the name is too long
	add4	r0,#3,r4
	ldy	#2
	lda	[r0],Y
	cmp	#65
	blt	nx2
	lda	#$4F
	sta	err
	brl	tr1
nx2	tay		move in the file name
	short M
	sta	[sh_dcb+d_name]
nx3	lda	[r4],Y
	sta	[sh_dcb+d_name],Y
	dey
	bne	nx3
	long	M
	brl	tr1
;
;  Handle level $40 calls
;
os1	lda	sh_dcb+e_pcount	min parameter count is 1
	cmp	#13
	bge	os1a
	cmp	#1
	bge	os2
os1a	lda	#$53
	sta	err
	brl	tr2
os2	lda	wcFileList	if no files left then
	ora	wcFileList+2
	bne	os4
	lda	[sh_dcb+e_pathName]	  make sure the buffer is large enough
	cmp	#4
	bge	os3
e4F	lda	#$4F
	sta	err
	brl	tr2
os3	lda	#0	  return empty string
	ldy	#2
	sta	[sh_dcb+e_pathName],Y
	lda	[sh_dadr]	  if pCount >= 15 then
	cmp	#15
	jlt	tr2
	lda	sh_dcb+e_optionList	    if optionList <> nil then
	ora	sh_dcb+e_optionList+2
	jeq	tr2
	lda	[sh_dcb+e_optionList]	      if the length field is long enough then
	cmp	#4
	jlt	tr2
	lda	#0		set length to 0
	ldy	#2
	sta	[sh_dcb+e_optionList],Y
	brl	tr2

os4	jsr	GetEntry	get the file entry
	move4 wcPath,r0	make sure the file buffer is large enough
	ldy	#2
	lda	[r0],Y
	clc
	adc	#3
	cmp	[sh_dcb+e_pathName]
	bge	e4F
	tay		move the name to the output buffer
	short M
os5	lda	[r0],Y
	sta	[sh_dcb+e_pathName],Y
	dey
	cpy	#1
	bne	os5
	long	M
	lda	sh_dcb+e_optionList	if optionList <> nil
	ora	sh_dcb+e_optionList+2
	beq	os5c
	lda	sh_dcb+e_pcount	  and pcount >= 8 then
	cmp	#8
	blt	os5c
	lda	wcOptionList	  make sure the options buffer is large
	sta	r0	   enough
	lda	wcOptionList+2
	sta	r2
	ora	r0
	bne	os5aa
	lla	r0,zeroList
os5aa	ldy	#2
	lda	[r0],Y
	clc
	adc	#3
	cmp	[sh_dcb+e_optionList]
	blt	os5a
	brl	e4f
os5a	tay		  move the option list to the output
	short M	   buffer
os5b	lda	[r0],Y
	sta	[sh_dcb+e_optionList],Y
	dey
	cpy	#1
	bne	os5b
	long	M
os5c	anop		endif
	free	wcOptionList	done with this
	move4 sh_dcb+e_optionList,wcOptionList make sure we return the right value
	lda	sh_dcb+e_pcount	move in any other requested areas
	asl	A
	tay
	lda	wordCount,Y
	tay
	bra	os7
os6	lda	wcNext+2,Y
	sta	[sh_dadr],Y
	dey
	dey
os7	cpy	#4
	bne	os6
;
;  Terminal Processing
;
tr1	free	wcPath	dispose of the path name buffer
tr2	return err	return the error code
;...............................................................
;
;  GetEntry - Get the next wildcard file entry
;
;...............................................................
;
GetEntry anop

	move4 wcFileList,r0	r0 = wcFileList
	ldy	#wcLength-2	wc = r0^
ge1	lda	[r0],Y
	sta	wcNext,Y
	dey
	dey
	bpl	ge1
	move4 wcNext,wcFileList	wcFileList = wc.next
	free	r0	Free(r0)
	rts
;
;  local data
;
err	ds	2	error number
zeroList	dc	i'4,0'	zero OSString (for optionList)

wordCount dc	i'4,4,6,8,12,14,22,30,34,38,42,46,50'
	end

****************************************************************
*
*  PopVariables - Pop a variable table
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
PopVariables private
	using Common
	using RecCom
d_level	equ	0	disp to level
;
;  Initialization
;
	lda	sh_fnum	if the call is a level $40 call then
	and	#$40
	beq	in1
	lda	sh_dcb	  if pCount <> 0 then
	beq	in1
	return #$4	    return a bad pcount error

in1	lda	v_next	make sure there is a level to pop
	ora	v_next+2
	bne	pp1
	return #$FFFF
;
;  Dispose of all current nodes
;
pp1	jsr	FirstVariable	get the first entry

pp2	lda	r0	quit if no more
	ora	r0+2
	beq	pp3
	move4 r0,p1	save the node pointer
	free	r4	dispose of the name
	free	r8	dispose of the string
	jsr	NextVariable	next entry
	free	p1	dispose of the node
	bra	pp2

pp3	move4 v_next,r0	set new node
	ldy	#6	copy current info
pp4	lda	[r0],Y
	sta	v_next,Y
	dey
	dbpl	Y,pp4
	free	r0	dispose of old node
	return #0
	end

****************************************************************
*
*  PushVariables - Push a variable table
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
PushVariables private
	using Common
	using RecCom
v_node_size	equ   8

	lda	sh_fnum	if the call is a level $40 call then
	and	#$40
	beq	in1
	lda	sh_dcb	  if pCount <> 0 then
	beq	in1
	return #$4	    return a bad pcount error

in1	malloc #v_node_size	get 8 bytes to save the node
	sta	r0
	stx	r2
	ora	r2
	bne	ps1
	lda	#outOfMem
	sta	error
	bra	ps3

ps1	ldy	#v_node_size-2	copy current info
ps2	lda	v_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,ps2
	move4 r0,v_next	set the next pointer
	jsr	CopyVariables	export the variables
	sta	error
ps3	return #0

error	ds	2	error number
	end

****************************************************************
*
*  ReadIndexed - Read an indexed variable
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
ReadIndexed private
	using Common
d_name	equ	0	disp to name pointer
d_string equ	4	disp to string pointer
d_index	equ	8	disp to index value
d_export equ	10	disp to export flag
;
;  Initialization
;
	stz	error	no error so far

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb3
	lda	sh_dcb	  if pCount <> 4 then
	cmp	#4
	beq	lb1
	return #4	    return invalid pCount error
lb1	ldx	#0	  change the call to a class 0 call
	ldy	#6
lb2	lda	sh_dcb+2,X
	sta	sh_dcb,X
	inx
	inx
	dey
	bne	lb2
	add4	sh_dadr,#2

lb3	lda	sh_dcb+d_index	check for 0 index value
	beq	err
	jsr	FirstVariable

rd1	lda	r0	find the variable
	ora	r0+2
	jeq	err
	dec	sh_dcb+d_index
	beq	rd2
	jsr	NextVariable
	bra	rd1

rd2	lda	sh_fnum	split on call level
	and	#$40
	beq	rd5
;
;  Class 1 call
;
	ldy	#12	return the export flag
	lda	[r0],Y
	ldy	#d_export
	sta	[sh_dadr],Y
	ph4	r4	return the name
	ph4	sh_dcb+d_name
	jsr	ReturnOSString
	sta	error
	ph4	r8	return a pointer to the value
	ph4	sh_dcb+d_string
	jsr	ReturnOSString
	ora	error
	sta	error
	bra	rts
;
;  Class 0 call
;
rd5	ph4	r4	copy over the name converting it to
	ph4	sh_dcb+d_name	  pascal format
	ph2	#0
	jsr	ReturnPString
	sta	error
	ph4	r8	copy over the value converting it to
	ph4	sh_dcb+d_string	  pascal format
	ph2	#0
	jsr	ReturnPString
	ora	error
	sta	error
	bra	rts
;
;  No string - return null strings
;
err	lda	sh_fnum	split on call level
	and	#$40
	beq	err1

	ph4	#null	return null for the name
	ph4	sh_dcb+d_name
	jsr	ReturnOSString
	sta	error
	ph4	#null	return null for the value
	ph4	sh_dcb+d_string
	jsr	ReturnOSString
	ora	error
	sta	error
	bra	rts

err1	short M
	lda	#0	set string and name to null
	sta	[sh_dcb+d_name]
	sta	[sh_dcb+d_string]
	long	M

rts	return error

error	ds	2	error number
null	dc	i'0'	null string
	end

****************************************************************
*
*  ReadKey - get a keypress
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
ReadKey	private
	using Common
d_key	equ	0	disp to key code
d_modifiers equ 2	disp to modifiers code

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	sp2
	lda	sh_dcb	  if pCount <> 2 then
	cmp	#2
	beq	sp1
	return #4	    return invalid pCount error
sp1	add4	sh_dadr,#2

sp2	pha		if the event manager is active then
	_EMStatus
	pla
	bcs	lb0
	beq	lb0
sp3	pha		  if a keypress event is available then
	ph2	#$0028
	ph4	#event
	_GetNextEvent
	pla
	beq	sp3
	ldy	#d_key	    set the keypress
	lda	message
	and	#$00FF
	sta	[sh_dadr],Y
	ldy	#d_modifiers	    set the modifiers flag
	lda	modifiers
	sta	[sh_dadr],Y
	bra	lb3	  return

lb0	short M
lb1	lda	>$C000	wait for a key
	bpl	lb1
	pha
	pha
	lda	>$C025
	sta	>$C010
	long	M
	sta	modifiers	set up the modifiers flag
	lda	#0
	lsr	modifiers
	bcc	mm1
	ora	#$0200
mm1	lsr	modifiers
	bcc	mm2
	ora	#$1000
mm2	lsr	modifiers
	bcc	mm3
	ora	#$0400
mm3	lsr	modifiers
	lsr	modifiers
	bcc	mm4
	ora	#$2000
mm4	lsr	modifiers
	lsr	modifiers
	bcc	mm5
	ora	#$0800
mm5	lsr	modifiers
	bcc	mm6
	ora	#$0100
mm6	ldy	#d_modifiers
	sta	[sh_dadr],Y
	pla		set up the keypress
	and	#$007F
	ldy	#d_key
	sta	[sh_dadr],Y

lb3	return #0

event	anop		event record
what	ds	2
message	ds	4
when	ds	4
where	ds	4
modifiers ds	2
	end

****************************************************************
*
*  ReadVariable - Read a varaible
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
ReadVariable private
	using Common
d_name	equ	0	disp to name pointer
d_string equ	4	disp to string pointer
d_export equ	8	disp to export flag

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	rd1
	lda	sh_dcb	  if pCount <> 3 then
	cmp	#3
	beq	lb1
	return #4	    return invalid pCount error
lb1	ldx	#0	  change the call to a class 0 call
	ldy	#5
lb2	lda	sh_dcb+2,X
	sta	sh_dcb,X
	inx
	inx
	dey
	bne	lb2
	add4	sh_dadr,#2
	bra	rd3

rd1	ph4	sh_dcb+d_name	convert the name to a GS/OS string
	jsl	PtoOSString
	sta	sh_dcb+d_name
	stx	sh_dcb+d_name+2
	ora	sh_dcb+d_name+2
	bne	rd3
	return #outOfMem

rd3	move4 sh_dcb+d_name,r12	get the pointer to the name
	jsr	FindVariable	find the variable
	lda	r0
	ora	r0+2
	bne	rd4
rd3a	lla	r8,null	not found so return null string

rd4	lda	sh_fnum	split on call level
	and	#$40
	beq	rd5
	ldy	#12	return the export flag
	lda	[r0],Y
	ldy	#d_export
	sta	[sh_dadr],Y
	ph4	r8	return the variable
	ph4	sh_dcb+d_string
	jsr	ReturnOSString
	sta	error
	bra	rts

rd5	ph4	r8	return the variable
	ph4	sh_dcb+d_string
	ph2	#0
	jsr	ReturnPString
	sta	error
	free	sh_dcb+d_name	free the dynamic buffer

rts	return error

error	ds	2	error number
null	dc	i'0'	null string
	end

****************************************************************
*
*  Redirect - Redirect input/output
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Redirect private
	using Common
	using IOCom
e_pCount equ	0	disp to parameter count
e_devc	equ	2	disp to device number
e_append equ	4	disp to append flag
e_name	equ	6	disp to name pointer

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	rd2
	lda	sh_dcb+e_pCount	  if pCount <> 3 then
	cmp	#3
	beq	rd1
	return #4	    return invalid pCount error

rd1	ph4	sh_dcb+e_name	  duplicate the name
	jsl	DuplicateOSString
	bra	rd3

rd2	sub4	sh_dadr,#2	else it is a class 0 call -- change it
	ph4	sh_dcb-2+e_name
	lda	sh_dcb+e_append-2
	sta	sh_dcb+e_append
	lda	sh_dcb+e_devc-2
	sta	sh_dcb+e_devc
	jsl	PtoOSString
rd3	sta	sh_dcb+e_name	save the name ptr & check for errors
	stx	sh_dcb+e_name+2
	ora	sh_dcb+e_name+2
	bne	rd4
	return #outOfMem

rd4	~OSGet_level level	get the old file level
	~OSSet_level zero	set to shell level
	la	direction,2	if file name is .console then
	ph4	#console
	ph4	sh_dcb+e_name
	jsl	CompareOSStrings
	tax
	bne	lb1
	stz	direction	  direction = 0
!			else
!			  direction = 2
lb1	anop		endif

	lda	sh_dcb+e_devc	if device is input then
	bne	lb3
	jsr	CloseStin	  close old device
	move	direction,stin_direction	  set input direction
	move4 sh_dcb+e_name,stIn_fName	  set input file name
	jsr	RedirectStin	  redirect input
	bra	lb6

lb3	dec	A	else if device is standard out then
	bne	lb4
	jsr	CloseStout	  close old device
	move	direction,stout_direction  set direction
	move	sh_dcb+e_append,stout_append set append flag
	move4 sh_dcb+e_name,stOut_fName  set file name
	jsr	RedirectStout	  redirect output
	bra	lb6

lb4	dec	A	else if device is error out then
	bne	lb5
	jsr	CloseErrout	  close old device
	move	direction,errout_direction set direction
	move	sh_dcb+e_append,erroutappend set append flag
	move4 sh_dcb+e_name,errOut_fName set file name
	jsr	RedirectErrout	  redirect error out
	bra	lb6	else
lb5	la	o_error,$53	  bad parameter error
	bra	lb7	endif
lb6	stz	o_error	return with no error
lb7	~OSSet_Level level	reset file level
	return o_error
;
;  Local data
;
direction ds	2	direction, based on name
level	dc	i'1,0'	file level of caller
o_error	ds	2	error number
zero	dc	i'1,0'	zero for setting to shell level
	end

****************************************************************
*
*  SetDevices - Set console I/O device info
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
SetDevices private
	using Common
	using IOCom

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	gt1
	lda	sh_dcb	  if pCount <> 6 then
	cmp	#6
	beq	lb1
	return #4	    return invalid pCount error
lb1	add4	sh_dadr,#2

gt1	ldy	#16	copy the info
gt2	lda	[sh_dadr],Y
	sta	devstout,Y
	dey
	dey
	bpl	gt2
	return #0
	end

****************************************************************
*
*  SetLanguage - Set language
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
SetLanguage private
	using Common
d_lnum	equ	0	disp to language number

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	st1
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error
lb1	lda	sh_dcb+2
	sta	sh_dcb+d_lnum

st1	lda	sh_dcb+d_lnum
	pha
	ora	#$8000
	jsr	FindCommand2
	pla
	bcc	st2
	sta	clang
	return #0

st2	return #$FFFF
	end

****************************************************************
*
*  SetLInfo - Set L Info
*
*  DCB:
*       Disp	 Length  Name    Use
*       ====	 ======  ====    ===
*       0 	 4       SADDR   addr of source file name
*       4 	 4       OADDR   addr of output file name
*       8 	 4       PADDR   addr of parameter list
*       12	 4       LADDR   addr of language specific input string
*       16	 1       MERR    max error allowed
*       17	 1       MERRF   max error found
*       18	 1       OPS     operation flag
*       19	 1       KEEP    keep flag
*       20	 4       MINUS   set of letters selected with -
*       24	 4       PLUS    set of letters selected with +
*       28	 4       ORG     origin
*
****************************************************************
*
SetLInfo private
	using Common
d_saddr	equ	0	displacements info LInfo field
d_oaddr	equ	4	
d_paddr	equ	8	
d_laddr	equ	12	
d_merr	equ	16	
d_merrf	equ	17	
d_ops	equ	18	
d_keep	equ	19	
d_minus	equ	20	
d_plus	equ	24	
d_org	equ	28

	free	li_saddr	free the old names
	free	li_oaddr
	free	li_paddr
	stz	error	no errors

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	lb2
	add4	sh_dadr,#2	  skip the parameter count
	lda	sh_dcb	  make sure the parameter count is 11
	cmp	#11
	beq	lb1
	lda	#4
	sta	error
lb1	ph4	sh_dcb+d_saddr+2	  duplicate the source name
	jsl	DuplicateOSString
	sta	li_saddr
	stx	li_saddr+2
	ph4	sh_dcb+d_oaddr+2	  duplicate the output name
	jsl	DuplicateOSString
	sta	li_oaddr
	stx	li_oaddr+2
	ph4	sh_dcb+d_paddr+2	  duplicate the parameter list
	jsl	DuplicateOSString
	sta	li_paddr
	stx	li_paddr+2
	bra	lb3

lb2	anop		else {class 0 call}
	ph4	sh_dcb+d_saddr	  duplicate the source name
	jsl	PtoOSString
	sta	li_saddr
	stx	li_saddr+2
	ph4	sh_dcb+d_oaddr	  duplicate the output name
	jsl	PtoOSString
	sta	li_oaddr
	stx	li_oaddr+2
	ph4	sh_dcb+d_paddr	  duplicate the parameter list
	jsl	PtoOSString
	sta	li_paddr
	stx	li_paddr+2

lb3	ph4	li_saddr	make sure we are using colons
	jsl	SlashToColon
	ph4	li_oaddr
	jsl	SlashToColon
	lda	error	check for errors
	bne	lb5
	lda	li_saddr
	ora	li_saddr+2
	beq	lb4
	lda	li_oaddr
	ora	li_oaddr+2
	beq	lb4
	lda	li_paddr
	ora	li_paddr+2
	bne	lb5
lb4	lda	#$0201
	sta	error

lb5	short M	save the parameters
	ldy	#d_org+3
lb6	lda	[sh_dadr],Y
	sta	li_saddr,Y
	dey
	cpy	#d_merr
	bge	lb6
	long	M

	return error

error	ds	2	error number
	end

****************************************************************
*
*  SetStop - Set the stop flag
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
SetStop	private
	using Common
d_stop	equ	0	disp to language number

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	st1
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error

lb1	lda	sh_dcb+d_stop+2
	bra	st2

st1	lda	sh_dcb+d_stop
st2	sta	stop_flag
	return #0
	end

****************************************************************
*
*  SetVariable - Set a variable
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
SetVariable private
	using Common
d_name	equ	0	disp to name pointer
d_string equ	4	disp to string pointer
d_export equ	6	disp to export flag
;
;  Initialization
;
	lda	#1	note that a variable has been set
	sta	setUsed
	stz	error	no error so far
	stz	p16	p16 = null
	stz	p18
	stz	p12	p12 = null
	stz	p14
	lda	sh_fnum	split on call level
	and	#$40
	beq	cr2
;
;  Class 1 call
;
	lda	sh_dcb	make sure pCount = 3
	cmp	#3
	beq	os1
	lda	#4
	sta	error
	bra	rts

os1	ldx	#8	for L1 just call directly
os2	lda	sh_dcb+2,X
	sta	r12,X
	dex
	dbpl	X,os2
	jsr	RSetVariable	set the value
	bra	chk
;
;  Class 0 call
;
cr2	ph4	sh_dcb+d_name	convert name
	jsl	PtoOSString
	sta	r12
	sta	p12
	stx	r14
	stx	p14
	ora	r14
	beq	oom
	ph4	sh_dcb+d_string	convert value
	jsl	PtoOSString
	sta	r16
	sta	p16
	stx	r18
	stx	p18
	ora	r18
	beq	oom
	stz	r20	do not export
	jsr	RSetVariable
chk	bcc	rts
oom	lda	#outOfMem
	sta	error
rts	free	p12	free dynamic buffers
	free	p16
	return error

error	ds	2	error code
p12	ds	2	pointers (for use when free is called)
p14	ds	2
p16	ds	2
p18	ds	2
	end

****************************************************************
*
*  Stop - Stop
*
*  Inputs:
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Stop	private
	using Common
d_stop	equ	0	disp to stop flag

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	sp2
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	sp1
	return #4	    return invalid pCount error
sp1	add4	sh_dadr,#2

sp2	pha		if the event manager is active then
	_EMStatus
	pla
	bcs	lb0
	beq	lb0
	pha		  if a keypress event is available then
	ph2	#$0028
	ph4	#event
	_EventAvail
	pla
	beq	lb0
	lda	message	    if it's open-apple . then
	cmp	#'.'
	bne	lb0
	lda	modifiers
	and	#$0100
	beq	lb0
	pha		      get the event
	ph2	#$0028
	ph4	#event
	_GetNextEvent
	pla
	lda	#1	      set the stop flag
	ldy	#d_stop
	bra	lb2

lb0	ldy	#d_stop
	short M
	lda	>$C000	see if open-apple . has been pressed
	bpl	lb1
	cmp	#$80+'.'
	bne	lb1
	lda	>$C025
	bpl	lb1
	sta	>$C010	clear strobe
	lda	#1	set stop flag
	bra	lb2
lb1	lda	#0	clear stop flag
lb2	long	M
	and	#$00FF
	sta	[sh_dadr],Y
	beq	lb3
	sta	stop_flag	set stop flag to true
lb3	return #0

event	anop		event record
what	ds	2
message	ds	4
when	ds	4
where	ds	4
modifiers ds	2
	end

****************************************************************
*
*  Unset - Unset a variable
*
*  Inputs:
*	SH_FNUM - call number
*	SH_DADR - address of original DCB
*	SH_DCB - copy of call DCB
*
****************************************************************
*
Unset	private
	using Common
	using reccom
d_name	equ	0	disp to name pointer

	stz	error	no error so far
	lda	sh_fnum	split on call level
	and	#$40
	beq	us1
	lda	sh_dcb	make sure pCount = 1
	cmp	#1
	beq	os1
	return #4

os1	move4 sh_dcb+d_name+2,r12	get the pointer to the name
	stz	allocated	no buffer allocated
	bra	us2

us1	ph4	sh_dcb+d_name	convert the name to an OS string
	jsl	PtoOSString
	sta	sh_dcb+d_name
	sta	r12
	stx	sh_dcb+d_name+2
	stx	r14
	lda	#1	note that we allocated the buffer
	sta	allocated

us2	jsr	RUnset	unset the variable

rts	lda	allocated	if allocated then
	beq	rts1
	free	sh_dcb+d_name	  free(d_name)
rts1	return error

allocated ds	2	was d_name dynamically allocated
error	ds	2	error code
	end

****************************************************************
*
*  Version - Version Number
*
*  Inputs:
*	SH_DADR - address of original DCB
*
****************************************************************
*
Version	private
	using Common

	lda	sh_fnum	if this is a class 1 call then
	and	#$40
	beq	vr1
	lda	sh_dcb	  if pCount <> 1 then
	cmp	#1
	beq	lb1
	return #4	    return invalid pCount error
lb1	add4	sh_dadr,#2

vr1	lda	v
	sta	[sh_dadr]
	lda	v+2
	ldy	#2
	sta	[sh_dadr],Y
	return #0

v	dc	c'204 '
	end

****************************************************************
*
*  CleanUp - Clean up for Reset
*
*  Inputs:
*	RESET_APP - in application flag
*
*  Outputs:
*	1) shuts down application and disposes of it's memory
*	2) closes all open files above the shell level
*
****************************************************************
*
CleanUp	private
	debug CleanUp
	using common
	using reccom

	lda	reset_app	if in application and the quit vector
	beq	cl1	  has not been redirected then
	lda	mntr_vector+1
	cmp	#mntr_entry
	bne	cl1
	lda	mntr_vector+2
	cmp	#>mntr_entry
	bne	cl1

	lda	application_id	shut down the application
	pha
	pha
	ph2	#0
	_UserShutDown
	pla
	disposeall application_id	get rid of applications memory
	stz	application_id
	stz	reset_app	not in application

cl1	lda	#1	close all open application files
	sta	level
	~OSSet_Level lvRec
	~OSClose clRec
	stz	level
	~OSSet_Level lvRec

cl3	lda	exec_next	pop any exec stacks
	ora	exec_next+2
	beq	cl4
	jsr	PopExec
	bra	cl3

cl4	lda	ex_next	pop any exec file lists
	ora	ex_next+2
	beq	cl5
	jsr	PopScript
	bra	cl4

cl5	lda	ins_next	pop any instruction stacks
	ora	ins_next+2
	beq	cl6
	jsr	PopInstruction
	bra	cl5

cl6	lda	v_next	pop any variable tables
	ora	v_next+2
	beq	cl7
	PopVariables 0
	bra	cl6

cl7	lda	a_next	pop any alias tables
	ora	a_next+2
	beq	cl8
	jsr	PopAlias
	bra	cl7

cl8	lda	type_ptr	dispose of any type buffer open
	ora	type_ptr+2
	beq	cl9
	free	type_ptr
	stz	type_ptr
	stz	type_ptr+2

cl9	jsr	PullLine	pop any command line records
	bcs	cl9

cl10	jsr	PopRedirection	pop any I/O records
	bcs	cl10

	stz	skipping	not skipping
	rts

lvRec	dc	i'1'	OSSet_Level, OSGet_Level record
level	ds	2

clRec	dc	i'1,0'	OSClose record
	end

****************************************************************
*
*  CopyVariables - Copy a variable table
*
*  Inputs:
*	V_POINTER - pointer to current table
*
*  Outputs:
*	V_POINTER - pointer to new table
*	A - error code
*
****************************************************************
*
CopyVariables private
	debug CopyVariables
	using Common
	using RecCom

	jsr	FirstVariable	get the first node
	stz	v_pointer	initialize new table
	stz	v_pointer+2

cp1	lda	r0	quit if at end of list
	ora	r0+2
	beq	rts
	ldy	#12	see if this is exportable
	lda	[r0],Y
	beq	cp2
	sta	r20
	move4 r4,r12
	move4 r8,r16
	ph4	r0	save list pointer
	jsr	RSetVariable	set the variable
	bcs	err
	pl4	r0	get list pointer
cp2	jsr	NextVariable	next node
	bra	cp1
rts	lda	#0
	rts

err	pla		return with out of memory error
	pla
	lda	#outOfMem
	rts
	end

****************************************************************
*
*  InsertVariable - Insert a variable into the table
*
*  Inputs:
*	R0 - pointer to variable node
*
****************************************************************
*
InsertVariable private
	debug InsertVariable
	using Common
	using RecCom

	lda	v_pointer	if V_POINTER = nil then
	ora	v_pointer+2
	bne	in1
	move4 r0,v_pointer	  V_POINTER = R0
	rts

in1	ldy	#2	else
	move4 v_pointer,r4	  R4 = V_POINTER

in2	lda	[r4]	  while R4^next != nil
	ora	[r4],Y
	beq	in3
	lda	[r4]	    R4 = R4^next
	tax
	lda	[r4],Y
	sta	r4+2
	stx	r4
	bra	in2
in3	lda	r0	  R4^next = R0
	sta	[r4]
	lda	r0+2
	sta	[r4],Y
	rts
	end

****************************************************************
*
*  OStoProDOSDate - change a GS/OS date to a ProDOS date
*
*  Inputs:
*	r0-r7 - GS/OS date
*
*  Outputs:
*	X-A - ProDOS date
*
****************************************************************
*
OStoProDOSDate private

	short I,M
	lda	r3	year
	sta	ProDOSDate+1
	lda	r5	month
	inc	A
	asl	A
	asl	A
	asl	A
	asl	A
	asl	A
	rol	ProDOSDate+1
	inc	r4	month/day
	ora	r4
	sta	ProDOSDate
	lda	r2	hour
	sta	ProDOSDate+3
	lda	r1	minute
	sta	ProDOSDate+2
	long	I,M
	ldx	ProDOSDate+2
	lda	ProDOSDate
	rts

ProDOSDate ds	4
	end

****************************************************************
*
*  PopExec - Pop an exec file entry
*
*  Inputs:
*	EXEC_NEXT - exec node
*
*  Outputs:
*	EXEC_NEXT - pointer to next record
*	EXEC_PTR - pointer to exec string
*	EXEC_DISP - displacment into string
*
****************************************************************
*
PopExec	private
	debug PopExec
	using Common
	using RecCom

	lda	exec_next	make sure there is stuff to pop
	ora	exec_next+2
	beq	rts
	move4 exec_next,r0	get the next node pointer
	ldy	#8	copy current info
pp1	lda	[r0],Y
	sta	exec_next,Y
	deY
	dbpl	Y,pp1
	free	r0	dispose of this node
rts	rts
	end

****************************************************************
*
*  PopInstruction - Pop an instruction stack record
*
*  Inputs:
*	INS_NEXT- instruction stack node
*
*  Outputs:
*	INS_NEXT - pointer to next record
*	INS_PTR - pointer to instruction stack
*	INS_DISP - displacement into stack
*	INS_SIZE - stack size
*
****************************************************************
*
PopInstruction private
	debug PopInstruction
	using Common
	using RecCom
;
;  Pop the individual records from the stack
;
	lda	ins_next	make sure there is stuff to pop
	ora	ins_next+2
	beq	rts

lb1	lda	ins_disp	while there is stuff on the stack do
	beq	lb4

	jsr	PopStack	get top of stack type
	cmp	#loopsy	if loopsy then
	beq	lb1	  next item

	cmp	#ifsy	else if ifsy then
	beq	lb1	  next item

	cmp	#forsy	else if forsy then
	bne	lb4
lb2	jsr	TopType	  while TopType <> forsy do
	cmp	#forsy
	beq	lb3
	jsr	PopWord	    free(PopWord)
	free	r0
	bra	lb2
lb3	jsr	PopStack	  PopStack
	bra	lb1
;
;  Pop the record
;
lb4	free	ins_ptr	dispose the the stack
	move4 ins_next,r0	get the next node pointer
	ldy	#10	copy current info
pp1	lda	[r0],Y
	sta	ins_next,Y
	deY
	dbpl	Y,pp1
	free	r0	dispose of this node
rts	rts
	end

****************************************************************
*
*  ProDOStoOSDate - change a ProDOS date to a GS/OS date
*
*  Inputs:
*	r0-r3 - ProDOS date
*
*  Outputs:
*	r4-r11 - GS/OS date
*
****************************************************************
*
ProDOStoOSDate private

	short I,M
	lda	r0	year
	lsr	A
	sta	r7
	lda	r0	month
	lsr	A
	lda	r1
	ror	A
	lsr	A
	lsr	A
	lsr	A
	lsr	A
	sta	r9
	lda	r1	day
	and	#$1F
	dec	A
	sta	r8
	lda	r2	hour
	sta	r6
	lda	r3	minute
	sta	r5
	stz	r4	second
	stz	r10	null
	stz	r11	weekday
	long	I,M
	rts
	end

****************************************************************
*
*  PushExec - Push an exec file entry
*
*  Inputs:
*	EXEC_NEXT - exec node
*	EXEC_PTR
*	EXEC_DISP
*
*  Outputs:
*	C - clear is no error else set
*
****************************************************************
*
PushExec private
	debug PushExec
	using Common
	using RecCom

exec_node_size equ   10

	malloc #exec_node_size	get space for node
	sta	r0
	stx	r2
	ora	r2
	bne	ps1
	sec		error
	rts

ps1	ldy	#8	copy current info
ps2	lda	exec_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,ps2
	move4 r0,exec_next	set the next pointer
	clc
	rts
	end

****************************************************************
*
*  PushInstruction - Push an instruction stack record
*
*  Inputs:
*	INS_NEXT - instruction stack node
*	INS_PTR - pointer to instruction stack
*	INS_DISP - displacement into stack
*	INS_SIZE - stack size
*
*  Outputs:
*	C - clear is no error else set
*
****************************************************************
*
PushInstruction private
	debug PushInstruction
	using Common
	using RecCom

ins_node_size equ   12

	malloc #ins_node_size	get space for node
	sta	r0
	stx	r2
	ora	r2
	bne	ps1
	sec		error
	rts

ps1	ldy	#10	copy current info
ps2	lda	ins_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,ps2
	move4 r0,ins_next	set the next pointer
	clc
	rts
	end

****************************************************************
*
*  RSetVariable - Set a variable
*
*  Inputs:
*	R12 - adrress of variable name (OS string)
*	R16 - address of string (OS string)
*	R20 - export flag
*	C - set if error occurred
*
****************************************************************
*
RSetVariable private
	debug RSetVariable
	using Common

	lda	r16	if (r16 = nil) or (r16^ = 0) then
	ora	r18
	beq	us1
	lda	[r16]
	bne	us2
us1	jsr	RUnset	  unset the variable
	clc
	rts

us2	jsr	FindVariable	try to find the variable
	lda	r0
	ora	r0+2
	jeq	st3	if variable exists
	free	r8	  dispose of old string
	ph4	r16	  create a copy of the string
	jsl	DuplicateOSString
	sta	r8
	stx	r10
	ora	r10
	sec
	jeq	rts
	ldy	#8	  set the variable node
	lda	r8
	sta	[r0],Y
	iny
	iny
	lda	r10
	sta	[r0],Y
	ldy	#12
	lda	r20
	sta	[r0],Y
	clc
rts	rts
;
;  Create a new variable from scratch
;
st3	stz	r0	set pointers to nil
	stz	r2
	stz	r4
	stz	r6
	stz	r8
	stz	r10
	malloc #14	allocate the node
	sta	r0
	stx	r2
	ora	r2
	jeq	err
	ph4	r12	make a copy of the name
	jsl	DuplicateOSString
	sta	r4
	stx	r6
	ora	r6
	jeq	err
	ph4	r16	make a copy of the string
	jsl	DuplicateOSString
	sta	r8
	stx	r10
	ora	r10
	jeq	err

	lda	#0	next = nil
	sta	[r0]
	ldy	#2
	sta	[r0],Y
	ldy	#4	set the name pointer
	lda	r4
	sta	[r0],Y
	iny
	iny
	lda	r6
	sta	[r0],Y
	ldy	#8	set the string pointer
	lda	r8
	sta	[r0],Y
	iny
	iny
	lda	r10
	sta	[r0],Y
	ldy	#12	set the export flag
	lda	r20
	sta	[r0],Y
	jsr	InsertVariable	insert into variable table
	clc
	rts

err	free	r0	dispose of any space allocated
	free	r4
	free	r8
	sec
	rts
	end

****************************************************************
*
*  RUnset - unset a variable
*
*  Inputs:
*	r12 - address of the variable to unset
*
****************************************************************
*
RUnset	private
	using RecCom

	stz	error	no error so far
	jsr	FindVariable	find the variable
	lda	r0
	ora	r2
	bne	us3a
	dec	error
	bra	rts

us3a	lda	p1	remove node from the list
	ora	p1+2	if this is the only node then
	bne	us4
	ldy	#2	  V_POINTER = R0^next
	lda	[r0]
	sta	v_pointer
	lda	[r0],Y
	sta	v_pointer+2
	bra	us5
us4	ldy	#2	else
	lda	[r0]	  P1^next = R0^next
	sta	[p1]
	lda	[r0],Y
	sta	[p1],Y
us5	free	r0	dispose of the node
	free	r4	dispose of the name
	free	r8	dispose of the string

rts	lda	error	return the error
	rts

error	ds	2	error number
	end
