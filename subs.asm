	keep	obj/subs
	mcopy subs.macros
****************************************************************
*
*  Subs
*
*  This module contains general purpose and utility subroutine
*  used throughout the shell.
*
****************************************************************
	copy	sh.dp
	eject
****************************************************************
*
*  SpinnerCommon - common data area for the spinner subroutines
*
****************************************************************
*
SpinnerCommon privdata

spinSpeed equ	3	calls before one spinner move

spinning dc	i'0'	are we spinning now?
spinDisp dc	i'0'	disp to the spinner character
spinCount ds	2	spin loop counter

spinner	dc	i1'$7C,$2F,$2D,$5C'	spinner characters
	end

****************************************************************
*
*  shortName - used to find the length of a GS/OS name
*
****************************************************************
*
shortName private
buffSize equ	1	buffer size; should be zero

	dc	i'buffSize+4,0'
	ds	buffSize
	end

****************************************************************
*
*  shortName2 - used to find the length of a GS/OS option list
*
****************************************************************
*
shortName2 private
buffSize equ	1	buffer size; should be zero

	dc	i'buffSize+4,0'
	ds	buffSize
	end

****************************************************************
*
*  AppendOSNames - append two GS/OS path names
*
*  Inputs:
*	p1,p2 - pointers to input format GS/OS names
*	flags -
*		0 - return an input string
*		1 - return an output string
*
*  Outputs:
*	X-A - nil for out of memory, otherwise a pointer to the string
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the actual length of the expanded path name.  The
*	caller is responsible for disposing of the memory.
*
****************************************************************
*
AppendOSNames start seg2
out	equ	1	return pointer
chars	equ	5	pointer to input format portion of out

	lsub	(4:p1,4:p2,2:flags),8

	clc		find the length of the buffer
	lda	[p1]
	adc	[p2]
	inc	A
	inc	A
	ldx	flags
	beq	lb1
	inc	A
	inc	A
	sta	chars
lb1	jsl	Malloc	allocate the memory
	sta	out
	stx	out+2
	ora	out
	beq	lb7
	lda	flags	if flags then
	beq	lb2
	lda	chars	  set the length of the buffer
	sta	[out]
	add4	out,#2,chars	  chars = out+2
	bra	lb3	else
lb2	move4 out,chars	  chars = out
lb3	anop		endif
	clc		set the length
	lda	[p1]
	adc	[p2]
	sta	[chars]
	lda	[p1]	move in the first string
	beq	lb4a
	tax
	ldy	#2
	short M
lb4	lda	[p1],Y
	sta	[chars],Y
	iny
	dex
	bne	lb4
	long	M
lb4a	clc		update chars
	lda	[p1]
	adc	chars
	sta	chars
	bcc	lb5
	inc	chars+2
lb5	lda	[p2]	move in the second string
	beq	lb7
	tax
	ldy	#2
	short M
lb6	lda	[p2],Y
	sta	[chars],Y
	iny
	dex
	bne	lb6
	long	M

lb7	lret	4:out
	end

****************************************************************
*
*  ColonToSlash - substitutes slashes for colons in GS/OS path names
*
*  Inputs:
*	ptr - pointer to the GS/OS input name
*
****************************************************************
*
ColonToSlash start seg2

	lsub	(4:ptr),0

	lda	[ptr]
	beq	lb3
	tax
	ldy	#2
	short M
lb1	lda	[ptr],Y
	cmp	#':'
	bne	lb2
	lda	#'/'
	sta	[ptr],Y
lb2	iny
	dex
	bne	lb1
	long	M

lb3	lret
	end

****************************************************************
*
*  CompareOSStrings - compare two GS/OS path names
*
*  Inputs:
*	p1,p2 - pointers to input format GS/OS names
*	caseSensitive - are compares case sensitive?
*
*  Outputs:
*	A - 0 if equal, 1 if not equal
*
****************************************************************
*
CompareOSStrings start seg2
	using Common
val	equ	1	return value
ch	equ	3	temp character value

	lsub	(4:p1,4:p2),4

	lda	#1	assume the strings are not equal
	sta	val
	lda	[p1]	not equal if lengths differ
	cmp	[p2]
	bne	lb7
	tax
	beq	lb5

	short M
	ldy	#2	Y is disp into strings
	lda	>caseSensitive	if not case sensitive then
	bne	lb2
lb1	lda	[p1],Y	  do a case insensitive compare
	jsl	ToUpper
	sta	ch
	lda	[p2],Y
	jsl	ToUpper
	cmp	ch
	bne	lb6
	iny
	dex
	bne	lb1
	stz	val
	bra	lb6	else
lb2	lda	[p1],Y	  do a case sensitive compare
	cmp	[p2],Y
	bne	lb6
	iny
	dex
	bne	lb2
lb5	stz	val
lb6	long	M	endif

lb7	lret	2:val
	end

****************************************************************
*
*  ConvertPrefix - convert old to new prefixes
*
*  Inputs:
*	ptr - address of the input path name to convert
*
*  Outputs:
*	X-A - pointer to path name; nil if no conversion needed
*
*  Notes:
*	This subroutine checks to see if the path name starts
*	with a numbered prefix from 0-7.  If so, and if the
*	current prefix 0 is null, a path with the prefix
*	number converted to the new, GS/OS equivalent is
*	returned.  If no conversion is needed, a nil pointer
*	is returned.
*
*	If a pointer is returned, it has been allocated from
*	dynamic memory, and it is up to the caller to dispose
*	of the memory.
*
****************************************************************
*
ConvertPrefix private seg2
nptr	equ	1	new prefix pointer

	lsub	(4:ptr),4

	stz	nptr	assume no conversion is necessary
	stz	nptr+2
	lda	[ptr]	if length = 1
	beq	lb3
	cmp	#1
	beq	lb1
	ldy	#3	  or ptr^.string[2] in [':','/'] then
	lda	[ptr],Y
	and	#$00FF
	cmp	#'/'
	beq	lb1
	cmp	#':'
	bne	lb3
lb1	ldy	#2	  if ptr^.string[1] in ['0'..'7'] then
	lda	[ptr],Y
	and	#$00FF
	cmp	#'0'
	blt	lb3
	cmp	#'8'
	bge	lb3
	lda	#0	    if length(prefix(0)) = 0 then
	sta	>prName+2
	OSGet_Prefix prRec
	lda	>prName+2
	bne	lb3
	clc		      allocate space for the new buffer
	lda	[ptr]
	adc	#3
	jsl	Malloc
	sta	nptr
	stx	nptr+2
	ora	nptr+2
	beq	lb3
	lda	[ptr]	      set the length
	inc	A
	sta	[nptr]
	ldy	#2	      set the prefix number
	lda	[ptr],Y
	and	#$000F
	asl	A
	tax
	lda	>nums,X
	sta	[nptr],Y
	lda	[ptr]	      copy over the remaining chars
	dec	A
	beq	lb3
	tax
	ldy	#3
	short	M
lb2	lda	[ptr],Y
         iny
	sta	[nptr],Y
	dex
	bne	lb2
	long	M

lb3	lret	4:nptr	return the new prefix pointer

prRec	dc	i'2'	GetPrefix record
	dc	i'0'
	dc	a4'prName'

prName	dc	i'4,0'	prefix name buffer

nums	dc	c'08'	prefix equivalents table
	dc	c'09'
	dc	c'13'
	dc	c'14'
	dc	c'15'
	dc	c'16'
	dc	c'17'
	dc	c'18'
	end

****************************************************************
*
*  CtoOSString - convert a C string to an OS string
*
*  Inputs:
*	cstr - pointer to the C string
*
*  Outputs:
*	X-A - nil for out of memory, otherwise a pointer to the string
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the length of the string.  The caller is responsible
*	for disposing of the memory.
*
****************************************************************
*
CtoOSString start seg2
str	equ	1	pointer to duplicate string
str2	equ	7	pointer to duplicate string + 2
len	equ	5	length of the string

	lsub	(4:cstr),10

	ldy	#0	find the length of the C string
	short M
lb1	lda	[cstr],Y
	beq	lb2
	iny
	bne	lb1
lb2	long	M
	tya		allocate space for the string
	sta	len
	inc	A
	inc	A
	jsl	Malloc
	sta	str
	stx	str+2
	ora	str+2
	beq	lb5
	lda	len	set the length
	sta	[str]
	add4	str,#2,str2	copy over the string characters
	ldy	len
	beq	lb5
	short M
	dey
	beq	lb4
lb3	lda	[cstr],Y
	sta	[str2],Y
	dey
	bne	lb3
lb4	lda	[cstr]
	sta	[str2]
	long	M

lb5	lret	4:str
	end

****************************************************************
*
*  DuplicateOSString - create a duplicate of an O/S string
*
*  Inputs:
*	p - pointers to string to duplicate
*
*  Outputs:
*	X-A - nil for out of memory, otherwise a pointer to the string
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the length of the string.  The caller is responsible
*	for disposing of the memory.
*
****************************************************************
*
DuplicateOSString start seg2
str	equ	1	pointer to duplicate string

	lsub	(4:p),4

	lda	[p]	reserve a buffer
	inc	A
	inc	A
	jsl	Malloc
	sta	str
	stx	str+2
	ora	str+2	branch if out of memory
	beq	lb2
	lda	[p]	move the string
	inc	A
	tay
	short M
lb1	lda	[p],Y
	sta	[str],Y
	dey
	bpl	lb1
	long	M

lb2	lret	4:str
	end

****************************************************************
*
*  ExpandDevice - Expand devices (shell call)
*
*  Inputs:
*	dcb - address of the parameter block
*
*  Outputs:
*	A - error code; 0 if no error occurred
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the actual length of the expanded path name.  The
*	address of the buffer is returned in the appropriate
*	spot in the parameter block.  The caller is
*	responsible for disposing of the memory.  No memory
*	is reserved if an error occurs.
*
****************************************************************
*
ExpandDevice start
buffPtr	equ	1	pointer to the file name buffer
err	equ	5	error
pathPtr	equ	7	input path pointer

	sub	(4:dcb),10

	stz	err	assume we will succeed
         stz	pathPtr	pathPtr = nil
	stz	pathPtr+2

	ldy	#2	fetch the input path name
	lda	[dcb],Y
	sta	rec+2
	iny
	iny
	lda	[dcb],Y
	sta	rec+4
	ph4	rec+2	convert prefixes 0-7
	jsl	ConvertPrefix
	sta	pathPtr
	stx	pathPtr+2
	ora	pathPtr+2
	beq	cp1
	move4	pathPtr,rec+2

cp1	lda	#0	get the length of the buffer
	sta	shortName+2
	OSExpandDevices rec

	lda	shortName+2	reserve a block of memory
	bne	lb0
	lda	#$0044	no path - return a path not found error
	sta	err
	bra	lb2
lb0	clc
	adc	#4
	jsl	Malloc
	ldy	#6	save the pointer to the memory
	sta	[dcb],Y
	sta	buffPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	buffPtr+2
	ora	buffPtr	check for out of memory condition
	bne	lb1
	dec	err	(err = -1)
	bra	lb2

lb1	lda	shortName+2	set the length of the buffer
	clc
	adc	#4
	sta	[buffPtr]
	ldy	#4	save the input name pointer
	lda	[dcb],Y	set converted name pointer
	pha
	lda	rec+4
	sta	[dcb],Y
	dey
	dey
	lda	[dcb],Y
	pha
	lda	rec+2
	sta	[dcb],Y
	move4 dcb,addr	get the volume name
	jsl	$E100A8
	dc	i'$0154'
addr	ds	4
	bcc	lb1a	branch if no error occured
	sta	err	save the error number
	free	buffPtr	dispose of the buffer
	ldy	#6
	lda	#0
	sta	[dcb],Y
	iny
	iny
	sta	[dcb],Y
lb1a	ldy	#2	restore input name pointer
	pla
	sta	[dcb],Y
	iny
	iny
	pla
	sta	[dcb],Y
lb2	free	pathPtr	free converted path, if any
	ret	2:err	return the error code

rec	dc	i'2'	ExpandDevices record
	ds	4	input path name
	dc	a4'shortName'	output path name
	end

****************************************************************
*
*  ExpandPath - Expand a path name
*
*  Inputs:
*	dcb - address of the parameter block
*
*  Outputs:
*	A - error code; 0 if no error occurred
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the actual length of the expanded path name.  The
*	address of the buffer is returned in the appropriate
*	spot in the parameter block.  The caller is
*	responsible for disposing of the memory.  No memory
*	is reserved if an error occurs.
*
****************************************************************
*
ExpandPath start
buffPtr	equ	1	pointer to the file name buffer
err	equ	5	error
pathPtr	equ	7	input path pointer

	sub	(4:dcb),10

	stz	err	assume we will succeed
         stz	pathPtr	pathPtr = nil
	stz	pathPtr+2

	ldy	#2	fetch the input path name
	lda	[dcb],Y
	sta	rec+2
	iny
	iny
	lda	[dcb],Y
	sta	rec+4
	ph4	rec+2	convert prefixes 0-7
	jsl	ConvertPrefix
	sta	pathPtr
	stx	pathPtr+2
	ora	pathPtr+2
	beq	cp1
	move4	pathPtr,rec+2

cp1	lda	#0	get the length of the buffer
	sta	shortName+2
	OSExpand_Path rec
	bcc	la1
	cmp	#$4F
	beq	la1
	sta	err
	bra	lb2

la1	lda	shortName+2	reserve a block of memory
	clc
	adc	#4
	jsl	Malloc
	ldy	#6	save the pointer to the memory
	sta	[dcb],Y
	sta	buffPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	buffPtr+2
	ora	buffPtr	check for out of memory condition
	bne	lb1
	dec	err	(err = -1)
	bra	lb2

lb1	lda	shortName+2	set the length of the buffer
	clc
	adc	#4
	sta	[buffPtr]
	ldy	#4	save the input name pointer
	lda	[dcb],Y	set converted name pointer
	pha
	lda	rec+4
	sta	[dcb],Y
	dey
	dey
	lda	[dcb],Y
	pha
	lda	rec+2
	sta	[dcb],Y
	ph4	dcb	get the volume name
	ph2	#$200E
	jsl	$E100B0
	bcc	lb1a	branch if no error occured
	sta	err	save the error number
	free	buffPtr	dispose of the buffer
	ldy	#6
	lda	#0
	sta	[dcb],Y
	iny
	iny
	sta	[dcb],Y
lb1a	ldy	#2	restore input name pointer
	pla
	sta	[dcb],Y
	iny
	iny
	pla
	sta	[dcb],Y
lb2	free	pathPtr	free converted path, if any
	ret	2:err	return the error code

rec	dc	i'3'	ExpandPath record
	ds	4	input path name
	dc	a4'shortName'	output path name
	dc	i'0'	don't uppercase the name
	end

****************************************************************
*
*  FindAlias - find an alias
*  FindVaraible - find a variable
*
*  Inputs:
*	R12 - pointer to name of variable
*	V_POINTER - pointer to variable table
*
*  Outputs:
*	R0 - pointer to token entry (0 if no string found)
*	R4 - pointer to variable name
*	R8 - pointer to variable string
*	P1 - pointer to last node in list (used by unset, 0 if first entry)
*
****************************************************************
*
FindVariable start
	debug FindVariable
	using Common
	using RecCom

	jsr	FirstVariable	get the first variable entry
	bra	fv

FindAlias entry
	jsr	FirstAlias	get the first alias entry

fv	stz	p1	initialize last node
	stz	p1+2

fv1	lda	r0	quit if no more
	ora	r0+2
	beq	rts
	ph4	r4	see if names match
	ph4	r12
	jsl	CompareOSStrings
	tax
	beq	rts
	move4 r0,p1	next entry
	jsr	NextAlias
	bra	fv1

rts	rts
	end

****************************************************************
*
*  FindCommand - Identify a command
*
*  Inputs:
*	LINE - input line with the command as the first entry
*
*  Outputs:
*	A - command number; 0 for a language
*	Y - language number (if A=0)
*	C - set if the line contains a command
*	R0 - address of command table entry
*	CTLAST - pointer to command table entry of command, if found
*
****************************************************************
*
FindCommand start
	debug FindCommand
	using Common
;
;  Read the command name.
;
	stz	ctlast	ctlast = nil
	stz	ctlast+2
	jsr	SkipBlanks	remove leading blanks
	short M
	ldy	#2	place command in BUFF1
	ldx	#0
rc3	jsr	NextChar
	cmp	#RETURN
	beq	rc4
	jsl	IsWhite
	bcs	rc4
	jsl	ToUpper
	sta	buff1,Y
	iny
	cpy	#16
	blt	rc3
	bra	err
rc4	dey		save length of command
	dey
	sty	buff1
	long	M
	tya		quit if there was no command
	bne	st0
err	clc
	rts
;
;  Scan the table for the command.
;
st0	move4 ctpointer,r0
st1	ldy	#disp_cnum+1	quit if at end of table
	lda	[r0],Y
	beq	bad
	ldy	#disp_cnum	branch if its a utility
	lda	[r0],Y
	beq	st4
	ldy	#disp_name	get length of name
	lda	[r0],Y
	and	#$00FF
	cmp	buff1	branch if its different from what we
	bne	st4	 want
	sta	r6	compare the names
	ldx	#2
	iny
	short M
st2	lda	[r0],Y
	cmp	buff1,X
	bne	st3
	iny
	inx
	dbne	r6,st2
st3	long	M
	beq	ln1	branch if names match

st4	ldy	#disp_name	get next command
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	st1
	inc	r2
	bra	st1

bad	clc
	rts
;
;  If the command is a language, return a command number of 0; otherwise
;  return the command number.
;
ln1	ldy	#disp_cnum	branch if not a language
	lda	[r0],Y
	bpl	ln2
	and	#$7FFF	set language number
	tay
	lda	#0	set command number
ln2	pha		set command name
	phy
	move4 r0,ctlast
	lla	r0,buff1
	jsr	SetCommandName
	ply
	pla
	sec		successful search
	rts
	end

****************************************************************
*
*  FindCommand2 - Find a command by command number
*
*  Inputs:
*	A - command or language number to search for
*
*  Outputs:
*	C - set if language is in command table
*	R0 - if C is set, points to language name
*	CTLAST - points to command
*
****************************************************************
*
FindCommand2 start
	debug FindCommand2
	using Common

	sta	r4	save language number
	move4 ctpointer,r0
st1	ldy	#disp_cnum+1	quit if at end of table
	lda	[r0],Y
	beq	err
	dey		branch if not a language
	lda	[r0],Y
	cmp	r4	continue search if not one we want
	beq	st3

	ldy	#disp_name	get next command
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	st1
	inc	r2
	bra	st1

st3	move4 r0,ctlast	allow restartable languages
	add4	r0,#disp_name	R0 points to language name
	sec		found it
	rts

err	clc		language not found
	rts
	end

****************************************************************
*
*  FirstAlias - Get the first alias entry
*  FirstVariable - Get the first variable entry
*  NexatAlias - Get next alias entry
*  NextVariable - Get next variable entry
*
*  Inputs:
*	V_POINTER - pointer to variable table
*	A_POINTER - pointer to alias table
*	R0 - points to current variable
*
*  Outputs:
*	R0 - points to next variable,alias (0 if none)
*	R4 - points to variable,alias name
*	R8 - points to variable,alias string
*
****************************************************************
*
FirstVariable start
	debug FirstVariable
	using Common
	using RecCom

	move4 v_pointer,r0	get the variable table pointer
	bra	nv1

FirstAlias entry
	move4 a_pointer,r0	get the alias table pointer
	bra	nv1

NextVariable entry
NextAlias entry
	lda	[r0]	next entry
	tax
	ldy	#2
	lda	[r0],Y
	stx	r0
	sta	r0+2

nv1	ldy	#10	move the name and value pointer
	ldx	#6
nv2	lda	[r0],Y
	sta	r4,X
	dey
	dey
	dex
	dbpl	X,nv2
rts	rts
	end

****************************************************************
*
*  GetBootVol - Get the boot volume
*
*  Inputs:
*	dcb - address of the parameter block
*
*  Outputs:
*	A - error code; 0 if no error occurred
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the actual length of the boot volume name.  The
*	address of the buffer is returned in the appropriate
*	spot in the parameter block.  The caller is
*	responsible for disposing of the memory.  No memory
*	is reserved if an error occurs.
*
****************************************************************
*
GetBootVol start
buffPtr	equ	1	pointer to the file name buffer
err	equ	5	error

	sub	(4:dcb),6

	stz	err	assume we will succeed

	lda	#0	get the length of the buffer
	sta	shortName+2
	OSGet_Boot_Vol shortPrm

	lda	shortName+2	reserve a block of memory
	clc
	adc	#4
	jsl	Malloc
	ldy	#2	save the pointer to the memory
	sta	[dcb],Y
	sta	buffPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	buffPtr+2
	ora	buffPtr	check for out of memory condition
	bne	lb1
	dec	err	(err = -1)
	bra	lb2

lb1	lda	shortName+2	set the length of the buffer
	clc
	adc	#4
	sta	[buffPtr]
	ph4	dcb	get the volume name
	ph2	#$2028
	jsl	$E100B0
	bcc	lb2	branch if no error occured
	sta	err	save the error number
	free	buffPtr	dispose of the buffer
lb2	ret	2:err	return the error code

shortPrm dc	i'1',a4'shortName'	parm for Get_Boot_Vol
	end

****************************************************************
*
*  GetDirEntry - Get a directory entry
*
*  Inputs:
*	dcb - address of the parameter block
*
*  Outputs:
*	A - error code; 0 if no error occurred
*
*  Notes:
*	This subroutine reserves memory buffers based on
*	the actual length of the file path name and the
*	option list.  The address of the buffers are returned
*	in the appropriate spot in the parameter block.  The
*	caller is responsible for disposing of the memory.
*
*	No memory is reserved if an error occurs.
*
*	Nil is returned for the optionList parameter if the
*	length of the option list is 0.
*
*	Due to the nature of the GetDirEntry call, this
*	subroutine only handles calls where base=0.
*
****************************************************************
*
GetDirEntry start
buffPtr	equ	1	pointer to the file name buffer
opPtr	equ	5	pointer to the option list buffer
err	equ	9	error
oserr	equ	11	error returned by GetDirEntry

	sub	(4:dcb),12

	stz	err	assume we will succeed

	stz	opPtr	opPtr = nil
	stz	opPtr+2
	ldy	#2	get the length of the buffer
	lda	[dcb],Y
	sta	refNum
	ldy	#6
	lda	[dcb],Y
	sta	base
	iny
	iny
	lda	[dcb],Y
	sta	displacement
	OSGet_Dir_Entry shortPrm
	sta	oserr

	lda	medName+2	reserve a block of memory
	clc
	adc	#4
	jsl	Malloc
	ldy	#10	save the pointer to the memory
	sta	[dcb],Y
	sta	buffPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	buffPtr+2
	ora	buffPtr	check for out of memory condition
	bne	lb1
	dec	err	(err = -1)
	brl	lb10

lb1	lda	medName2+2	if length(optionList) <> 0
	beq	lb4
	lda	[dcb]	  and dcb^.pCount >= 15 then
	cmp	#15
	blt	lb4
	clc		  reserve a block of memory
	lda	medName2+2
	adc	#4
	jsl	Malloc
	ldy	#$32	  save the pointer
	sta	[dcb],Y
	sta	opPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	opPtr+2
	ora	opPtr	  if there was an out of memory error
	bne	lb2	    then
	dec	err	    err = -1
	free	buffPtr	    free(buffPtr)
	ldy	#10	    dcb^.name = nil
	lda	#0
	sta	[dcb],Y
	iny
	iny
	sta	[dcb],Y
	bra	lb3	  else
lb2	lda	medName2+2	    set the length of the buffer
	clc
	adc	#4
	sta	[opPtr]
lb3	anop		  endif
	bra	lb5	else if dcb^.pCount >= 15 then
lb4	lda	[dcb]
	cmp	#15
	blt	lb5
	ldy	#$32	  dcb^.optionList = nil
	lda	#0
	sta	[dcb],Y
	iny
	iny
	sta	[dcb],Y
lb5	anop		endif

	lda	medName+2	set the length of the buffer
	clc
	adc	#4
	sta	[buffPtr]

	lda	oserr	if oserr then
	beq	lb5a
	ph4	dcb	  get the prefix name
	ph2	#$201C
	jsl	$E100B0
	jcc	lb10	  branch if no error occured
	sta	err	  save the error number
	free	buffPtr	  dispose of the buffers
	free	opPtr
	bra	lb10	else
lb5a	lda	opPtr                      if opPtr <> nil then
	ora	opPtr+2
	beq	lb7
	ldy	medName2+2                   fill in the option list
	iny
	iny
	iny
	short	M
lb6	lda	medName2,Y
	sta	[opPtr],Y
	dey
	cpy	#1
	bne	lb6
lb7	short	M                          fill in the name
	ldy	medName+2           
	iny
	iny
	iny
lb8	lda	medName,Y
	sta	[buffPtr],Y
	dey
	cpy	#1
	bne	lb8
	long	M
	move4	buffPtr,name               set up the addresses
	move4	opPtr,options
	lda	[dcb]                      fill in the record
	asl	A
	tax
	lda	len,X
	tay
lb9	lda	shortPrm,Y
	sta	[dcb],Y
	dey
	dey
	bne	lb9
	lla	name,medName	  repair the name pointers
	lla	options,medName2
lb10	ret	2:err	return the error code

shortPrm dc	i'17'	parm for GetDirEntry
refNum	ds	2
	ds	2
base	ds	2
displacement ds 2
name	dc	a4'medName'
	ds	2
	ds	2
	ds	4
	ds	4
	ds	8
	ds	8
	ds	2
	ds	4
	ds	2
options	dc	a4'medName2'
	ds	4
	ds	4

len	dc	i'2,2,4,6,8,12,14,16,20,24,32,40,42,46,48,52,56,60'

medName	dc	i'82'
	ds	80
medName2	dc	i'16'
	ds	14
	end

****************************************************************
*
*  GetFileType - get the type of a file
*
*  Inputs:
*	ptr - pointer to the path name
*
*  Outputs:
*	A - file type; -1 if the file does not exist
*	X - lsw of aux type
*
****************************************************************
*
GetFileType start
	sub	(4:ptr),0

	move4 ptr,giPathName	get the file's info
	OSGet_File_Info giRec
	bcc	lb1
	lda	#-1	no file: return -1
	sta	giFileType

lb1	ret	4:giFileType

giRec	dc	i'4'
giPathName ds	4
	ds	2
giFileType ds	2
giAuxType ds	4
	end

****************************************************************
*
*  GetPrefix - Get a prefix name
*
*  Inputs:
*	dcb - address of the parameter block
*
*  Outputs:
*	A - error code; 0 if no error occurred
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the actual length of the boot volume name.  The
*	address of the buffer is returned in the appropriate
*	spot in the parameter block.  The caller is
*	responsible for disposing of the memory.  No memory
*	is reserved if an error occurs.
*
****************************************************************
*
GetPrefix start
buffPtr	equ	1	pointer to the file name buffer
err	equ	5	error

	sub	(4:dcb),6

	stz	err	assume we will succeed

	lda	#0	get the length of the buffer
	sta	shortName+2
	ldy	#2
	lda	[dcb],Y
	sta	prefixNum
	OSGet_Prefix shortPrm

	lda	shortName+2	reserve a block of memory
	clc
	adc	#4
	jsl	Malloc
	ldy	#4	save the pointer to the memory
	sta	[dcb],Y
	sta	buffPtr
	iny
	iny
	txa
	sta	[dcb],Y
	sta	buffPtr+2
	ora	buffPtr	check for out of memory condition
	bne	lb1
	dec	err	(err = -1)
	bra	lb2

lb1	lda	shortName+2	set the length of the buffer
	clc
	adc	#4
	sta	[buffPtr]
	ph4	dcb	get the prefix name
	ph2	#$200A
	jsl	$E100B0
	bcc	lb2	branch if no error occured
	sta	err	save the error number
	free	buffPtr	dispose of the buffer
lb2	ret	2:err	return the error code

shortPrm dc	i'2'	parm for Get_Prefix
prefixNum ds	2
	dc	a4'shortName'
	end

****************************************************************
*
*  IntToOSString - Convert 2 byte integer to string
*
*  Inputs:
*	int - number to convert
*	str - addr of output string (OS input string format)
*
*  Notes:
*	The maximum string size returned is 5 characters.
*	Be sure and leave one extra character in the buffer
*	for work space, for a total buffer length of
*	8 bytes.
*
****************************************************************
*
IntToOSString start Seg2
	debug IntToOSString

	lsub	(2:int,4:str),0

	ldy	#2	Y is disp in string
	ldx	#8	X is disp into denominators
	lda	#'0'	handle zero as a special case
	sta	[str],Y
	lda	int
	bne	lb1
	iny
	bra	lb5
lb1	cmp	>den,X	loop until the first den is found
	bge	lb2
	dex
	dbne	X,lb1
lb2	pha		initialize the string
	lda	#'0'
	sta	[str],Y
	pla
lb3	sec		while A > den(X) do
	sbc	>den,X	  A = a-den(X)
	bcc	lb4
	pha
	lda	[str],Y	  string(Y)++
	inc	A
	sta	[str],Y
	pla
	bra	lb3	endwhile
lb4	adc	>den,X	(fix A)
	iny		next character position
	dex		next denominator
	dbpl	X,lb2
lb5	tya		set the length
	dec	A
	dec	A
	sta	[str]

	lret

den	dc	i'1,10,100,1000,10000'
	end

****************************************************************
*
*  IsAlpha - Alphabetic identification
*
*  Inputs:
*	A - letter to check
*
*  Outputs:
*	C - set if alpha, else clear
*
****************************************************************
*
IsAlpha	start seg2
	debug IsAlpha

	php
	short M
	cmp	#'A'
	blt	not
	cmp	#'z'+1
	bge	not
	cmp	#'Z'+1
	blt	is
	cmp	#'a'
	bge	is

not	plp
	clc
	rtl

is	plp
	sec
	rtl
	longa on
	end

****************************************************************
*
*  IsDigit - See if a character is numeric
*
*  Inputs:
*	A - character to check
*
*  Outputs:
*	C - set if numeric, else clear
*
*  Notes:
*	May be called with long or short regs
*
****************************************************************
*
IsDigit	start seg2
	debug IsDigit

	php
	short M
	cmp	#'0'
	blt	no
	cmp	#'9'+1
	blt	yes
no	plp
	clc
	rtl

yes	plp
	sec
	rtl
	longa on
	end

****************************************************************
*
*  IsWhite - is the character a whitespace character?
*
*  Inputs:
*	A - character to check
*
*  Outputs:
*	C - set for a whitespace char; else clear
*
*  Notes:
*	May be called with long or short regs
*
****************************************************************
*
IsWhite	start seg2
TAB	equ	9	horizontal tab

	php		switch to short M
	short M
	cmp	#' '	space is a whitespace char
	beq	yes
	cmp	#TAB	tab is a whitespace char
	beq	yes

	plp		not white space
	clc
	rtl

yes	plp		is white space
	sec
	rtl

	longa on
	end

****************************************************************
*
*  IsXDigit - Hexadecimal identification
*
*  Inputs:
*	A - character to check
*
*  Outputs:
*	C - set if ok else clear
*
*  Notes:
*	May be called with long or short regs
*
****************************************************************
*
IsXDigit start seg2
	debug IsXDigit

	php		save processor status
	short M
	jsl	IsDigit
	bcs	yes
	jsl	ToUpper
	cmp	#'A'
	bcc	no
	cmp	#'F'+1
	bcc	yes

no	plp		restore status
	clc		return no
	rtl

yes	plp		restore status
	sec		return yes
	rtl
	longa on
	end

****************************************************************
*
*  OSStringToLong - Convert an OS string to a long value
*
*  Inputs:
*	str - pointer to string to convert
*	A - value
*
****************************************************************
*
OSStringToLong start seg2
	debug OSStringToLong
val	equ	1	value

	lsub	(4:str),4

	stz	val	val = 0
	stz	val+2
	lda	[str]	Y = # of characters
	tay
	beq	rts
	add4	str,#2
cv1	lda	[str]	for each character do
	and	#$007F
	jsl	IsDigit	get a character
	bcc	rts	quit if not a number
	asl	val	A = A * 10
	rol	val+2
	lda	val
	ldx	val+2
	asl	val
	rol	val+2
	asl	val
	rol	val+2
	clc
	adc	val
	sta	val
	txa
	adc	val+2
	sta	val+2
	lda	[str]	A = A+digit
	and	#$000F
	clc
	adc	val
	sta	val
	bcc	cv2
	inc	val+2
cv2	inc4	str	next character
	dey
	bne	cv1

rts	lret	4:val
	end

****************************************************************
*
*  Pause - Check for pause/stop
*
*  Outputs:
*	C - set if stop requested
*
****************************************************************
*
Pause	start
	debug Pause
	using Common

	php
	long	I,M
	OSStop stopRec	see if an open-apple . has been typed
	lda	stopFlag
	bne	lb4
	lda	waitFlag	quit if waitFlag is clear
	beq	lb2
	short M
	lda	>$C000	see if a key has been pressed
	bpl	lb2
	sta	>$C010	clear strobe
	jsr	PauseOn	turn on the pause symbol
lb1	lda	>$C000
	bpl	lb1
	sta	>$C010	clear strobe
	jsr	PauseOff	turn off the pause symbol
	cmp	#$80+'.'	see if key was open-apple .
	bne	lb2
	lda	>$C025
	bmi	lb3
lb2	plp
	clc		not ESC - return
	rts

lb3	sta	>$C010	ESC found - stop
	long	M
lb4	lda	#1	set stop flag
	sta	stop_flag
	plp
	sec
	rts

stopRec	dc	i'1'	OSStop record
stopFlag ds	2
	end

****************************************************************
*
*  PauseOff - turn off the pause symbol
*
*  Notes:
*	1. Can be called with long or short registers.
*	2. All registers are preserved.
*
****************************************************************
*
PauseOff start

	php
	long	I,M
	pha
	phx
	phy
	OSConsoleOut space
	OSConsoleOut left
	ply
	plx
	pla
	plp
	rts

space	dc	i'1,$20'
left	dc	i'1,8'
	end

****************************************************************
*
*  PauseOn - turn on the pause symbol
*
*  Notes:
*	1. Can be called with long or short registers.
*	2. All registers are preserved.
*
****************************************************************
*
PauseOn	start

	php
	long	I,M
	pha
	phx
	phy
	OSConsoleOut inverse
	OSConsoleOut mouseOn
	OSConsoleOut hour
	OSConsoleOut mouseOff
	OSConsoleOut normal
	OSConsoleOut left
	ply
	plx
	pla
	plp
	rts

inverse	dc	i'1,15'
mouseOn	dc	i'1,27'
hour	dc	i'1,$43'
mouseOff dc	i'1,24'
normal	dc	i'1,14'
left	dc	i'1,8'
	end

****************************************************************
*
*  PopAlias - Pop an alias table
*
*  Inputs:
*	A_NEXT - pointer to next record
*	A_POINTER - pointer to current alias table
*
*  Outputs:
*	A_NEXT - previous record
*	A_POINTER - pointer to previous table
*
****************************************************************
*
PopAlias start
	using Common
	using RecCom

	lda	a_next	make sure there is a level to pop
	ora	a_next+2
	bne	pp1
	rts
;
;  Dispose of all current nodes
;
pp1	jsr	FirstAlias	get the first entry

pp2	lda	r0	quit if no more
	ora	r2
	beq	pp3
	move4 r0,p1	save the node pointer
	free	r4	dispose of the name
	free	r8	dispose of the string
	jsr	NextAlias	next entry
	free	p1	dispose of the node
	bra	pp2

pp3	move4 a_next,r0	set new node
	ldy	#6	copy current info
pp4	lda	[r0],Y
	sta	a_next,Y
	dey
	dbpl	Y,pp4
	free	r0	dispose of old node
	rts
	end

****************************************************************
*
*  PopScript - Pop an exec file record
*
*  Inputs:
*	EX_NEXT - exec node
*
*  Outputs:
*	ex_next - pointer to next record
*	ex_ptr - pointer to exec file
*
****************************************************************
*
PopScript start
	debug PopScript
	using Common
	using RecCom

	lda	ex_next	make sure there is stuff to pop
	ora	ex_next+2
	beq	rts
	free	ex_ptr
	move4 ex_next,r0	get the next node pointer
	ldy	#6	copy current info
pp1	lda	[r0],Y
	sta	ex_next,Y
	dey
	dbpl	Y,pp1
	free	r0	dispose of this node
rts	rts
	end

****************************************************************
*
*  PrintOSName - print a GS/OS path name
*
*  Inputs:
*	ptr - pointer to the path name
*	flags - flags word.  The following bits may be set:
*		$8000 - the input is an output string (includes
*		        a buffer length)
*		$4000 - print to error out
*		$2000 - don't do separator substitution
*
****************************************************************
*
PrintOSName start seg2
	using	Common
len	equ	1	length of the string
duplicated equ	3	did we duplicate the string?
ch	equ	5	char to substitute for :

	lsub	(4:ptr,2:flags),6

	lda	flags	if flags & $8000 then
	bpl	lb0
	add2	ptr,#2	  ptr += 2
lb0	stz	duplicated	duplicated = false
	lda	flags	if not (flags & $2000) then
         and	#$2000
	bne	cv3
	jsl	SeparatorSet	  if separator is set to a single char
	bcc	cv3                         then
	sta	ch	    save the char
         ph4	ptr                          make a copy
	jsl	DuplicateOSString
	tay                                check for OOM
	stx	duplicated
	ora	duplicated
	sta	duplicated
	beq	cv3
	sty	ptr                          use the copy
	stx	ptr+2
	lda	[ptr]                        change any ':' to new char
	beq	cv3
	tax
	ldy	#2
	short	M
cv1	lda	[ptr],Y
	cmp	#':'
	bne	cv2
	lda	ch
	sta	[ptr],Y
cv2	iny
	dex
	bne	cv1
	long	M
cv3	lda	[ptr]	len = ptr
	beq	lb3
	sta	len
	ldy	#2	Y {index into ptr} = 2
	asl	flags	if flags & $4000 then
	bpl	lb2
lb1	lda	[ptr],Y	  write to errout
	and	#$00FF
	jsl	~errout
	iny
	dec	len
	bne	lb1
	bra	lb3	else
lb2	lda	[ptr],Y	  write to stdout
	and	#$00FF
	jsl	~stdout
	iny
	dec	len
	bne	lb2
lb3	anop		endif
	lda	duplicated	if duplicated then
	beq	lb4
	free	ptr                        free ptr
lb4	anop		endif

	lret
	end

****************************************************************
*
*  PtoOSString - convert a P string to an OS string
*
*  Inputs:
*	pstr - pointer to the P string
*
*  Outputs:
*	X-A - nil for out of memory, otherwise a pointer to the string
*
*  Notes:
*	This subroutine reserves a memory buffer based on
*	the length of the string.  The caller is responsible
*	for disposing of the memory.
*
****************************************************************
*
PtoOSString start seg2
str	equ	1	pointer to duplicate string
str2	equ	7	pointer to duplicate string+1
len	equ	5	length of the string

	lsub	(4:pstr),10

	lda	[pstr]	allocate space for the string
	and	#$00FF
	sta	len
	inc	A
	inc	A
	jsl	Malloc
	sta	str
	stx	str+2
	ora	str+2
	beq	lb5
	lda	len	set the length
	sta	[str]
	add4	str,#1,str2	copy over the string characters
	ldy	len
	beq	lb5
	short M
lb3	lda	[pstr],Y
	sta	[str2],Y
	dey
	bne	lb3
	long	M

lb5	lret	4:str
	end

****************************************************************
*
*  PushAlias - Push an alias table exporting current aliases
*
*  Inputs:
*	A_NEXT - pointer to next node
*	A_POINTER - pointer to table
*
*  Outputs:
*	A_NEXT - pointer to last record
*	A_POINTER - pointer to copy of last table
*
****************************************************************
*
PushAlias start
	using Common
	using RecCom
a_node_size	equ   8

	malloc #a_node_size	get 8 bytes to save the node
	sta	r0
	stx	r2
	ora	r2
	jeq	err2

	ldy	#a_node_size-2	copy current info
ps1	lda	a_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,ps1
	move4 r0,a_next	set the next pointer
	jsr	FirstAlias	get the first node
	stz	a_pointer	initialize new table
	stz	a_pointer+2

ps2	lda	r0	quit if at end of list
	ora	r2
	beq	rts
	move4 r4,r12
	move4 r8,r16
	ph4	r0	save list pointer
	jsr	SetAlias	set the variable
	bcs	err
	pl4	r0	get list pointer
	jsr	NextAlias	next node
	bra	ps2
rts	rts

err	pla
	pla
err2	lda	#outofmem
	jsl	SysError
	rts
	end

****************************************************************
*
*  PushScript - Push an exec file entry
*
*  Inputs:
*	ex_next - exec node
*	ex_ptr - pointer to file
*
*  Outputs:
*	C - clear is no error else set
*
****************************************************************
*
PushScript start
	debug PushScript
	using Common
	using RecCom

ex_node_size equ   8

	malloc #ex_node_size	get space for node
	sta	r0
	stx	r2
	ora	r2
	bne	ps1
	sec		error
	rts

ps1	ldy	#ex_node_size-2	copy current info
ps2	lda	ex_next,Y
	sta	[r0],Y
	dey
	dbpl	Y,ps2
	move4 r0,ex_next	set the next pointer
	clc
	rts
	end

****************************************************************
*
*  QuitShell - Quit the shell and return to the launcher
*
****************************************************************
*
QuitShell start
	using Common

	jsr	ShutDown	shut down restartable applications
	DisposeAll User_ID	get rid of my memory
	lda	ProDOS	reset ProDOS hook
	sta	>pro_entry
	lda	ProDOS+2
	sta	>pro_entry+2
	lda	StackProDOS	reset ProDOS stack hook
	sta	>pro_stack_entry
	lda	StackProDOS+2
	sta	>pro_stack_entry+2
	OSQuit qtRec	quit

	pea	$00FF	if we come back, die
	ph4	#msg
	_SysFailMgr

msg	dw	'Quit call failed: '

qtRec	dc	a'0'
	end

****************************************************************
*
*  ReadCommand - read a command from the command table
*
*  Inputs:
*	r4 - pointer to the file to read from
*
*  Outputs:
*	r0 - length of command read
*	buff1 - command table entry
*	C - set if done, else clear
*
****************************************************************
*
ReadCommand private
	debug ReadCommand
	using Common
buff2	equ	buff1+$80

fm1	stz	r0	init line length
fm1a	lda	[r4]	read a line
	sec
	and	#$00FF
	jeq	rts
	ldy	r0
	sta	buff2,Y
	inc	r0
	inc4	r4
	cmp	#RETURN
	bne	fm1a
	lda	buff2	skip if first char is ; or *
	and	#$00FF
	cmp	#';'
	beq	fm1
	ldy	#0	skip if the line is blank
	jsr	SkipBlank
	lda	buff2,Y
	and	#$00FF
	cmp	#RETURN
	beq	fm1
	ldx	#0	read the command name into BUFF1
fm2	lda	buff2,Y
	and	#$00FF
	jsl	IsWhite
	bcs	fm3
	cmp	#RETURN
	beq	fm3
	jsl	ToUpper
	sta	buff1+disp_name+1,X
	inx
	iny
	bra	fm2
fm3	jsr	SkipBlank	skip to the command type
	stz	buff1+disp_restart	default RESTART = FALSE
	stz	buff1+disp_user_id	initialize USER_ID to 0
	lda	buff2,Y	read command type
	and	#$00FF
	cmp	#'*'
	bne	fm3a
	inc	buff1+disp_restart
	iny
	lda	buff2,Y
	and	#$00FF
fm3a	jsl	ToUpper
	stz	buff1+disp_cnum	initialize the command number
	cmp	#'U'	skip line if the identifier is bad
	beq	fm7
	cmp	#'L'
	beq	fm4
	cmp	#'C'
	jne	fm1
fm4	iny		read the command number
	pha
	jsr	SkipBlank
fm5	lda	buff2,Y
	and	#$00FF
	jsl	IsDigit
	bcc	fm6
	and	#$000F
	pha
	lda	buff1+disp_cnum
	asl	A
	pha
	asl	A
	asl	A
	clc
	adc	1,s
	adc	3,s
	sta	buff1+disp_cnum
	pla
	pla
	iny
	bra	fm5
fm6	pla		quit if command # is 0
	cmp	#'L'	if this is a language, set the high bit
	bne	fm7
	lda	buff1+disp_cnum
	ora	#$8000
	sta	buff1+disp_cnum
fm7	txa		set the length of the name
	short M
	sta	buff1+disp_name
	long	M
	clc		set the length of the command
	adc	#disp_name+1
	sta	r0
	clc		return with a command
rts	rts
;
;  Skip to the next non-blank.
;
SkipBlank lda	buff2,Y
	and	#$00FF
	jsl	IsWhite
	bcc	rts
	iny
	bra	SkipBlank
	end

****************************************************************
*
*  ReadSYSCMND - Reads And Formats The Command Table
*
*  Inputs:
*	R0 - address of command table name
*
*  Outputs:
*	CTHANDLE - handle to command table
*
****************************************************************
*
ReadSYSCMND start
	debug ReadSYSCMND
	using Common
;
;  Scan the table to find out how long it is.
;
	move4 r0,opPathname	open the command table file
	OSOpen opRec
	jcs	err
	lda	opRefnum	save ref #
	sta	clRefnum
	sta	rdRefnum
	sta	gtRefnum
	OSGet_EOF gtRec	find spot for file
	jcs	err
	lda	gtEOF
	ldx	gtEOF+2
	clc
	adc	#2
	bcc	in1
	inx
in1	jsl	MLalloc
	sta	fhandle
	stx	fhandle+2
	ora	fhandle+2
	bne	ln
	lda	#outofmem
	brl	err
ln	move4 fhandle,rdDataBuffer
	move4 gtEOF,rdRequestCount	read the file
	OSRead rdRec
	jcs	err
	OSClose clRec
	add4	rdDataBuffer,gtEOF,r0	set eof mark
	lda	#0
	sta	[r0]
	move4 rdDataBuffer,r4
	lda	#disp_name+1	compute length of file by reading
	sta	r10	 all lines and summing their
	stz	r12	 lengths
	stz	r2
ln1	jsr	ReadCommand
	bcs	ln2
	add4	r10,r0
	bra	ln1
;
;  Read the command table
;
ln2	move4 rdDataBuffer,r4	set mark to start of file
	mlalloc r10	find a spot for the command table
	sta	cthandle
	stx	cthandle+2
	ora	cthandle+2
	bne	ln3
	lda	#outofmem
	brl	err
ln3	lda	cthandle	get the table's pointer
	sta	ctpointer
	sta	r10
	lda	cthandle+2
	sta	ctpointer+2
	sta	r10+2
rd1	jsr	ReadCommand	read in the commands
	bcs	rd3
	ldx	r0
	ldy	#0
	short M
rd2	lda	buff1,Y
	sta	[r10],Y
	iny
	dbne	X,rd2
	long	M
	clc
	tya
	adc	r10
	sta	r10
	bcc	rd1
	inc	r12
	bra	rd1
rd3	ldy	#disp_cnum	set trailing zero bytes
	lda	#0
	sta	[r10],Y
	iny
	sta	[r10],Y
	ldy	#disp_user_id
	sta	[r10],Y
	sta	[r10]
	free	fhandle	get rid of file
	rts
;
;  Handle load errors.
;
err	jsl	SysError
	puts	#'Error occurred while loading the command table.',cr=t
	brl	TermError
;
;  Local data
;
fhandle	ds	4	file handle

opRec	dc	i'3'	OSOpen record
opRefnum ds	2
opPathname ds	4
	dc	i'$0001'

gtRec	dc	i'2'	OSGet_EOF record
gtRefnum ds	2
gtEOF	ds	4

clRec	dc	i'1'	OSClose record
clRefnum ds	2

rdRec	dc	i'4'	OSRead record
rdRefnum ds	2
rdDatabuffer ds 4
rdRequestCount ds 4
	ds	4
	end

****************************************************************
*
*  ReturnLPString - return a long (up to 255 char) P string
*
*  Inputs:
*	in - pointer to OS input string to return
*	out - pointer to P string buffer (256 chars long)
*	upper - uppercase the names?
*
****************************************************************
*
ReturnLPString start
error	equ	1	error number

	sub	(4:in,4:out,2:upper),2

	lda	in	if in = nil then
	ora	in+2
	bne	lb0
	lla	in,zero	  in = @zero

lb0	stz	error	no error
	lda	[in]	set the length
	cmp	#255
	blt	lb1
	lda	#$4F	buffer too small error
	sta	error
	lda	#255
lb1	sta	[out]
	tay		copy in the string
	beq	lb3
	inc4	in
	short M
lb2	lda	[in],Y
	cmp	#':'
	bne	lb2a
	lda	#'/'
lb2a	ldx	upper
	beq	lb2b
	jsl	ToUpper
lb2b	sta	[out],Y
	dey
	bne	lb2
	long	M

lb3	ret	2:error

zero	dc	i'0'	zero length string
	end

****************************************************************
*
*  ReturnOSString - return an OS string
*
*  Inputs:
*	in - pointer to OS input string to return
*	out - pointer to OS output string buffer
*
****************************************************************
*
ReturnOSString start
error	equ	1	error code

	sub	(4:in,4:out),2

	lda	in	if in = nil then
	ora	in+2
	bne	lb0
	lla	in,zero	  in = @zero

lb0	lda	#$4F	assume the buffer is too small
	sta	error
	lda	[out]	make sure the buffer is at least 4
	cmp	#4	 characters long
	blt	lb2
	lda	[in]	set the length
	ldy	#2
	sta	[out],Y
	clc		make sure there is room to copy in the
	adc	#3	 string
	cmp	[out]
	bge	lb2
	add4	out,#2	copy in the string
	lda	[in]
	tay
	iny
	short M
lb1	lda	[in],Y
	sta	[out],Y
	dey
	bne	lb1
	long	M
	stz	error	no error

lb2	ret	2:error

zero	dc	i'0'	zero length string
	end

****************************************************************
*
*  ReturnPString - return a P string
*
*  Inputs:
*	in - pointer to OS input string to return
*	out - pointer to P string buffer (65 chars long)
*	slash - convert colons to slashes?
*
****************************************************************
*
ReturnPString start
error	equ	1	error number

	sub	(4:in,4:out,2:slash),2

	lda	in	if in = nil then
	ora	in+2
	bne	lb0
	lla	in,zero	  in = @zero

lb0	stz	error	no error
	lda	[in]	set the length
	cmp	#65
	blt	lb1
	lda	#$4F	buffer too small error
	sta	error
	lda	#64
lb1	sta	[out]
	tay		copy in the string
	beq	lb3
	inc4	in
	short M
lb2	lda	[in],Y
	ldx	slash
	beq	lb2a
	cmp	#':'
	bne	lb2a
	lda	#'/'
lb2a	jsl	ToUpper
	sta	[out],Y
	dey
	bne	lb2
	long	M

lb3	ret	2:error

zero	dc	i'0'	zero length string
	end

****************************************************************
*
*  SeparatorSet - see if the separator variable is set to a char
*
*  Outputs:
*        C - set if set; else clear
*	A - character (if C = 1)
*
****************************************************************
*
SeparatorSet start
	using	Common

	phd	                         save entry state
	phb
	phk	                         use local addressing
	plb
	lda	sh_my_dp                 use our DP
	tcd
	ldx	#14	save r0 area
lb1	lda	r0,X
	sta	tr0,X
	dex
	dex
	bpl	lb1
	move4	p1,tp1	save p1
	lla	r12,Separator	find the variable
	jsr	FindVariable
	stz	ch	assume we fail
	lda	r0	if the variable is not nil then
	ora	r2
	beq	lb2
	lda	[r8]                       if it is one char then
	cmp	#1
	bne	lb2
	ldy	#2                           return the char
	lda	[r8],Y
	and	#$00FF
	sta	ch
lb2      move4	tp1,p1	restore p1
	ldx	#14	restore r0 area
lb3	lda	tr0,X
	sta	r0,X
	dex
	dex
	bpl	lb3
	plb		restore entry state
	pld
	clc                            return results
	lda	>ch
	beq	lb4
	sec
lb4	rtl

ch	ds	2	char to return
tr0	ds	16	temp r0 area
tp1	ds	4	temp p1
	end

****************************************************************
*
*  SetAlias - Set an alias
*
*  Inputs:
*	R12 - adrress of alias name (C string)
*	R16 - address of alias value (C string)
*	C - set if error occurred
*
****************************************************************
*
SetAlias start
	debug SetAlias
	using Common
	using RecCom

	jsr	FindAlias	try to find the alias
	lda	r0
	ora	r0+2
	jeq	st3	if variable exists
	free	r8	  dispose of old string
	lda	[r16]	  get size of new string
	inc	A
	inc	A
	jsl	Malloc	  find a new string area
	sta	r8
	stx	r10
	ora	r10
	jeq	err
	lda	[r16]	  copy over the string
	tay
	iny
	short M
st1	lda	[r16],Y
	sta	[r8],Y
	dey
	bne	st1
	lda	[r16]
	sta	[r8]
	long	M
	ldy	#8	  set the alias node
	lda	r8
	sta	[r0],Y
	iny
	iny
	lda	r8+2
	sta	[r0],Y
	clc
	rts
;...............................................................
;
;  Create a new alias from scratch
;
;...............................................................
;
st3	stz	r0	no buffers reserved, yet
	stz	r2
	stz	r4
	stz	r6
	stz	r8
	stz	r10
	malloc #12	allocate the node
	sta	r0
	stx	r2
	ora	r2
	jeq	err
	lda	[r12]	compute size of name
	inc	A
	inc	A
	jsl	Malloc	allocate the name
	sta	r4
	stx	r6
	ora	r6
	jeq	err
	lda	[r16]	compute size of the value
	inc	A
	inc	A
	jsl	Malloc	allocate the name
	sta	r8
	stx	r10
	ora	r10
	jeq	err

	lda	[r12]	copy over the name
	tay
	iny
	short M
st4	lda	[r12],Y
	sta	[r4],Y
	dey
	bne	st4
	lda	[r12]
	sta	[r4]
	long	M	copy over the string
	lda	[r16]
	tay
	iny
	short M
st6	lda	[r16],Y
	sta	[r8],Y
	dey
	bne	st6
	lda	[r16]
	sta	[r8]
	long	M	set up the node
	lda	#0	next = nil
	sta	[r0]
	iny
	iny
	sta	[r0],Y
	iny		set the name pointer
	iny
	lda	r4
	sta	[r0],Y
	iny
	iny
	lda	r4+2
	sta	[r0],Y
	iny		set the string pointer
	iny
	lda	r8
	sta	[r0],Y
	ldy	#10
	lda	r8+2
	sta	[r0],Y
;
;  Insert the new alias into the alias table
;
	lda	a_pointer	if A_POINTER = nil then
	ora	a_pointer+2
	bne	in1
	move4 r0,a_pointer	  A_POINTER = R0
	clc
	rts

in1	ldy	#2	else
	move4 a_pointer,r4	  R4 = V_POINTER

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
	clc
	rts
;
;  Handle an out of memory error creating the entry
;
err	free	r0
	free	r4
	free	r8
	sec
	rts
	end

****************************************************************
*
*  SetCommandName - Set the command name shell variable
*
*  Inputs:
*	R0 - pointer to C string command name
*
****************************************************************
*
SetCommandName start
	debug SetCommandName
	using Common

	ph2	setUsed	preserve this -- we aren't changing {Status}
	move4	r0,svValue	set {Command}
	OSSet svRec
	pl2	setUsed	restore setUsed
	rts

svRec	dc	i'3'
	dc	a4'command'
svValue	ds	4
	dc	i'0'
	end

****************************************************************
*
*  ShutDown - Shut down all restartable applications
*
*  Inputs:
*	cthandle - command table handle
*
****************************************************************
*
ShutDown start
	debug ShutDown
	using Common
	using RecCom

	move4 cthandle,r0	for each command entry do
lb1	ldy	#disp_restart	  if command is restartable and
	lda	[r0],Y	    there is a user ID then
	beq	lb2
	ldy	#disp_user_id
	lda	[r0],Y
	beq	lb2
	pha		    shut it down
	pha
	lda	#0
	sta	[r0],Y
	pha
	_UserShutDown
	pla
lb2	anop		  endif
	ldy	#disp_name	next
	lda	[r0],Y
	and	#$00FF
	ldy	#disp_cnum
	ora	[r0],Y
	beq	rts
	ldy	#disp_name
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	lb1
	inc	r2
	bra	lb1
rts	rts
	end

****************************************************************
*
*  SlashToColon - substitutes colons for slashes in GS/OS path names
*
*  Inputs:
*	ptr - pointer to the GS/OS input name
*
****************************************************************
*
SlashToColon start seg2

	lsub	(4:ptr),0

	lda	[ptr]
	beq	lb3
	tax
	ldy	#2
	short M
lb1	lda	[ptr],Y
	cmp	#':'
	beq	lb2a
	cmp	#'/'
	bne	lb2
	lda	#':'
	sta	[ptr],Y
lb2	iny
	dex
	bne	lb1
lb2a	long	M

lb3	lret
	end

****************************************************************
*
*  Spin - spin the spinner
*
*  Notes: Starts the spinner if it is not already in use.
*
****************************************************************
*
Spin	start
	using SpinnerCommon
	using Common

	lda	spinning	if not spinning then
	bne	lb1
	inc	spinning	  spinning := true
	lda	#spinSpeed	  set the timer
	sta	spinCount

lb1	dec	spinCount	if --spinCount <> 0 then
	bne	lb3
	lda	#spinSpeed	  spinCount := spinSpeed
	sta	spinCount
	dec	spinDisp	  spinDisp--
	bpl	lb2	  if spinDisp < 0 then
	lda	#3	    spinDisp := 3
	sta	spinDisp
lb2	ldx	spinDisp	  spinLoc^ := spinner[spinDisp]
	lda	spinner,X
	and	#$00FF
	pha
	jsl	cout_vector
	ph2	#8
	jsl	cout_vector
lb3	rts
	end

****************************************************************
*
*  StopSpin - stop the spinner
*
*  Notes: The call is safe, and ignored, if the spinner is inactive
*
****************************************************************
*
StopSpin start
	using SpinnerCommon
	using Common
                     
	lda	spinning
	beq	lb1
	stz	spinning
	ph2	#' '
	jsl	cout_vector
	ph2	#8
	jsl	cout_vector
lb1	rts
	end

****************************************************************
*
*  SysError - Write a system error message
*
*  Inputs:
*	A - error number
*
*  Notes:
*	A is unchanged
*
****************************************************************
*
SysError start	Seg2
	debug SysError
	using Common

	ldy	#2	see if a message exists
	sta	r0
	lla	r2,errors
lb1	lda	[r2]
	beq	number
	cmp	r0
	beq	lb2
	lda	[r2],Y
	and	#$00FF
	clc
	adc	#3
	adc	r2
	sta	r2
	bra	lb1
lb2	lla	r8,errors	yes - now see if the tool exists
lb3	lda	r0
	ora	#$00FF
	sta	r6
lb4	lda	[r8]
	beq	number
	cmp	r6
	beq	lb5
	clc
	lda	[r8],Y
	and	#$00FF
	adc	#3
	adc	r8
	sta	r8
	bra	lb4
lb5	inc	r2	yes - print it
	inc	r8
	puts	{r8},errout=t
	puts	{r2},cr=t,errout=t
	lda	r0
	rtl

number	puts	#'Error number ',errout=t write the error as a number
	lda	r0
	and	#$00FF
	sta	r2
	put2	r2,errout=t
	puts	#' in tool ',errout=t
	lda	r0
	ldx	#8
lb6	lsr	a
	dbne	X,lb6
	sta	r2
	put2	r2,cr=t,errout=t
	lda	r0
	rtl

errors	derr	$02,$FF,'Memory Manager: '
	derr	$02,$01,'Out of memory'
	derr	$02,$02,'Illegal operation on a nil handle'
	derr	$02,$03,'A nil handle was expected for this operation'
	derr	$02,$04,'Illegal operation on a locked or immovable block'
	derr	$02,$05,'Attempt to purge an unpurgable block'
	derr	$02,$06,'An invalid handle was given'
	derr	$02,$07,'An invalid owner ID was given'

	derr	$11,$FF,'Loader: '
	derr	$11,$01,'Not found'
	derr	$11,$02,'Segment not dynamic'
	derr	$11,$03,'Segment is locked'
	derr	$11,$04,'File is not a load file'
	derr	$11,$05,'System loader is busy'
	derr	$11,$06,'In-use counter is non-zero'
	derr	$11,$07,'File version error'
	derr	$11,$08,'UserID error'
	derr	$11,$09,'SegNum out of sequence'
	derr	$11,$0A,'Illegal load record found'
	derr	$11,$0B,'Load segment is foreign'

	derr	$00,$FF,'GS/OS: '
	derr	$00,$01,'Bad GS/OS call number'
	derr	$00,$04,'Parameter count out of range'
	derr	$00,$07,'GS/OS is busy'
	derr	$00,$10,'Device not found'
	derr	$00,$11,'Invalid device number (request)'
	derr	$00,$20,'Invalid request'
	derr	$00,$21,'Invalid control or status code'
	derr	$00,$22,'Bad call parameter'
	derr	$00,$23,'Character device not open'
	derr	$00,$24,'Character device already open'
	derr	$00,$25,'Interupt table full'
	derr	$00,$26,'Resources not available'
	derr	$00,$27,'I/O error'
	derr	$00,$28,'No device connected'
	derr	$00,$29,'Driver is busy'
	derr	$00,$2B,'Device is write protected'
	derr	$00,$2C,'Invalid byte count'
	derr	$00,$2D,'Invalid block address'
	derr	$00,$2E,'Disk has been switched'
	derr	$00,$2F,'Device off line or no media present'
	derr	$00,$40,'Invalid pathname syntax'
	derr	$00,$43,'Invalid reference number'
	derr	$00,$44,'Subdirectory does not exist'
	derr	$00,$45,'Volume not found'
	derr	$00,$46,'File not found'
	derr	$00,$47,'Create or rename with existing name'
	derr	$00,$48,'Volume full error'
	derr	$00,$49,'Volume directory full'
	derr	$00,$4A,'Version error (incompatible file format)'
	derr	$00,$4B,'Unsupported (or incorrect) storage type'
	derr	$00,$4C,'End-of-file encountered'
	derr	$00,$4D,'Position out of range'
	derr	$00,$4E,'Access not allowed'
	derr	$00,$4F,'Buffer too small'
	derr	$00,$50,'File is already open'
	derr	$00,$51,'Directory error'
	derr	$00,$52,'Unknown volume type'
	derr	$00,$53,'Parameter out of range'
	derr	$00,$54,'Out of memory'
	derr	$00,$57,'Duplicate volume name'
	derr	$00,$58,'Not a block device'
	derr	$00,$59,'Specified level outside legal range'
	derr	$00,$5A,'Block number too large'
	derr	$00,$5B,'Invalid path names for ChangePath'
	derr	$00,$5C,'Not an executable file'
	derr	$00,$5D,'Operating system not supported'
	derr	$00,$5F,'Too many applications on stack'
	derr	$00,$60,'Data unavailable'
	derr	$00,$61,'End of directory has been reached'
	derr	$00,$62,'Invalid FST class call'
	derr	$00,$63,'File does not contain required resource'
	dc	i'0'
	end

****************************************************************
*
*  TermError - terminal error handler
*
****************************************************************
*
TermError start

	putcr
	puts	#'The shell cannot recover from this error.',cr=t
	putcr
	puts	#'                      (OK)',cr=t
	short M
lb1	lda	>$00C000
	bpl	lb1
	sta	>$00C010
	long	M
	brl	QuitShell
	end

****************************************************************
*
*  ToInt - Convert a character to an integer
*
*  Inputs:
*	A - character
*
*  Outputs:
*	A - value
*
*  Notes:
*	This subroutine can be called with long or short
*	registers.  It assumes the character is in the least
*	significant byte of the accumulator.
*
****************************************************************
*
ToInt	start
	debug ToInt

	php		save processor status
	short M
	cmp	#'A'
	blt	hx1
	sbc	#'A'-'9'-1
hx1	and	#$0F
	plp		restore processor status
	rts
	longa on
	end

****************************************************************
*
*  ToUpper - convert a character to uppercase
*
*  Inputs:
*	A - character to convert
*
*  Outputs:
*	A - uppercase character
*
*  Notes:
*	This subroutine can be called with long or short
*	registers.  It assumes the character is in the least
*	significant byte of the accumulator.
*
****************************************************************
*
ToUpper	start seg2

	php
	short M
	cmp	#'a'
	blt	lb1
	cmp	#'z'+1
	bge	lb1
	and	#$5F
lb1	plp
	rtl
	longa on
	end
