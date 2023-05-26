	keep	obj/line
	mcopy line.macros
****************************************************************
*
*  Line
*
*  This module contains the subroutines used to directly
*  manipulate the buffer containing the current command line.
*
****************************************************************
*
	copy	sh.dp
LineCommon privdata

buffSize ds	4	size of the line buffer
ch	ds	2	next EXEC character
firstToken ds	2	disp to token (for ExpandAlias)
line	dstr	,255	user input line
pipenum	dc	c'0'	pipe number (as a character)
shellPtr dc	a4'0'	ptr to the first char in a shell variable
shellLen dc	i'0'	# chars left in the shell variable

lineLen	equ	256	length of a line
lLine	ds	lineLen	logical line buffer for short lines
eLine	ds	lineLen	exec file line buffer for short lines
lAllocated ds	2	was lp^ allocated? (default is @lLine)
eAllocated ds	2	was ep^ allocated? (default is @eLine)
	end

****************************************************************
*
*  ChangeChar - change a character
*
*  Inputs:
*	X - index into the line
*	A character to change to
*
*  Notes:
*	1.  The A register may be long or short on entry
*	2.  X is not changed
*
****************************************************************
*
ChangeChar start
	debug ChangeChar

	php
	phx
	tay
	txa
	clc
	adc	lDisp
	cmp	lLength
	bge	lb1
	tyx
	tay
	txa
	short M
	sta	[lp],Y
	long	M
lb1	plx
	plp
	rts
	end

****************************************************************
*
*  ClearLine - clear any information from the current line
*
*  Inputs:
*	lp - pointer to the current line
*
****************************************************************
*
ClearLine start
	debug ClearLine
	using LineCommon

	lda	lAllocated	if there is a line then
	beq	lb1
	free	lp	  dispose of it
lb1	stz	lp	  set the pointer to NULL
	stz	lp+2
	stz	lAllocated	  note that nothing is allocated
	lda	eAllocated	if there is an exec line then
	beq	lb2
	free	ep	  dispose of it
lb2	stz	ep	  set the pointer to NULL
	stz	ep+2
	stz	eAllocated	  note that nothing is allocated
	stz	line+1	clear user inputs, too
	rts
	end

****************************************************************
*
*  Continuation - check for a continuation line
*
*  Inputs:
*	exec_disp - disp into exec file
*	r0 - ptr to exec file
*
*  Outputs:
*	exec_disp - points to 1st char in next line if the line
*		was continued
*	C - set for a continuation, else clear
*
****************************************************************
*
Continuation private
	using LineCommon
	using Common
	debug Continuation
	using RecCom

	ldy	exec_disp	scan for non-whitespace before a CR
	short M
lb1	lda	[r0],Y
	beq	lb3
	cmp	#RETURN
	beq	found
	jsl	IsWhite
	bcc	lb3
	iny
	bra	lb1

lb3	long	M
	clc		none found - return
	rts

found	long	M
	iny		found - update exec_disp
	sty	exec_disp
	sec
	rts
	end

****************************************************************
*
*  ExpandAlias - if the last token in the line is an alias, expand it
*
*  Inputs:
*	lp - line pointer
*	lLength - # chars in the line
*	firstToken - disp to the start of the token
*
*  Outputs:
*	C - set for error; clear if no error
*
****************************************************************
*
ExpandAlias private
	using Common
	using LineCommon
	debug ExpandAlias

	ph4	r0
	sec		quit if there are no characters
	lda	eLength
	sbc	firstToken
	beq	lb3
	sta	r16	save the alias length
	inc	A	reserve space for the OS name
	inc	A
	jsl	Malloc
	sta	r12
	stx	r14
	ora	r14
	beq	lb3
	lda	r16	set the string length
	sta	[r12]
	clc		get the address of the characters -2
	lda	firstToken
	adc	ep
	sta	r16
	lda	ep+2
	adc	#0
	sta	r18
	sub4	r16,#2	copy over the alias name
	lda	[r12]	copy over the characters
	tax
	ldy	#2
	short M
lb1	lda	[r16],Y
	sta	[r12],Y
	iny
	dex
	bne	lb1
	long	M
	ph4	r4	find the alias
	jsr	FindAlias
	pl4	r4
	free	r12	free the os string buffer

	lda	r0	quit if there is no alias
	ora	r2
	beq	lb3

	lda	firstToken	erase the alias
	sta	eLength
	ldy	#2	put the characters
	lda	[r8]
	tax
lb2	lda	[r8],Y
	and	#$00FF
	phy
	jsr	PutChar
	ply
	bcs	lb4
	iny
	dex
	bne	lb2
lb3	clc
lb4	pl4	r0
	rts
	end

****************************************************************
*
*  ExpandPipe - expand a pipe in the current line buffer
*
*  Inputs:
*	lp - line pointer
*	lLength - # chars in the line
*
*  Outputs:
*	C - set for error; clear if no error
*
****************************************************************
*
ExpandPipe private
	using LineCommon
	using Common
	debug ExpandPipe

	lda	#' '	write a padd char
	jsr	PutChar
	bcs	rts
	lda	#'>'	write the output redirection char
	jsr	PutChar
	bcs	rts
	jsr	WritePipe	write '3/syspipe'
	bcs	rts
	lda	#RETURN	write the line feed
	jsr	PutChar
	lda	#'<'	write the input redirection char
	jsr	PutChar
	bcs	rts
	jsr	WritePipe	write '3/syspipe'
	bcs	rts
	lda	#' '	write a padd char
	jsr	PutChar
	bcs	rts
	inc	pipenum	increment the pipe number for next time
	clc
rts	rts
;
;  WritePipe - write '14:syspipe[pipenum]'
;
WritePipe ldx	#0
wp1	lda	pipeName,X
	and	#$00FF
	phx
	jsr	PutChar
	plx
	bcs	rts
	inx
	cpx	#l:pipeName
	bne	wp1
	lda	pipenum
	brl	PutChar

pipeName dc	c'14:SYSPIPE'
	end

****************************************************************
*
*  ExpandShellVariable - expand a shell variable
*
*  Outputs:
*	shellPtr - pointer to the first char of the var; NULL
*		for none
*	shellLen - # chars left in the variable; 0 for none
*
****************************************************************
*
ExpandShellVariable private
	using LineCommon
	using Common
	debug ExpandShellVariable
;
;  Collect the shell name in the user input line buffer.  Continue
;  collecting characters until:
;
;  1.  The terminating '}' is found.
;  2.  255 characters have been found, in which case you still scan
;      for the terminating '}', but ignore all characters past the
;      255th.
;  3.  The end of the line or exec file is reached, in which case
;      a '}' is implied.
;
	stz	line+1
cn1	jsr	NextExecChar
	lda	ch
	cmp	#'}'
	beq	fv1
	cmp	#RETURN
	beq	fv1
	short I,M
	ldx	line+1
	inx
	beq	cn2
	stx	line+1
	sta	line+1,X
cn2	long	I,M
	bra	cn1
;
;  Find the shell variable
;
fv1	stz	shellPtr	assume there is no shell variable
	stz	shellPtr+2
	stz	shellLen
	short I,M	null terminate the shell var name
	ldx	line+1
	beq	fv4
	lda	#0
	sta	line+2,X
	long	I,M
	ph4	r0	save critical variables
	ph4	r4
	ph4	#line+2	get the variable value
	jsl	CtoOSString
	sta	r12
	stx	r14
	ora	r14
	beq	fv2
	jsr	FindVariable
	free	r12	free(r12)
	lda	r0	if there is a value then
	ora	r2
	beq	fv2
	lda	[r8]	  set the length
	sta	shellLen
	add4	r8,#2,shellPtr	  set the address
fv2	pl4	r4	restore variables
	pl4	r0
fv4	long	I,M
	stz	line+1	no user line...
	rts
	end

****************************************************************
*
*  GetChar - get a character from the command line
*
*  Inputs:
*	lp - line pointer
*	lDisp - disp in line buffer
*	line - user input line
*
*  Outputs:
*	lDisp - updated
*	A - character; RETURN if line was empty
*
*  Notes:
*	1.  X, Y preserved.
*	2.  Register sizes on entry can be anything.
*	3.  Direct page varables are not disturbed.
*
****************************************************************
*
GetChar	start
	debug GetChar
	using LineCommon
	using Common
;
;  Initialization
;
	php		save register size
	long	I	switch to the size we want
	short M
	phx		save registers
	phy

	lda	line+1	branch if input is from the user's line
	bne	rl1
;
;  Handle input from the normal line
;
	ldy	lDisp	return RETURN if past the end of line
	cpy	lLength
	blt	in2
	lda	#RETURN
	bra	tr1
in2	lda	[lp],Y	get the character
	cmp	#RETURN	if it is not a RETURN then
	beq	tr1
	iny		  increment the character counter
	sty	lDisp
	bra	tr1
;
;  Read a character from the user line
;
rl1	short I	get the character
	lda	line+2
	pha
	dec	line+1	remove it from the line
	beq	rl3
	ldx	line+1
	ldy	#0
rl2	lda	line+3,Y
	sta	line+2,Y
	iny
	dex
	bne	rl2
rl3	pla
	long	I
;
;  Terminal processing
;
tr1	long	M	zero the high byte
	and	#$00FF
	ply		restore regs & return
	plx
	plp
	rts
	end

****************************************************************
*
*  GetLine - read a line from stdin
*
*  Outputs:
*	line - line read
*
*  Notes:
*	Skips any remaining characters in the current command
*	line.
*
****************************************************************
*
GetLine	start
	using LineCommon
	using Common
	debug GetLine

	stz	line+1	make sure there are no characters in
!			 an old line
lb1	jsr	GetChar	eat characters until the end of line
	cmp	#RETURN	 is reached
	bne	lb1
	gets	line,cr=t	get the line
	rts
	end

****************************************************************
*
*  GetString - read a (possibly quoted) string from the command line
*
*  Outputs:
*	X-A - pointer to an OS output string; null for error
*	C - set for error, else clear
*
*  Notes:
*	1. If no error is flagged, the caller is responsible
*	   for doing a dispose on the pointer.
*
****************************************************************
*
GetString start
	debug GetString
	using LineCommon
	using Common

	jsr	LineLength	reserve space for the variable
	inc	A
	inc	A
	inc	A
	jsl	Malloc
	sta	r12
	stx	r14
	ora	r14
	beq	err1
	stz	quote	assume that there is no quote
	ldy	#2	no characters read so far
	jsr	GetChar	get the first character from the line
	cmp	#'"'	branch if it is not a quote
	bne	lb2
	inc	quote	note that there was an opening quote

lb1	jsr	GetChar	get the next character
lb2	cmp	#RETURN	branch if all characters have been read
	beq	lb4
	ldx	quote	if there was a starting quote then
	beq	lb3
	cmp	#'"'	  if it is a quote then...
	bne	lb3
lb2a	jsr	GetChar	    get the next char, too.
	jsl	IsWhite	    skip any white space
	bcs	lb2a
	cmp	#RETURN	    if there are no more chars then
	beq	lb5	      we are finished
	cmp	#'"'	    if it is not a quote then
	bne	err2	      flag the error
lb3	sta	[r12],Y	save the character in the string
	iny
	bra	lb1

lb4	ldx	quote	make sure we were not expecting a
	bne	err2	 closing quote
lb5	tya		set the length
	dec	A
	dec	A
	sta	[r12]
	lda	r12	return the pointer
	ldx	r12+2
	clc
	rts

err1	lda	#outOfMem	could not allocate memory
	jsl	SysError
	sec
	rts

err2	free	r12	dispose of the work area
	puts	#'Mismatched quotes',cr=t,errout=t
	sec
	rts

quote	ds	2	quote flag
	end

****************************************************************
*
*  GetToken - read a (possibly quoted) string from the command line
*
*  Outputs:
*	X-A - pointer to a GS/OS input string; null for error
*	C - set for error, else clear
*
*  Notes:
*	1. If no error is flagged, the caller is responsible
*	   for doing a dispose on the pointer.
*
****************************************************************
*
GetToken start
	debug GetToken
	using LineCommon
	using Common

	jsr	LineLength	reserve space for the variable
	inc	A
	inc	A
	jsl	Malloc
	sta	r12
	stx	r14
	ora	r14
	beq	err1
	stz	quote	assume that there is no quote
	ldy	#2	no characters read so far
lb0	jsr	GetChar	get the first character from the line
	jsl	IsWhite
	bcs	lb0
	cmp	#'"'	branch if it is not a quote
	bne	lb2
	inc	quote	note that there was an opening quote

lb1	jsr	GetChar	get the next character
lb2	cmp	#RETURN	branch if all characters have been read
	beq	lb4
	ldx	quote	if there was a starting quote then
	beq	lb2a
	cmp	#'"'	  if it is a quote then...
	bne	lb3
	jsr	GetChar	    get the next char, too.
	cmp	#RETURN	    if there are no more chars then
	beq	lb5	      we are finished
	cmp	#'"'	    if it is a quote then
	beq	lb3	      include it as a character
	bra	lb5	    else we are done
lb2a	jsl	IsWhite	else if this is whitespace then
	bcs	lb5	  we are done
lb3	short M	save the character in the string
	sta	[r12],Y
	long	M
	iny
	bra	lb1

lb4	ldx	quote	make sure we were not expecting a
	bne	err2	 closing quote
lb5	tya		set the token length
	dec	A
	dec	A
	sta	[r12]
	lda	r12	return the pointer
	ldx	r12+2
	clc
	rts

err1	lda	#outOfMem	could not allocate memory
	jsl	SysError
	sec
	rts

err2	free	r12	dispose of the work area
	puts	#'Mismatched quotes',cr=t,errout=t
	sec
	rts

quote	ds	2	quote flag
	end

****************************************************************
*
*  LastCLine - Get the last line
*
*  Outputs:
*	C - set if error, else clear
*
****************************************************************
*
LastCLine start
	debug LastCLine
	using Common
	using LineCommon

lb1	jsr	Pause	check for early exit
	bcs	err2
	jsr	LastCommand	last command on this line
	bcc	lb2
	clc
	rts

lb2	jsr	ClearLine	dispose of old line
	jsr	LastLine	last line in EXEC file
	bcs	err
	lda	eLength	skip to last command in line
	sta	eDisp
	bra	lb1

err	puts	#'Error during line skip',cr=t,errout=t
err2	lda	#true
	sta	exec_exit
	sec
	rts
	end

****************************************************************
*
*  LastCommand - last command to LINE
*
*  Inputs:
*	ep - pointer to exec file line
*	eDisp - disp in exec file
*
*  Outputs:
*	eDisp - points to next command line
*	lp - pointes to expanded command line
*	lLength - length of command line
*	lDisp - set to 0
*	C - set if command found
*
****************************************************************
*
LastCommand private
	debug LastCommand
	using LineCommon
	using Common
;
;  Initialization
;
	lda	ep	quit if there is no line
	ora	ep+2
	beq	no1
;
;  Skip back to the start of the previous line
;
	short M
	ldy	eDisp	branch if we are at the start
	beq	no1
	dey		branch if the start is a blank line
	beq	lb4
	lda	#RETURN	skip back to the start or the revious
lb1	dey		  RETURN
	beq	lb2
	cmp	[ep],Y
	bne	lb1
	bra	lb3
lb2	cmp	[ep],Y
	bne	lb4
lb3	iny
lb4	sty	eDisp	set the displacement
	long	M
	phy
	jsr	NextCommand	expand shell variables
	pl2	eDisp
	sec
	rts
;
;  Exit here if no previous line exists
;
no1	long	M
	clc
	rts
	end

****************************************************************
*
*  LastLine - Last line in EXEC list
*
*  Inputs:
*	EXEC_PTR - pointer to exec file
*	EXEC_DISP - displacement into file
*
*  Outputs:
*	lp - pointer to line read
*	lDisp - 0
*	lLength - length of the line
*	C - set if error, else clear if line read
*
****************************************************************
*
LastLine private
	debug LastLine
	using RecCom
	using Common
d_addr	equ	0	disp to pointer to EXEC file
r_next	equ	0	disp to handle of next entry
r_addr	equ	4	disp to pointer to EXEC file
r_disp	equ	8	disp to disp into EXEC file
r	equ	r0	pointer to current entry
next	equ	r4+r_next	handle of next entry
addr	equ	r4+r_addr	addr of EXEC file
disp	equ	r4+r_disp	disp into EXEC file

	move4 exec_ptr,r4	get the exec file pointer
	ldy	exec_disp
	jsr	Back	back up two lines
	bcs	err
	jsr	Back
	bcs	err
	sty	exec_disp	save disp into exec file
	jsr	ClearLine	clear the old line
	jmp	NextLine	read the line

err	sec		no line found
	rts
;
;  Back - back up one line
;
Back	tya		error if at start of list
	beq	err
bk0	dey		error if last char is not RETURN
	lda	[r4],Y
	and	#$007F
	cmp	#RETURN
	bne	err
bk1	dey		back up to last RETURN or to start of
	bmi	bk2	 list
	lda	[r4],Y
	and	#$007F
	cmp	#RETURN
	bne	bk1
bk2	iny
	beq	bk5	if not at the start of the list then
	short M	  see if this line was continued
	phy
	dey
	beq	bk4
bk3	dey
	beq	bk4
	lda	[r4],Y
	cmp	#' '
	beq	bk3
	cmp	#TAB
	beq	bk3
	cmp	#'\'
	bne	bk4
	long	M
	ply
	bra	bk0
bk4	long	M
	ply
bk5	clc
	rts
	end

****************************************************************
*
*  LineAddr - return a pointer to the command line
*
*  Note: this command line is a separately allocated dynamic
*  variable.  The pointer must be disposed of by the caller
*  when the need for the line is exhausted.  There must be a
*  line when this routine is called.
*
*  Inputs:
*	lp - line pointer
*	lDisp - disp in lp
*	quote - processing a quoted string?
*
*  Outputs:
*	X-A - address
*
****************************************************************
*
LineAddr start
	debug LineAddr

	ph2	lDisp	save volitile labels
	stz	lDisp	go back to the start of the line
	jsr	LineLength	get the length of the line
	sta	len
	inc	A	reserve space for it
	jsl	Malloc
	sta	r12
	stx	r14
	ora	r14
	beq	lb3
	ldy	len	move in the line
	short M
	lda	#0
	sta	[r12],Y
	tyx
	beq	lb3
	dey
	beq	lb2
lb1	lda	[lp],Y
	sta	[r12],Y
	dey
	bne	lb1
lb2	lda	[lp]
	sta	[r12]
lb3	long	M
	pl2	lDisp	restore volitile variables
	lda	r12	return the result
	ldx	r14
	rts

len	ds	2
	end

****************************************************************
*
*  LineLength - return the number of characters left in the
*	current line.
*
*  Inputs:
*	lp - pointer to the current line
*	lDisp - disp in lp for next character
*	quote - processing a quoted string?
*
*  Outputs:
*	A - # bytes left
*
****************************************************************
*
LineLength start
	debug LineLength
	using Common
	using LineCommon
;
;  Handle a line in the GetLine line buffer
;
	lda	line+1	if there is a line in the GetLine buffer
	and	#$00FF
	bne	lb3	  return it's length
;
;  Handle a line in the input line buffer
;
	lda	lp	quit if there is no line
	ora	lp+2
	beq	lb3
	short M	use short A to scan the line
	ldy	lDisp	get the current disp
lb1	lda	[lp],Y	skip to the RETURN
	cmp	#RETURN
	beq	lb2
	iny
	bra	lb1

lb2	long	M	compute the line length
	tya
	sec
	sbc	lDisp
lb3	rts
	end

****************************************************************
*
*  NextChar - return a character and increment the index
*
*  Inputs:
*	X - index into the line
*	lp - command line
*
*  Outputs:
*	A - character read; RETURN if at end of line
*	X - incremented
*
*  Notes:
*	1. Y is not changed.
*	2. Direct page is not changed.
*	3. The A register may be long or short on entry.
*
****************************************************************
*
NextChar start
	using Common
	using LineCommon
	debug NextChar
;
;  Handle the user line
;
	php		save the caller's reg size
	long	M
	lda	line+1	branch if there is no user line
	and	#$00FF
	beq	lb1
	sta	length	return $0D if the disp is past the end
	cpx	length	 of the user line
	blt	ul1
	lda	#$000D
	bra	rts
ul1	inx		return the character
	lda	line+1,X
	and	#$00FF
	bra	rts
;
;  Handle input from the line buffer
;
lb1	phy		get the disp into the line buffer
	txa
	clc
	adc	lDisp
	cmp	lLength	branch if past the line
	bge	none
	tay		get the character
	lda	[lp],Y
	and	#$00FF
	cmp	#RETURN	branch if at end of line
	beq	none
	inx		inc the index
none	ply		restore caller's Y
rts	plp
	rts
;
;  local data
;
length	ds	2
	end

****************************************************************
*
*  NextCLine - Get the next line
*
*  Outputs:
*	lDisp,lStart - disp to next command line
*	C - set if error, else clear
*
****************************************************************
*
NextCLine start
	debug NextCLine
	using LineCommon

lb1	jsr	Pause	check for exit
	bcs	rts
	jsr	NextCommand	next command on this line
	bcc	lb2
	clc		there was one - return
rts	rts

lb2	jsr	ClearLine	dispose of old line
	jsr	NextLine	next line in EXEC file
	bcc	lb1	(condition code set by NextLine)
	rts
	end

****************************************************************
*
*  NextCommand - next command in the line buffer
*
*  Inputs:
*	ep - pointer to exec file line
*	eDisp - disp in exec file
*
*  Outputs:
*	eDisp - points to next command line
*	lp - pointes to expanded command line
*	lLength - length of command line
*	lDisp - set to 0
*	C - set if command found
*
****************************************************************
*
NextCommand private
	debug NextCommand
	using Common
	using RecCom
	using LineCommon

	lda	ep	quit if no line in buffer
	ora	ep+2
	jeq	no
	ldy	eDisp	quit if we are past the end of the line
	cpy	eLength
	jge	no
	lda	lAllocated	dispose of any old line pointer
	beq	lb0
	free	lp

lb0	move4 ep,r0	set up so NextExecChar reads from the
	ph2	exec_disp	 line in ep
	lda	eDisp
	sta	exec_disp
	ph4	ep	set up so PutChar writes to a new buffer
	ph2	eLength
	ph2	eAllocated
	stz	eLength
	stz	buffSize
	stz	buffSize+2
	lla	ep,lLine
	lla	buffSize,lineLen
	stz	eAllocated

lb1	jsr	NextExecChar	get a character for the command line
	lda	ch
	cmp	#'{'	if it is '{' then
	bne	lb2
	jsr	ExpandShellVariable	  set up to read from a shell variable
	bra	lb1
lb2	cmp	#RETURN	if it is not a return then
	beq	lb3
	jsr	PutChar	  write the character
	bcc	lb1	  next character
lb2a	clc		  (branch for errors)
	bra	lb4

lb3	jsr	PutChar	write the RETURN
	bcs	lb2a
	sec
lb4	php		save the exit status
	move4 ep,lp	set up the new line info
	lda	eLength
	sta	lLength
	lda	eAllocated
	sta	lAllocated
	stz	lDisp
	lda	exec_disp	update eDisp
	inc	A
	sta	eDisp
	plp		get the error code
	pl2	eAllocated	restore exec line vars
	pl2	eLength
	pl4	ep
	pl2	exec_disp
	rts

no	clc
	rts
	end

****************************************************************
*
*  NextExecChar - return the next character from the EXEC file
*
*  Inputs:
*	r0 - pointer to the exec file
*	exec_disp - displacement into the file
*
*  Outputs:
*	ch - character read
*
*  Notes:
*     1. The pointer will not be advanced past the end of file
*	or past a RETURN in the exec file.  In either case,
*	ch is set to RETURN.
*     2. If a \ is encountered in the EXEC file, the routine
*	scans ahead.  If all characters are whitespace up to
*	the next RETURN character, the first character after the
*	RETURN character is returned.
*     3. Expands shell variables.
*
****************************************************************
*
NextExecChar private
	debug NextExecChar
	using LineCommon
	using Common
	using RecCom
;
;  If there is an active shell variable, get a character from it
;
	lda	shellLen	branch if there is no active variable
	beq	lb0
	ph4	r0	save r0
	move4 shellPtr,r0	get the pointer
	lda	[r0]	return the character
	and	#$00FF
	sta	ch
	inc4	shellPtr	next character
	dec	shellLen
	pl4	r0
	rts
;
;  Get a character from the EXEC file
;
lb0	ldy	exec_disp	if the next char is a RETURN or $00,
	lda	[r0],Y	  set ch to RETURN and quit
	and	#$00FF
	bne	lb1
	lda	#RETURN
lb1	cmp	#RETURN
	beq	lb3
	inc	exec_disp	inc the disp
	sta	ch	save the character
	cmp	#'\'	check for continuation lines
	bne	lb3
	jsr	Continuation
	bcs	lb0
lb3	sta	ch
	rts
	end

****************************************************************
*
*  NextLine - Get the next line in the EXEC list
*
*  Inputs:
*	exec_ptr - pointer to the exec file
*	exec_disp - displacement into the file
*
*  Outputs:
*	lp - line pointer
*	lLength - length of the line
*	lDisp - 0
*	C - set if error, else clear
*
****************************************************************
*
NextLine private
	debug NextLine
	using LineCommon
	using Common
	using RecCom

	ph4	r0	save the dp we will use
	ph4	r4
	stz	eDisp	zero the displacement
	move4 exec_ptr,r0	set up for character fetches
	ldy	exec_disp	quit if at end of file
	lda	[r0],Y
	and	#$00FF
	jeq	err

	stz	eLength	no characters so far
	stz	buffSize	no buffer allocated
	stz	buffSize+2
	lda	eAllocated	if eAllocated then
	beq	in0
	free	ep	  free the line buffer
	stz	eAllocated	  mark the buffer as unallocated
in0	lla	ep,eLine	start with the fixed line buffer
	lla	buffSize,lineLen
	lda	#'0'	no pipes so far
	sta	pipenum

in1	jsr	NextExecChar	get the first character
;
;  Scan the line
;
	lda	ch	place leading blanks in the line
	cmp	#' '
	bne	sl2
	jsr	PutChar
	jcs	err
	bra	in1

sl2	lda	ch	skip the alias check if the line is quoted
	cmp	#'"'
	beq	sl5
	lda	eLength	mark the start of the first token
	sta	firstToken
sl3	lda	ch	scan to the first whitespace character
	cmp	#RETURN
	beq	sl4
	cmp	#'"'
	beq	sl4
	cmp	#';'
	beq	sl4
	cmp	#'|'
	beq	sl4
	cmp	#' '
	beq	sl4
	cmp	#TAB
	beq	sl4
	jsr	PutChar
	jcs	err
	jsr	NextExecChar
	bra	sl3
sl4	jsr	ExpandAlias	expand the alias
	jcs	err

sl5	stz	quote	scan to the end of the line
sl6	lda	ch	quit if we found a RETURN
	cmp	#RETURN
	beq	sl11
	cmp	#'"'	if we have a quote then
	bne	sl7
	lda	quote	  flip the quote flag
	eor	#1
	sta	quote
	lda	ch
sl7	ldx	quote	if we are not processing a string then
	bne	sl8
	cmp	#';'	  quit if ch in [';','|']
	beq	sl10
	cmp	#'|'
	beq	sl9
sl8	jsr	PutChar	copy the character to the buffer
	bcs	err
	jsr	NextExecChar	next character
	bra	sl6

sl9	jsr	ExpandPipe	expand pipes into the line
	jcc	in1
	bra	err

sl10	lda	#RETURN	put a RETURN in the line
	jsr	PutChar
	jcc	in1
	bra	err

sl11	lda	#RETURN	mark the end of the last line
	jsr	PutChar
	bcs	err
	inc	exec_disp	skip the return in the EXEC file
	lda	skipping	if not skipping then
	bne	sl12
	lla	r12,echoStr	  if ECHO then
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	sl12
	jsr	PutLine	    write line to screen
sl12	pl4	r4	restore dp
	pl4	r0
	clc		return with a new line
	rts

err	pl4	r4	restore dp
	pl4	r0
	sec		return with error
	rts

quote	ds	2	processing quoted string?
	end

****************************************************************
*
*  PullLine - Pull command line record
*
*  Inputs:
*	lineNext - pointer to saved node
*
*  Outputs:
*	lp - pointer to the command line
*	lDisp - current disp in the command line
*	lineNext - pointer to next node in chain
*	lLength - length of the line
*	lStart - lDisp at start of last line
*	C - set if successful pull done, clear if none possible
*
****************************************************************
*
PullLine start
	using LineCommon
	debug PullLine
reclen	equ	20

	lda	lineNext	branch if no next node
	ora	lineNext+2
	beq	lb2
	jsr	ClearLine	dispose of any existing line
	move4 lineNext,r0	move pointer to DP
	ldy	#reclen-2	move record
lb1	lda	[r0],Y
	tyx
	sta	lineNext,X
	dey
	dbpl	Y,lb1
	free	r0	dump old record
	lda	#1	buffers are allocated
	sta	eAllocated
	sta	lAllocated
	sec
	rts

lb2	clc		no pull is possible
	rts
	end

****************************************************************
*
*  PushLine - Push command line record
*
*  Inputs:
*	lp - pointer to the command line
*	lDisp - current disp in the command line
*	lineNext - pointer to next node in chain
*	lLength - length of the line
*	ep - exec file line pointer
*	eDisp - exec file line disp
*	eLength - exec file line length
*
*  Outputs:
*	lineNext - pointer to saved node
*	C - set if successful
*
****************************************************************
*
PushLine start
	using Common
	debug PushLine
reclen	equ	20	length of the record to save

	stz	r8	r8 = nil
	stz	r10
	lda	lp	if there is a current line then
	ora	lp+2
	beq	lb5
	ldy	#0	  find it's length
	short	M
lb1	lda	[lp],Y
	cmp	#RETURN
	beq	lb2
	iny
	bra	lb1
lb2	long	M	  reserve a new line buffer
	tya
	inc	A
	lsr	A
	bcc	lb2a
	inc	A
lb2a	asl	A
	sta	len
	jsl	Malloc
	sta	r0
	stx	r2
	ora	r2
	jeq	bad
	ldy	len	  copy the line to the new buffer
	dey
	dey
	beq	lb4
lb3	lda	[lp],Y
	sta	[r0],Y
	dey
	dey
	bne	lb3
lb4	lda	[lp]
	sta	[r0]
lb5	anop		endif

	lda	ep	if there is a current exec line then
	ora	ep+2
	beq	lc5
	ldy	#0	  find it's length
	short	M
lc1	lda	[ep],Y
	beq	lc2
	iny
	bra	lc1
lc2	long	M	  reserve a new line buffer
	tya
	inc	A
	lsr	A
	bcc	lc2a
	inc	A
lc2a	asl	A
	sta	len
	jsl	Malloc
	sta	r8
	stx	r10
	ora	r10
	beq	bad
	ldy	len	  copy the line to the new buffer
	dey
	dey
	beq	lc4
lc3	lda	[ep],Y
	sta	[r8],Y
	dey
	dey
	bne	lc3
lc4	lda	[ep]
	sta	[r8]
lc5	anop		endif

	malloc #reclen	get space for record
	sta	r4
	stx	r6
	ora	r6	branch if out of memory
	beq	bad
	ph4	lp	save the current lp, ep
	ph4	ep
	move4 r0,lp	use the new line buffer
	move4 r8,ep	use the new exec buffer
	ldy	#reclen-2	move record to allocated memory
lb6	tyx
	lda	lineNext,X
	sta	[r4],Y
	dey
	dbpl	Y,lb6
	move4 r4,lineNext	save the pointer
	pl4	ep	restore the current lp, ep
	pl4	lp
	sec
	rts

bad	free	r0	dispose of any new line buffers
	free	r8
	clc		out of memory
	rts

len	ds	2	length of the line
	end

****************************************************************
*
*  PutChar - write a character to the line buffer
*
*  Inputs:
*	ep - pointer to the line buffer
*	A - character to write
*	eLength - # chars in the line buffer
*	buffSize - size of the current line buffer
*
*  Outputs:
*	C - set if an error occurred
*
****************************************************************
*
PutChar	private
	debug PutChar
	using Common
	using LineCommon

	ldy	eLength	branch if the buffer is not full
	cpy	buffSize
	jlt	lb5

	pha		save the character
	lda	buffSize+2	flag error if the buffer is already 64K
	beq	lb1
	puts	#'Line too long.',cr=t,errout=t
	pla
	sec
	rts

lb1	add4	buffSize,#256	flag error if out of memory
	mlalloc buffsize
	sta	r4
	stx	r6
	ora	r6
	bne	lb2
	lda	#outOfMem
	jsl	SysError
	pla
	sec
	rts

lb2	sec		move the old characters to the new buffer
	lda	buffSize
	sbc	#256
	tay
	dey
	dey
lb3	lda	[ep],Y
	sta	[r4],Y
	dey
	dey
	bne	lb3
	lda	[ep]
	sta	[r4]
	lda	eAllocated	if there was an old buffer then
	beq	lb4
	free	ep	  dispose of it
lb4	inc	eAllocated	set the allocated buffer flag
	move4 r4,ep	set the addr of the new buffer
	ldy	eLength	get the disp
	pla		restore the character
lb5	short M	save the character
	sta	[ep],Y
	long	M
	inc	eLength	update the length
	clc
	rts
	end

****************************************************************
*
*  PutLine - Write the command line to standard out
*
*  Inputs:
*	ep - pointer to the command line
*	eLength - length of the command line
*
****************************************************************
*
PutLine	private
	debug PutLine
	using Common

	ldy	#0
pl1	cpy	eLength
	bge	pl3
	lda	[ep],Y
	and	#$00FF
	cmp	#RETURN
	bne	pl2
	iny
	cpy	eLength
	beq	pl2
	dey
	lda	#';'
pl2	jsl	~stdout
	iny
	bra	pl1
pl3	rts
	end

****************************************************************
*
*  RemoveChar - remove a character from the command line
*
*  Inputs:
*	X - index +1 of character to remove
*
*  Notes:
*	1. Y is not changed.
*	2. Direct page is not changed.
*	3. The A register may be long or short on entry.
*
****************************************************************
*
RemoveChar start
	using LineCommon
	debug RemoveChar

	php
	long	M
	dex
	phx
	phy

	txa
	clc
	adc	lDisp
	tay
	short M
gb1	cpy	lLength
	bge	gb2
	iny
	lda	[lp],Y
	dey
	sta	[lp],Y
	iny
	bra	gb1
gb2	long	M
	dec	lLength
	ply
	plx
	plp
	rts
	end

****************************************************************
*
*  SkipBlanks - Skip blanks in command line
*
*  Inputs:
*	lDisp - disp in command line
*
*  Outputs:
*	lDisp - disp in command line with any leading blanks removed
*
****************************************************************
*
SkipBlanks start
	debug SkipBlanks
	using Common

	php
	long	I
	short M
	ldy	lDisp
sk1	lda	[lp],Y
	jsl	IsWhite
	bcc	sk3
	iny
	bra	sk1
sk3	sty	lDisp
	plp
	rts

	longa on
	end
