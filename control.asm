	keep	obj/control
	mcopy control.macros
	copy	sh.dp
****************************************************************
*
*  This unit contains the subroutines needed by the shell
*  commands used for flow of control.
*
****************************************************************
*
*  CCommon - Control common area
*
****************************************************************
*
CCommon	privdata
;
;  Constants
;
expr_max equ	32	size of the expression stack
;
;  String constants
;
loop	dosw	LOOP
for	dosw	FOR
if	dosw	IF
end	dosw	END
else	dosw	ELSE
one	dosw	'1'
zero	dosw	'0'
caseflag dosw	CaseSensitive
eq	dosw	'=='
ne	dosw	'!='
lparen	dosw	'('
rparen	dosw	')'
;
;  Variables
;
expr_sp	ds	2	expression stack pointer
expr_stack ds	expr_max	expression stack

nest_level ds	2	current nest level
	end

****************************************************************
*
*  BackUp - Back up to the top of a loop
*
*  Outputs:
*	C - set if error occurred
*	A - error number
*
****************************************************************
*
BackUp	start
	debug BackUp
	using Common
	using CCommon

	stz	nest_level	init nest level
	sta	skipping	note that we are skipping
	stz	r0	r0 = nil
	stz	r2
lb1	free	r0	free the old buffer
	jsr	Spin	spin the spinner
	jsr	LastCLine	get last line
	bcs	rts	quit if error
	jsr	GetToken	get command
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	ph4	r0	if token = IF or LOOP or FOR then
	ph4	#if
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#loop
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#for
	jsl	CompareOSStrings
	tax
	bne	lb1b
lb1a	dec	nest_level	  ++nest_level
	bne	lb1	  if nest level > 0 then continue
	bra	lb2
lb1b	ph4	r0	if it is an END then
	ph4	#end
	jsl	CompareOSStrings
	tax
	bne	lb1
	inc	nest_level	  nest_level--
	bra	lb1	  continue

lb2	free	r0	free the buffer
	jsr	NextCLine	skip the original command
	clc		successful skip
rts	stz	skipping	no longer skipping
	rts

oom	lda	#outOfMem	out of memory error
	sec
	rts
	end

****************************************************************
*
*  DoContinue - Continue to end of loop
*
*  Outputs:
*	C - set if error
*
****************************************************************
*
DoContinue start
	debug DoContinue
	using Common

lb1	jsr	TopType	while tos == ifsy do
	cmp	#ifsy
	bne	lb2
	jsr	ScanToEnd	  skip to end
	bcs	rts
	jsr	PopStack	  pop tos
	bra	lb1	endwhile
lb2	jsr	ScanToEnd	skip to end
rts	rts
	end

****************************************************************
*
*  DoElse - Scan to ELSE or END
*
****************************************************************
*
DoElse	start
	debug DoElse
	using Common
	using CCommon

	stz	nest_level	initialize nest level
	lda	#1	note that we are skipping
	sta	skipping
	stz	r0	r0 = nil
	stz	r2
lb1	free	r0	free the old token buffer
	jsr	Spin	spin the spinner
	jsr	NextCLine	get next line
	jcs	err	quit if error
	jsr	GetToken	get the command name
	sta	r0
	stx	r2
	ora	r2
	jeq	err
	ph4	r0	if token = IF or LOOP or FOR then
	ph4	#if
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#loop
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#for
	jsl	CompareOSStrings
	tax
	bne	lb1b
lb1a	inc	nest_level	  ++nest_level
	bra	lb1
lb1b	ph4	r0	if it is an ELSE then
	ph4	#else
	jsl	CompareOSStrings
	tax
	bne	lb3
	lda	nest_level	  if nest_level > 0 then continue
	bne	lb1
	free	r0	  next token
	jsr	GetToken
	sta	r0
	stx	r2
	ora	r2
	beq	err
	ph4	r0	  if token == "IF" then
	ph4	#if
	jsl	CompareOSStrings
	tax
	bne	lb2
	free	r0	    free token buffer
	jsr	Expression	    if !expression then
	stz	r0	      r0 = nil
	stz	r2
	jcc	lb1	      skip again
!			    endif
lb2	anop		  endif

	free	r0	  free token buffer
	clc		  return
	ldx	#0
	rts

lb3	ph4	r0	else if it is an END then
	ph4	#end
	jsl	CompareOSStrings
	tax
	jne	lb1
	dec	nest_level	  nest_level--
	jpl	lb1	  if nest_level >= 0 then continue
	free	r0	  free the token buffer
	clc		  return
	ldx	#1
	stz	skipping	stop skipping
	rts
!			else loop

SkipError entry 	used by ScanToEnd to save space
err	puts	#'Error during line skip',cr=t,errout=t
	lda	#true	stop the EXEC
	sta	exec_exit
	sec
	ldx	#0
	stz	skipping	stop skipping
	rts
	end

****************************************************************
*
*  Expression - Evaluate an expression
*
*  Inputs:
*	LINE - command line, with expression
*
*  Outputs:
*	C - set if true, clear if false
*
****************************************************************
*
Expression start
	debug Expression
	using Common
	using CCommon

	lla	r12,caseflag	caseSensitive = exists(casesensitive)
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	ex1
	lda	#1
ex1	sta	caseSensitive
	stz	expr_sp	sp = 0
	jsr	GetToken	nexttoken
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	jsr	StringExpression
	pha		save error
	jsr	PopOperand	pop(r0)
	lda	r0	carryflag = term1 == "1"
	ldx	r2
	phx
	pha
	phx
	pha
	ph4	#one
	jsl	CompareOSStrings
	tax
	beq	ex3
	pla		free the token buffer
	plx
	jsl	Free
	jsr	PurgeExpression	purge the stack
	pla		return error
	clc
	rts

ex3	pla		free the token buffer
	plx
	jsl	Free
	jsr	PurgeExpression	purge the stack
	pla		return error
	sec
	rts

oom	lda	#outOfMem	flag an out of memory error
	jsl	SysError
	bra	ex3
	end

****************************************************************
*
*  ForLoop - Do the loop part of a for loop
*
*  Outputs:
*	A - error code
*
****************************************************************
*
ForLoop	start
	debug ForLoop
	using Common

	jsr	BackUp	back up to the top of the loop
	bcc	ForLoop2
	rts

ForLoop2 entry		entry for first call

	stz	value	value := nil
	stz	value+2
	jsr	PopStack	remove the forsy
	jsr	PopWord	get the name of the variable
	move4 r0,varName
	jsr	TopType	see if we're at the end
 	cmp	#forsy
	beq	fo2
	jsr	PopWord	get the next value
	move4 r0,value
	OSSet stRec	set the variable
	lda	varName	place variable name back on stack
	jsr	Stack
	lda	varName+1
	jsr	Stack
	lda	varName+2
	jsr	Stack
	lda	varName+3
	jsr	Stack
	lda	#forsy	stack the for loop indicator
	jsr	Stack
	free	value	free the value string buffer
fo1	lda	#0
	rts

fo2	jsr	PopStack	remove forsy
	free	varName	free(varName)
	free	value	free(value)
	jsr	ScanToEnd
	bcc	fo1
	lda	#$FFFF
	rts

stRec	dc	i'3'	OSSet record
varName	ds	4	variable name pointer
value	ds	4	value pointer
	dc	i'0'	don't export
	end

****************************************************************
*
*  PopOperand - Pop a string from the expression stack
*
*  Inputs:
*	EXPR_STACK - expression stack
*	EXPR_SP - expression stack pointer
*
*  Outputs:
*	r0 - address of OS string; nil for empty stack
*
*  Notes:
*	The OS string is in a dynamically allocated buffer.
*	It is up to the caller to dispose of the memory.
*
****************************************************************
*
PopOperand private
	debug PopOperand
	using CCommon

	stz	r0	r0 = nil
	stz	r2
	lda	expr_sp	quit if stack is empty
	beq	lb1
	sec		pop the operand
	sbc	#4
	sta	expr_sp
	tax
	lda	expr_stack,X
	sta	r0
	lda	expr_stack+2
	sta	r2
lb1	rts
	end

****************************************************************
*
*  PopStack - Pop instruction stack
*
*  Inputs:
*	INS_PTR - pointer to instruction stack
*	INS_DISP - disp in instruction stack
*
*  Outputs:
*	A - byte poped from stack
*
****************************************************************
*
PopStack start
	debug PopStack
	using Common
	using RecCom
ptr	equ	1	work pointer
val	equ	5	return value

	sub	,6

	ldy	ins_disp	if nothing on stack then
	bne	lb1
	lda	#endsy	  report endsy
	bra	lb2

lb1	move4 ins_ptr,ptr	get the pointer
	dec	ins_disp	dec stack pointer
	ldy	ins_disp	read tos
	lda	[ptr],Y
	and	#$00FF
lb2	sta	val
	ret	2:val
	end

****************************************************************
*
*  PopWord - Pop word
*
*  Outputs:
*	R0 - addr of the OS string
*
*  Notes:
*	The pointer returned points to a dynamically allocated
*	buffer.  It is up to the caller to dispose of the
*	buffer.
*
*	This subroutine assumes there is a word on the stack
*	to pop.
*
****************************************************************
*
PopWord	start
	debug PopWord
	using Common

	ldx	#3
lb1	phx
	jsr	PopStack
	plx
	short M
	sta	r0,X
	long	M
	dex
	bpl	lb1
	rts
	end

****************************************************************
*
*  PurgeExpression - remove and parameters on the expression stack
*
*  Inputs:
*	expr_sp - expression stack pointer
*
****************************************************************
*
PurgeExpression start
	debug PurgeExpression
	using CCommon

lb1	jsr	PopOperand
	free	r0
	lda	r0
	ora	r2
	bne	lb1
	rts
	end

****************************************************************
*
*  PushOperand - Push a string to the expression stack
*
*  Inputs:
*	EXPR_STACK - expression stack
*	EXPR_SP - expression stack pointer
*	R0 - address of string
*
*  Outputs:
*	C - set for stack overflow
*
*  Notes:
*	The string must be in a dynamically allocated buffer.
*
****************************************************************
*
PushOperand private
	debug PushOperand
	using CCommon

	ldx	expr_sp
	cpx	expr_max
	beq	err
	lda	r0
	sta	expr_stack,X
	lda	r2
	sta	expr_stack+2,X
	inx
	inx
	inx
	inx
	stx	expr_sp
	clc
	rts

err	sec
	rts
	end

****************************************************************
*
*  PushWord - Push word
*
*  Inputs:
*	R0 - addr of OS string to push
*
*  Outputs:
*	C - set if error
*
****************************************************************
*
PushWord start
	debug PushWord
	using Common

	ph4	r0	make a copy of the string
	jsl	DuplicateOSString
	sta	buff
	stx	buff+2
	ora	buff+2
	beq	err3
	ldx	#0	push the address of the string
lb1	lda	buff,X
	phx
	jsr	Stack
	plx
	bcs	err1
	inx
	cpx	#4
	bne	lb1
	clc		return with no error
	rts

err1	txa		pop bytes already pushed
	beq	err2
	phx
	jsr	PopStack
	plx
	dex
	bra	err1
err2	free	buff	free the buffer
err3	sec
	rts

buff	ds	4	string pointer buffer
	end

****************************************************************
*
*  ScanToEnd - Scan to END
*
*  Outputs:
*	C - set if error
*
****************************************************************
*
ScanToEnd start
	debug ScanToEnd
	using Common
	using CCommon

	stz	skipping	note that we are skipping
	stz	nest_level	init nest level
	stz	r0	r0 = nil
	stz	r2
lb1	free	r0	free the old token buffer
	jsr	Spin	spin the spinner
	jsr	NextCLine	get lext line
	jcs	SkipError	quit if error
	jsr	GetToken	get command
	sta	r0	if token = IF or LOOP or FOR then
	stx	r2
	ora	r2
	beq	oom
	ph4	r0
	ph4	#if
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#loop
	jsl	CompareOSStrings
	tax
	beq	lb1a
	ph4	r0
	ph4	#for
	jsl	CompareOSStrings
	tax
	bne	lb1b
lb1a	inc	nest_level	  ++nest_level
	bra	lb1
lb1b	ph4	r0	if it is an END then
	ph4	#end
	jsl	CompareOSStrings
	tax
	bne	lb1
	dec	nest_level	  nest_level--
	bpl	lb1	  if nest_level >= 0 then continue
	free	r0	  free the token buffer
	stz	skipping	  stop skipping
	clc		  return
	rts
!			else loop

oom	lda	#outOfMem
	jsl	SysError
	sec
	rts
	end

****************************************************************
*
*  Stack - Stack a value on the instruction stack
*
*  Inputs:
*	INS_PTR - instruction stack pointer
*	INS_DISP - disp in instruction stack
*	INS_SIZE - current size of instruction stack
*	A - byte to push
*
*  Outputs:
*	C - set if error
*
****************************************************************
*
Stack	start
	debug Stack
	using Common
	using RecCom

	sta	r4	save value
	move4 ins_ptr,r0	get stack pointer
	ldy	ins_disp	if stack is full then
	cpy	ins_size
	blt	lb1
	tya		  see if it can be grown
	clc
	adc	#ins_stack_size
	bcs	err1
	sta	ins_size	  yes - save new size
	jsl	Malloc
	sta	r8
	stx	r10
	ora	r10
	beq	err2
	ph4	r0	  copy the memory
	ph4	r8
	ph2	#0
	ph2	ins_size
	_BlockMove
	move4 r8,r0	  r0 = r8
	move4 r8,ins_ptr	  save the new pointer

lb1	ldy	ins_disp	put the value on the stack
	short M
	lda	r4
	sta	[r0],Y
	long	M
	inc	ins_disp	inc stack pointer
	clc
	rts

err1	puts	#'Stack overflow',cr=t,errout=t
	bra	erts

err2	lda	#outofmem	out of memory
	jsl	SysError
	sub2	ins_size,#ins_stack_size
erts	sec
	rts
	end

****************************************************************
*
*  StringExpression - Evaluate a string expression
*
*  Inputs:
*	LINE - line containing expression
*	r0 - first token of expression
*
*  Outputs:
*	A - error code
*	stack - expression value
*
****************************************************************
*
StringExpression private
	debug StringExpression
	using Common
	using CCommon

	jsr	Term	if !(err = term) then
	sta	err
	tax
	jne	lb11
lb1	ph4	r0	  while (token == "==") or
	ph4	#eq
	jsl	CompareOSStrings
	tax
	beq	lb2
	ph4	r0	    (token == "!=") do
	ph4	#ne
	jsl	CompareOSStrings
	tax
	jeq	lb6
	free	r0
	brl	lb11
lb2	anop		    if token == "==" then
	free	r0	      free(r0)
	jsr	GetToken	      nexttoken
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
	jsr	Term	      if err = term then
	tax			return err
	beq	lb3
	rts
lb3	move4 r0,toke	      save the next token ptr
	jsr	PopOperand	      pop(term1)
	ph4	r0
	move4 r0,term1
	jsr	PopOperand	      pop(term2)
	ph4	r0
	move4 r0,term2
	jsl	CompareOSStrings	      if term1 == term2 then
	tax
	bne	lb4
	ph4	#one		stack("1")
	jsl	DuplicateOSString
	sta	r0
	stx	r2
	jsr	PushOperand
	bra	lb5	      else
lb4	ph4	#zero		stack("0")
	jsl	DuplicateOSString
	sta	r0
	stx	r2
	jsr	PushOperand
lb5	move4 toke,r0	      pop the token pointer
!			      endif
	brl	lb10	    else
lb6	free	r0	      Free(r0)
	jsr	GetToken	      nexttoken
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
	jsr	Term	      if err = term then
	tax			return err
	beq	lb7
	rts
lb7	move4 r0,toke	      save the next token ptr
	jsr	PopOperand	      pop(term1)
	ph4	r0
	move4 r0,term1
	jsr	PopOperand	      pop(term2)
	ph4	r0
	move4 r0,term2
	jsl	CompareOSStrings	      if term1 != term2 then
	tax
	beq	lb8
	ph4	#one		stack("1")
	jsl	DuplicateOSString
	sta	r0
	stx	r2
	jsr	PushOperand
	bra	lb9	      else
lb8	ph4	#zero		stack("0")
	jsl	DuplicateOSString
	sta	r0
	stx	r2
	jsr	PushOperand
lb9	move4 toke,r0	      pop the token pointer
!			      endif
lb10	anop		    endif
	free	term1	    free(term1)
	free	term2	    free(term2)
	brl	lb1	  endwhile
lb11	anop		endif
	lda	err	return err
	rts

oom	lda	#outOfMem	out of memory
	rts

err	ds	2
toke	ds	4	temp token pointer
term1	ds	4	term 1 pointer
term2	ds	4	term 2 pointer
	end

****************************************************************
*
*  Term - Evaluate a term
*
*  Inputs:
*	r0 - pointer to the first token in the term
*	LINE - input line
*
*  Outputs:
*	term on stack
*	A - error code
*
****************************************************************
*
Term	private
	debug Term
	using Common
	using CCommon

	stz	err	err = 0
	lda	r0
	ora	r2
	jeq	rts
	ph4	r0	if token == "(" then
	ph4	#lparen
	jsl	CompareOSStrings
	tax
	bne	lb2a
	free	r0	  free(r0)
	jsr	GetToken	  nexttoken
	sta	r0
	stx	r2
	ora	r2
	bne	lb0
oom	lda	#outOfMem
	rts

lb0	jsr	StringExpression	  err = expression
	sta	err
	ph4	r0	  if token = ")" then
	ph4	#rparen
	jsl	CompareOSStrings
	tax
	bne	lb1
	free	r0	    free(r0)
	jsr	GetToken	    nexttoken
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	bra	lb2	  else
lb1	puts	#''''')'''' expected',cr=t,errout=t
	dec	err	    err = $FFFF
lb2	anop		  endif
	bra	lb5	else if (token == "==") or
lb2a	ph4	r0
	ph4	#eq
	jsl	CompareOSStrings
	tax
	beq	lb3
	ph4	r0	  (token == "!=") then
	ph4	#ne
	jsl	CompareOSStrings
	tax
	bne	lb4
lb3	malloc #2	  stack("")
	sta	r4
	stx	r6
	ora	r6
	jeq	oom
	lda	#0
	sta	[r4]
	ph4	r0
	move4 r4,r0
	jsr	PushOperand
	pl4	r0
	bra	lb5	else
lb4	jsr	PushOperand	  stack(token)
	jsr	GetToken	  nexttoken
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
lb5	anop		endif
rts	lda	err	return err
	rts

err	ds	2
	end

****************************************************************
*
*  TopType - return top value on instruction stack
*
*  Inputs:
*	INS_PTR - instruction stack pointer
*	INS_DISP - disp in instruction stack
*
*  Outputs:
*	A - byte copied from stack
*
****************************************************************
*
TopType	start
	debug TopType
	using Common
	using RecCom

	ldy	ins_disp	if nothing on stack then
	bne	lb1
	lda	#endsy	  report endsy
	rts

lb1	move4 ins_ptr,r0	lock the stack
	ldy	ins_disp	read tos
	dey
	lda	[r0],Y
	and	#$00FF
	rts
	end
