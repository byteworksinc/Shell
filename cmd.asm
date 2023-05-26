	keep	obj/cmd
	mcopy cmd.macros
****************************************************************
*
*  Commands
*
*  This module contains the subroutines called to execute the
*  commands available at the shell level.
*
****************************************************************
	copy	sh.dp
	eject
****************************************************************
*
*  ShellCommand - Command interpreter entry point
*
*  Inputs:
*	LINE - line to interpret
*
****************************************************************
*
ShellCommand start
	debug ShellCommand
	using Common
	using RecCom
	using Command
;
;  Do initial processing
;
	stz	error	indicate no error yet
	stz	io_error	indicate no I/O redirection error
	stz	executeCmd	not an EXECUTE command (yet)
	jsr	LineLength	if line is zero length then quit
	tax
	jeq	lc8
	ldx	#0	if the line is all spaces, quit
mn2	jsr	NextChar
	cmp	#RETURN
	jeq	lc6
	cmp	#' '
	beq	mn2
;
;  Execute the line
;
lc1	jsr	DoRedirection	handle I/O redirection
	bcc	lc2
	lda	#$FFFF
	sta	error
	bra	lc6
lc2	lda	o_error
	sta	io_error
	jsr	FindCommand	if this is a command then
	bcc	lc4
	sta	r0	  save the command number
	cmp	#max_command+1	  if the command is good then
	bge	lc3
	asl	a	    call command handler
	tax
	jsr	(cTable,X)
	sta	error	    set {Status}
	bra	lc5	  else
lc3	puts	#'Bad command number: ',errout=t write bad command number
	put2	r0,cr=t,errout=t
err	lda	#$FFFF	    set exit flag
	sta	error
	bra	lc5	  endif
lc4	anop		else
	jsr	PushLine	  push command line pointer, disp
	bcc	err
	jsr	BRun	  execute the program
	sta	error
	jsr	PullLine	  pop command line pointer, disp
lc5	lda	io_error
	bne	lc5a
	lda	o_error
	sta	io_error
lc5a	jsr	PopRedirection	restore old I/O redirection

lc6	lda	io_error	if there was an I/O error then
	beq	lc7
	sta	error	  set the error code
lc7	lda	error	set the status flag
	jsr	SetStatus
lc8	lda	error	return the error code
	rts

mntr_entry entry	monitor entry vector
	phb
	pla
	pla
	jmp	Monitor
;
;  Bad command
;
badcom	entry
	pla		bad command in table exit
	brl	lc3
;
;  Local data
;
error	ds	2	error number/flag
io_error ds	2	IO error flag
	end

****************************************************************
*
*  COMMAND - Common Data Area for command table
*
****************************************************************
*
Command	privdata
;
;  Global variables for this module
;
catChar	ds	2	separator character for full path names
catCol	ds	2	column number for CATALOG
catFlags ds	4	minus flags field for CATALOG
catPathLength ds 2	length of the path name for CATALOG

max_command equ 49	number of commands available
;
;  ORCA Command Table
;
cTable	dc	a'SetLanguage'     0	languages
	dc	a'ASML'            1	ASML
	dc	a'ASMLG'           2	ASMLG
	dc	a'Assemble'        3	ASSEMBLE
	dc	a'Catalog'         4	CATALOG
	dc	a'Copy'            5	COPY
	dc	a'Create'          6	CREATE
	dc	a'Delete'          7	DELETE
	dc	a'Disable'         8	DISABLE
	dc	a'Edit'            9	EDIT
	dc	a'Enable'         10	ENABLE
	dc	a'Help'           11	HELP
	dc	a'Link'           12	LINK
	dc	a'Input'          13	INPUT
	dc	a'Prefix'         14	PREFIX
	dc	a'Quit'           15	QUIT
	dc	a'Rename'         16	RENAME
	dc	a'Show'           17	SHOW
	dc	a'Type'           18	TYPE
	dc	a'Exists'         19	EXISTS
	dc	a'Change'         20	CHANGE
	dc	a'FileType'       21	FILETYPE
	dc	a'For'            22	FOR
	dc	a'End'            23	END
	dc	a'Loop'           24	LOOP
	dc	a'Break'          25	BREAK
	dc	a'Continue'       26	CONTINUE
	dc	a'Exit'           27	EXIT
	dc	a'Set'            28	SET
	dc	a'Echo'           29	ECHO
	dc	a'If'             30	IF
	dc	a'Else'           31	ELSE
	dc	a'Compress'       32	COMPRESS
	dc	a'Switch'         33	SWITCH
	dc	a'Move'           34	MOVE
	dc	a'Commands'       35	COMMANDS
	dc	a'Export'         36	EXPORT
	dc	a'Unset'          37	UNSET
	dc	a'Execute'        38	EXECUTE
	dc	a'History'        39	HISTORY
	dc	a'Alias'          40	ALIAS
	dc	a'Unalias'        41	UNALIAS
	dc	a'Comment'        42	COMMENT
	dc	a'Home'           43	HOME
	dc	a'Erase'          44	ERASE
	dc	a'Init'           45	INIT
	dc	a'Touch'          46	TOUCH
	dc	a'Bye'            47     SHUTDOWN
	dc	a'Devices'        48	DEVICES
	dc	a'Newer'          49	NEWER
	end

****************************************************************
*
*  Alias - Set an alias
*
****************************************************************
*
Alias	private
	debug Alias
	using Common

	stz	error	no error so far
	stz	token	token = null
	stz	token+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	jcs	rts

	jsr	GetToken	if an alias name is not given then
	sta	r0
	sta	token
	stx	r2
	stx	token+2
	ora	r2
	bne	lb1
oom	lda	#outOfMem
	sta	error
	brl	rts
lb1	lda	[r0]
	bne	al1
	jsr	ListAlias	  list all the aliases
	brl	rts
;
;  Print a specific variable
;
al1	jsr	SkipBlanks	skip blanks in command line
	jsr	LineLength	if there is no string then
	tax
	jne	al5
	move4 token,r12	  read the alias value
	jsr	ReadAlias
	sta	r0
	stx	r2
	ora	r0
	beq	oom
	lda	[r0]
	bne	al2
	puts	#'No such alias',cr=t,errout=t
	dec	error
	bra	al3
al2	puts	#'Alias '	  print the alias and value
	ph4	token
	ph2	#0
	jsl	PrintOSName
	puts	#'  '
	ph4	r0
	ph2	#0
	jsl	PrintOSName
	putcr
al3	free	r0
	brl	rts
;
;  Set the alias
;
al5	jsr	SkipBlanks	get a string from the command line
	jsr	GetString
	sta	r16
	stx	r18
	ora	r18
	jeq	oom
	move4 token,r12	set the alias
	jsr	SetAlias
	php		dispose of the pointer
	free	r16
	plp
	bcc	rts

	puts	#'Could not set the alias',cr=t,errout=t
	dec	error
;
;  Return any error code
;
rts	free	token	free(token)
	lda	error	handle any error
	brl	CommandError
;
;  Local data
;
error	ds	2	error code
token	ds	4	pointer to a token
	end

****************************************************************
*
*  Assemble - Assemble
*
*  Assemble the source file.
*
****************************************************************
*
Assemble private
	debug Assemble
	using Common

	jsr	Operands	read the operand
	bcs	err
	lm	li_ops,#%00000001	set operations flag
	jsr	Compile	do it
	pha
	putcr
	pla
	rts

err	lda	#$FFFF	error return
	rts
	end

****************************************************************
*
*  ASMLG	 - Assemble, link and go
*
*  Assemble the source file.  If the max error level found is
*  less than or equal to the max error level allowed, link the
*  output file.  If the max error level is still less than or
*  equal to the max error level allowed, execute the program.
*
****************************************************************
*
ASMLG	private
	debug ASMLG
	using Common

	jsr	Operands	read the operand
	bcs	err
	lm	li_ops,#%00000111	set operations flag
	jsr	Compile	do it
	pha
	putcr
	pla
	rts

err	lda	#$FFFF	error return
	rts
	end

****************************************************************
*
*  ASML - Assemble and link
*
*  Assemble the source file.  If the max error level found is
*  less than or equal to the max error level allowed, link the
*  output file.
*
****************************************************************
*
ASML	private
	debug ASML
	using Common

	jsr	Operands	read the operand
	bcs	err
	lm	li_ops,#%00000011	set operations flag
	jsr	Compile	do it
	pha
	putcr
	pla
	rts

err	lda	#$FFFF	error return
	rts
	end

****************************************************************
*
*  Break - BREAK statement.
*
****************************************************************
*
Break	private
	debug Break
	using RecCom
	using Common

	jsr	RemoveCommand	remove the command
	jsr	EOLCheck	make sure there is no garbage
	bcc	bk1
err	lda	#$FFFF
	rts

bk1	jsr	DoContinue	skip to end of loop
	bcs	err
	jsr	PopStack	pop loop type
	cmp	#forsy	if forsy then
	bne	bk4

bk2	jsr	TopType	  while tos != forsy do
	cmp	#forsy
	beq	bk3
	jsr	PopWord	    free(PopWord)
	free	r0
	bra	bk2	  endwhile
bk3	jsr	PopStack	  pop forsy
bk4	lda	#0	endif
	rts
	end

****************************************************************
*
*  Bye - shut down the OS
*
****************************************************************
*
Bye     	private
	debug Bye
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	lb1
err	lda	#$FFFF
	rts

lb1	jsr	EOLCheck	check for garbage in line
	bcs	err

	jsr	ShutDown	shut down restartable applications
	DisposeAll User_ID	get rid of my memory
	php		disable interrupts
	sei
	lda	ProDOS	reset ProDOS hook
	sta	>pro_entry
	lda	ProDOS+2
	sta	>pro_entry+2
	lda	StackProDOS	reset ProDOS stack hook
	sta	>pro_stack_entry
	lda	StackProDOS+2
	sta	>pro_stack_entry+2
	plp		enable interrupts
	stz	dcDevNum	eject any disks
lb2	inc	dcDevNum
	OSD_Control dcRec
	bcc	lb2
	cmp	#$004E
	beq	lb2
	OS_ShutDown sdRec	shut down the OS

     	lda	#0
	rts

sdRec	dc	i'1'
	dc	i'$0000'

dcRec	dc	i'5'                     OS_DControl record
dcDevNum	ds	2
	dc	i'2'
	dc	i4'0'
	dc	i4'0'
	dc	i4'0'
	end

****************************************************************
*
*  Catalog
*
*  Catalog a directory.
*
****************************************************************
*
Catalog	private
	debug Catalog
	using Common
	using Command
DIR	equ	$F	directory file type
;
;  Set up for a directory scan
;
	stz	err	err = 0 (no error, yet)
	stz	catCol	starting in column 0
	stz	catPathLength	length of the path name being cataloged
	stz	didOne	no catalogs done, yet
	stz	edIn	edIn = nil
	stz	edIn+2
	stz	edOut	edOut = nil
	stz	edOut+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	malloc #buffSize	reserve a wildcard buffer
	sta	nxPath
	sta	r0
	stx	nxPath+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#buffSize	set the buffer length
	sta	[r0]
	jsr	RemoveCommand	remove the command
	flags ,(A,D,H,L,N,P,T)	get command line flags
	bcc	fl1
	lda	#-1
	sta	err
	brl	ex1
fl1	move4 r0,catFlags	save the flags
gd0	jsr	GetToken	read the first file name
	sta	edIn
	sta	r0
	stx	edIn+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]	if there is no name then
	bne	gd1
	lda	didOne	  if we have done a catalog then
	jne	ex1	    quit
!			  else
	free	edIn	    Free(edIn)
	malloc #5	    edIn = malloc(strlen("8:="));
	sta	edIn
	sta	r0
	stx	edIn+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#3	    strcpy(edIn, "8:=")
	sta	[r0]
	ldy	#2
	lda	#':8'
	sta	[r0],Y
	iny
	lda	#'=:'
	sta	[r0],Y

gd1	inc	didOne	starting a catalog
	stz	first	initialize first time in flag
	stz	directory	not a directory
	ph4	#edRec	expand devices and prefixes
	jsr	ExpandDevice
	sta	err
	tax
	jne	ex1
	free	edIn	Free(edIn)
	stz	edIn	edIn = nil
	stz	edIn+2

	move4 edOut,r0	replace all '?' on token with '='
	stz	r4	 and check for wildcard characters
	ldy	#2
	lda	[r0],Y
	tax
	ldy	#4
	short I,M
gd2	lda	[r0],Y
	cmp	#'?'
	bne	gd3
	lda	#'='
	sta	[r0],Y
gd3	cmp	#'='
	bne	gd4
	inc	r4
gd4	iny
	dex
	bne	gd2
	long	I,M
	lda	r4	if no wildcards
	jne	gd7
	ph2	edOut+2	  and file is a directory then
	lda	edOut
	inc	A
	inc	A
	pha
	jsr	GetFileType
	cmp	#-1
	bne	gd4a
	sta	err
	puts	#'Could not find the directory ',errout=t
	ph4	edOut
	ph2	#$C000
	jsl	PrintOSName
	putcr errout=t
	brl	ex1
gd4a	cmp	#DIR
	bne	gd7
	add4	r0,#2	  append := onto the name
	ph4	r0
	lda	[r0]
	tay
	iny
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	beq	gd5
	ph4	#colonEqual
	bra	gd6
gd5	ph4	#equal
gd6	ph2	#1
	jsl	AppendOSNames
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
	free	edOut	  Free(edOut)
	move4 r0,edOut	  edOut = r0
	inc	directory	  directory = true

gd7	lda	catFlags+2	if -d flag set then
	and	#^set_d
	beq	gd8
	lda	#$2000	  wcFlags = $2000 (recursive)
	sta	wcFlags
	bra	gd9	else
gd8	stz	wcFlags	  wcFlags = 0
gd9	anop		endif
	add4	edOut,#2,wcName	initialize for file find
	OSInit_Wildcard wcRec
	sta	err
	jcs	ex1
	lda	catFlags+2	if no -p flag used then
	and	#^set_p
	bne	pt3
	add4	edOut,#2,r0	  find the length of the base path name
	lda	[r0]
	inc	A
	tay
	short M
	lda	#':'
pt1	cmp	[r0],Y
	beq	pt2
	dey
	bne	pt1
	iny
pt2	long	M
	dey
	sty	catPathLength
pt3	anop		endif

	putcr		skip a line
	lda	catFlags+2	if no -h given then
	and	#^set_h
	bne	gd10
	ph4	wcName	  write the full path name
	ph2	#0
	jsl	PrintOSName
	putcr		  skip two lines
	putcr
	puts	head,cr=t	  write the header
	putcr
gd10	anop		endif

	lda	#':'                     set the separator character
	sta	catChar
	jsl	SeparatorSet
	bcc	gd11
	sta	catChar
gd11	anop
;
;  List files until no more entries
;
lf1	OSNext_Wildcard nxRec	get the next entry
	move4 nxPath,r0	if there are no more entries then
	ldy	#2
	lda	[r0],Y
	bne	lf2
	lda	first	  if no entries were found then
	bne	lf5
	puts	#'No entries found',cr=t	    print the error message
	bra	lf5	  exit the loop
lf2	lda	nxAccess	if the invisible bit is set then
	and	#$0004
	beq	lf3
	lda	catFlags+2	  if the -a flag was not used then
	and	#^set_a
	beq	lf4	    skip printing this file
lf3	inc	first	update first time in flag
	jsr	WriteFileInfo	list the file catalog entry
lf4	jsr	Pause	Pause
	bcc	lf1	if exit not requested then loop

lf5	putcr		put one blank line after the files
;
;  End of loop processing
;
	lda	catFlags+2	if no -h given then
	and	#^set_h
	jne	gd0
	add4	edOut,#2,r0	  create a volume name
	ldy	#3
	short M
	lda	#':'
el1	cmp	[r0],Y
	beq	el2
	iny
	bra	el1
el2	long	M
	tya
	dec	A
	sta	[r0]
	move4 r0,gdDevName	  get volume info
	OSGet_Dev_Number gdRec
	sta	err
	jcs	ex1
	lda	gdDevNum
	sta	diDivNum
	OSD_Info diRec
	sta	err
	jcs	ex1
	OSVolume vlRec
	sta	err
	jcs	ex1
	puts	#'Blocks Free: '	  print the volume info
	put4	vlFree,#6
	puts	#'      Blocks used: '
	sub4	vlBlocks,vlFree,r0
	put4	r0,#6
	puts	#'      Total Blocks: '
	put4	vlBlocks,#6,cr=t
	brl	gd0	  get the next command line argument
;
;  Out of memory error
;
oom	lda	#outOfMem
	sta	err
;
;  Exit processing
;
ex1	free	edIn	Free(edIn)
	free	edOut	Free(edOut)
	free	nxPath	Free(nxPath)
	lda	err	handle any error
	brl	CommandError
;
;  Local data
;
didOne	ds	2	have we done a catalog, yet?
directory ds	2	directory flag
err	ds	2	error number
first	ds	2	first time in flag

colonEqual dc	i'2',c':='	used to form wildcard paths
equal	dc	i'1',c'='

head	dc	2i1'77'	header message
	dc	c'Name            Type  Blocks  Modified  '
	dc	c'      Created         Access  Subtype'

edRec	dc	i'2'	ExpandDevice record
edIn	ds	4
edOut	ds	4

gdRec	dc	i'2'	OSGet_Dev_Number record
gdDevName ds	4
gdDevNum ds	2

diRec	dc	i'2'	OSD_Info record
diDivNum ds	2
	dc	a4'volumeName'

vlRec	dc	i'4'	OSVolume record
	dc	a4'volumeName+2'
	dc	a4'volumeName'
vlBlocks ds	4
vlFree	ds	4

wcRec	dc	i'2'	OSInit_Wildcard record
wcName	ds	4
wcFlags	ds	2
	end

****************************************************************
*
*  Change - Change the language stamp of a SRC file.
*
****************************************************************
*
Change	private
	debug Change
	using Common
minus_flags equ r0	minus flags mask

TXT	equ	4	file types for text and src
SRC	equ	$B0

	stz	error	no error so far
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	RemoveCommand	remove the command
	flags	,(P)	get command line flags
	bcc	in1
err	dec	error	return error code of -1
	brl	rts

in1	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress
;
;  Read in the file name
;
	sec		get the file name (must exist)
	lda	#0
	ldx	#1
	jsr	GetFileName
	sta	error
	jcs	rts
	stz	error
	sta	wcard
;
;  Get the file type
;
	jsr	SkipBlanks	skip leading blanks
	jsr	LineLength	see if line is empty
	tax
	bne	ch2
	puts	#'Language: '
	jsr	GetLine	ask for a language if none specified
	jsr	SkipBlanks
	jsr	LineLength	if line is blank again return
	tax
	jeq	rts

ch2	jsr	FindCommand	identify language
	jcc	ch2a
	cmp	#0
	beq	ch3
ch2a	puts	#'Language not available',cr=t,errout=t
	brl	err

ch3	sty	type	save the language type
	jsr	RemoveCommand	remove the language type
	jsr	EOLCheck	check for garbage at end of command
	jcs	err
	lda	wcard	if wildcards used then
	beq	ch4
	lda	noProgress                 if not noProgress then
	bne	ch4
	puts	#'Files changed:',cr=t	    write the header message
;
;  Loop thru each file and change the language
;
ch4	lda	wcard	write space for file names list
	beq	ch5
	puts	#'  '
ch5	lda	nxFileType	make sure file is text or src
	cmp	#TXT
	beq	ch6
	cmp	#SRC
	beq	ch6
	ph4	nxPath	file type error - print the message
	ph2	#$C000
	jsl	PrintOSName
	puts	#' must be TXT or SRC',cr=t,errout=t
	brl	ch9

ch6	lda	#TXT	split on SRC or TXT
	ldx	type
	beq	ch7
	lda	#SRC
ch7	sta	nxFileType	set the file type
	stx	nxAuxType	set the language
	move	#0,nxModDateTime,#8	reset mod date/time
	lda	wcard	if wcard then
	beq	ch8
	lda	noProgress                 if not noProgress then
	bne	ch8
	ph4	nxPath	    print the path name
	ph2	#$8000
	jsl	PrintOSName
	putcr
ch8	inc	nxPath	modify flags
	inc	nxPath
	OSSet_File_Info nxRec
	dec	nxPath
	dec	nxPath
	bcc	ch9
	dec	error
	bra	rts
ch9	jsr	Pause
	bcs	rts
	OSNext_Wildcard nxRec	get next file name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	ch4

rts	free	nxPath	free(nxPath)
	lda	error	handle any error
	brl	CommandError
;
;  Local data
;
error	ds	2	error number
noProgress ds	2	noProgress flag set?
wcard	ds	2	wildcards in use?
type	ds	2	file type
	end

****************************************************************
*
*  Commands - Reread the system command table
*
****************************************************************
*
Commands private
	debug Commands
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcs	err
	sec		get the file name
	lda	#$4000
	ldx	#1
	jsr	GetFileName
	bcs	rts
	jsr	ShutDown	shut down restartable applications
	free	cthandle	get rid of old table
	add4	nxPath,#2,r0	read the command table
	jsr	ReadSYSCMND
	free	nxPath	free(nxPath)
	lda	#0	return with no error
rts	rts

err	lda	#$FFFF
	rts
	end

****************************************************************
*
*  Comment
*
*  Comment statement.
*
****************************************************************
*
Comment	private

	lda	#0	comment command
	rts
	end

****************************************************************
*
*  Compress
*
*  Alphabetize/compress directories
*
****************************************************************
*
Compress private
	debug Compress
	using Common
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24

	stz	cflag	initialize the operation flag
	stz	aflag
	stz	error	no error so far
	stz	buff	buff = nil
	stz	buff+2
	stz	edOut	edOut = nil
	stz	edOut+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	cm1
	lda	#$FFFF
	rts
;
;  Read the flags and set the flag mask
;
cm1	jsr	GetToken	get compress parameters
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r0
	bne	cm1a
oom	lda	#outOfMem
	sta	error
	brl	rts
cm1a	lda	[r0]	if there were no parameters then
	bne	cm2
	free	r0	  free the token buffer
	puts	#'Parameters(C A): '	  ask for some
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r0
	beq	oom
	lda	[r0]
	jeq	rts

cm2	cmp	#1	make sure it is a single character
	beq	cm3
err2	puts	#'Invalid parameter',cr=t,errout=t
	dec	error
	brl	rts

cm3	jsr	IDPrm	identify the parameter; must be legal
	bcs	err2
	free	buff	free(buff)
	jsr	GetToken	get a second parameter or file name
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]
	beq	cm4
	cmp	#1	possible second parameter
	bne	cm4
	jsr	idprm	identify second parameter
	bcs	cm4
	free	r0	free(r0)
	jsr	GetToken	if a parameter then get file name
	sta	buff
	stx	buff+2
	ora	buff+2
	jeq	oom

cm4	move4 buff,edIn	expand devices and prefixes
	move4 edIn,r0
	lda	[r0]
	bne	cm4a
	lla	edIn,prefix0
cm4a	ph4	#edRec
	jsr	ExpandDevice
	sta	error
	bcs	rts
	add4	edOut,#2,r0	read in the directory
	jsl	ReadDirectory
	sta	error
	jcs	rts
	lda	aflag	see if we do a compress A
	beq	cm5
	jsl	Alphabetize
cm5	lda	cflag	see if we do a compress C
	beq	cm6
	jsl	Crunch
cm6	jsl	WriteDirectory	write back the directory
	bcc	cm6a
	dec	error
	bra	rts
cm6a	free	buff	free the token buffer
	jsr	GetToken	get the next name
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	jne	cm4

rts	free	buff	free the token buffer
	free	edOut	free the expanded path
	lda	error	return the error code
	jsr	CommandError
cm7	rts
;
;  Identify the paramter
;
IDPrm	ldy	#2	check the flag
	lda	[r0],Y
	and	#$00FF
	jsl	ToUpper
	cmp	#'C'
	bne	id2
	lda	cflag	check for C already defined
	bne	id4
	inc	cflag
	bra	id3
id2	cmp	#'A'
	bne	id4
	lda	aflag
	bne	id4
	inc	aflag
id3	clc		good identification
	rts

id4	sec		not a good identification
	rts
;
;  Local data
;
edRec	dc	i'2'	ExpandDevice record
edIn	ds	4
edOut	ds	4

aflag	ds	2	compress alphabetize flag
buff	ds	4	token buffer pointer
cflag	ds	2	compress crunch flag
error	ds	2	error number
	end

****************************************************************
*
*  Continue - CONTINUE statement
*
****************************************************************
*
Continue private
	debug Continue
	using Common

	jsr	RemoveCommand	remove the command
	jsr	EOLCheck	make sure there is no garbage
	bcc	lb1
err	lda	#$FFFF
	rts

lb1	jsr	DoContinue	skip to end of loop
	bcs	err
	jsr	LastCLine	back up to END
	bcs	err
	lda	#0
	rts
	end

****************************************************************
*
*  Copy - Copy one or more files.
*
****************************************************************
*
Copy	private
	debug Copy
	using Common
minus_flags equ r0	minus flags mask

	jsr	RemoveCommand	remove the command
	flags ,(C,D,F,R,P)	get command line flags
	bcc	lb0
	lda	#$FFFF
	rts

lb0	lda	minus_flags+2	if -D flag is set then
	and	#^set_d
	beq	lb1
	brl	DiskCopy	  do disk copy
lb1	anop		else
	lda	#1	  do a file copy
	brl	FileCopy
	end

****************************************************************
*
*  Create - Create a new directory.
*
****************************************************************
*
Create	private
	debug Create
	using Common

	stz	edIn	edIn = nil
	stz	edIn+2
	stz	crPathname	crPathname = nil
	stz	crPathname+2
	stz	error	no error yet
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	lb1
	dec	error
	brl	rts
lb1	jsr	GetToken	read path name to create
	sta	edIn
	sta	r0
	stx	edIn+2
	stx	r2
	ora	r0
	bne	lb2
oom	lda	#outOfMem	check for out of memory error
	sta	error
	brl	rts
lb2	lda	[r0]	branch if we have a path name
	bne	cr2
	puts	#'Path name: '	none on command line - prompt for one
	jsr	GetLine
;
;  Loop over command names
;
cr1	free	edIn	dispose of the old name buffer
	jsr	GetToken	while there is a file name
	sta	edIn
	sta	r0
	stx	edIn+2
	stx	r2
	ora	r0
	beq	oom
	lda	[r0]
	beq	rts
cr2	ph4	#edRec	  expand devices and prefixes
	jsr	ExpandDevice
	sta	error
	tax
	jne	rts
	free	crPathname	  free any old path buffer
	add4	edOut,#2,crPathname	  create the directory
	OSCreate crRec
	sta	error
	bcc	cr1	next file

rts	free	crPathname	Free(crPathname)
	free	edIn	Free(edIn)
	lda	error	handle any error
	brl	CommandError
;
;  Local data
;
error	ds	2	error number

crRec	dc	i'5'	OSCreate record
crPathname ds	4
crAccess dc	i'$C3'
crFileType dc	i'$0F'
crAuxType dc	i4'0'
crStorageType dc i'$0D'

edRec	dc	i'2'	ExpandDevice record
edIn	ds	4
edOut	ds	4
	end

****************************************************************
*
*  Delete - Delete a file
*
****************************************************************
*
Delete	private
	debug Delete
	using Common
minus_flags equ r0	minus flags mask

	stz	error	no error, yet
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	RemoveCommand	remove the command
	flags ,(C,P,W)	get command line flags
	bcc	in1
	dec	error
	brl	rts

in1	lda	minus_flags+2	set check before delete flag
	and	#^set_c
	eor	#^set_c
	sta	check
	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress
	lda	minus_flags	noWarning = <-w flag set>
	and	#set_w
	sta	noWarning
;
;  Loop over multiple file names
;
	OSBegin_Session bsRec	start a session
dl	sec		get the file name
	lda	#0
	ldy	noWarning
	bne	dl0
	lda	#2
dl0	ldy	check
	beq	dl0a
	ora	#$0004
dl0a	tax
	lda	#$3000
	jsr	GetFileName
	jcs	dl5
	sta	wcard
	stz	error

	lda	wcard	check wildcard flag
	beq	dl2
	cmp	#2	see if '=' only
	bne	dl1
	lda	check	if check then
	beq	dl1
	puts	#'Are you sure? '	  make sure he wants to do this
	jsr	YesOrNo
	cmp	#'N'
	jeq	dl5
	cmp	#'Q'
	jeq	rts
dl1	lda	noProgress               if not noProgress then
	jne	dl4a	
	puts	#'Files deleted:',cr=t     print header
	brl	dl4a	(go check for at least one match)

dl2	add4	nxPath,#2,dsPathname	dsPathname = nxPath+2
	lda	wcard	if wcard then
	beq	dl3
	lda	noProgress                 if not noProgress then
	bne	dl3
	puts	#'  '	    write a file name
	ph4	dsPathname
	ph2	#0
	jsl	PrintOSName
	putcr

dl3	jsr	Spin	  spin the spinner
	OSDestroy dsRec	  delete a file
	bcc	dl4	  if error then
	sta	error	    save the error code
	brl	rts	    quit
dl4	jsr	Pause
	bcs	rts
	OSNext_Wildcard nxRec	get next file name
dl4a	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	dl2
dl5	free	nxPath	free(nxPath)
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	SkipBlanks	get next file name on line
	jsr	LineLength
	tax
	jne	dl

rts	OSEnd_Session bsRec	stop the session
	free	nxPath	free(nxPath)
	lda	error	handle any error
	brl	CommandError
;
;  Local data
;
check	ds	2	check before deleting?
noProgress ds	2	noProgress flag set?
noWarning ds	2	noWarning flag set?
wcard	ds	2	wild card flag
error	ds	2	error flag

bsRec	ds	2	session record

dsRec	dc	i'1'	OSDestroy record
dsPathname ds	4
	end

****************************************************************
*
*  Devices - Show a device list
*
****************************************************************
*
Devices	private
	debug Devices
	using Common
;...............................................................
;
; Check for flags & garbage on the command line
;
;...............................................................

	jsr	RemoveCommand	remove the command
	flags ,(T,D,S,U,V,I,F,M,B,L,N) get command line flags
	bcs	err
	jsr	EOLCheck	check for garbage at end of command
	bcc	nf1
err	lda	#$FFFF
	rts
;...............................................................
;
; If no flags were specified, list the devices in table form
;
;...............................................................
;
;  Write the header
;
nf1	lda	r0	branch if flags were specified
	ora	r2
	jne	fl1
	puts	#'Device Name'
	prbl	#20
	puts	#'Device Type     Volume Name',cr=t
	puts	#'------------------------------ --------------- -------------------------------',cr=t
;
;  List the devices
;
	lda	#1	init the device number
	sta	dev_num
un1	OSD_Info block	get device name
	jcs	un6	done if error
	ph4	#name+2	write the device name
	ph2	#0
	jsl	PrintOSName
	ldx	#31	padd with spaces
	txy
	lda	name+2
	jsr	Padd

	jsr	PrintDeviceType	print the device type
	ldx	#16
	ldy	#47
	lda	(r4)
	xba
	and	#$00FF
	jsr	Padd

	OSVolume vlBlock	get the volume name (if any)
	bcs	un4
	ph4	#vlName+2	print the volume name
	ph2	#0
	jsl	PrintOSName
	brl	un5
un4	cmp	#$2F	print the error number
	beq	er1
	cmp	#$58
	beq	er2
	sta	vlName+2
	puts	#'Error number '
	put2	vlName+2
	bra	un5
er1	puts	#'<Offline>'
	bra	un5
er2	puts	#'<Character Device>'
un5	putcr
	inc	dev_num	next device
	jsr	Pause
	jcc	un1

un6	lda	#0	return with no error
	rts
;...............................................................
;
; If flags were specified, list the devices in open form
;
;...............................................................
fl1	lda	#1	init the device number
	sta	dev_num
	move4 r0,minus_flags	save the flags

fl2	putcr		place a blank between devices
	OSD_Info block	get device name
	jcs	fl18	done if error

	puts	#'Name:'	write the device name
	prbl	#8
	ph4	#name+2
	ph2	#0
	jsl	PrintOSName
	putcr

	aif	apw,.devices_a
	lda	minus_flags+2	print the device number
	and	#^(set_l+set_n)
	beq	fl2a
	puts	#'Number:'
	prbl	#6
	put2	dev_Num,cr=t
fl2a	anop

.devices_a
	lda	minus_flags+2	print the device type
	and	#^set_l
	bne	fl3
	lda	minus_flags
	and	#set_t
	beq	fl4
fl3	puts	#'Type:'
	prbl	#8
	jsr	PrintDeviceType
	putcr

fl4	lda	minus_flags+2	print the version
	and	#^(set_l+set_d)
	jeq	fl6
	puts	#'Version:'
	prbl	#5
	lda	dev_version
	xba
	lsr	A
	lsr	A
	lsr	A
	lsr	A
	jsr	PutChar
	putc	#'.'
	lda	dev_version
	xba
	jsr	PutChar
	lda	dev_version
	lsr	A
	lsr	A
	lsr	A
	lsr	A
	jsr	PutChar
	lda	dev_version
	and	#$000F
	beq	fl5
	jsr	PutChar
fl5	putcr

fl6	lda	minus_flags+2	print the slot
	and	#^set_l
	bne	fl7
	lda	minus_flags
	and	#set_s
	beq	fl8
fl7	puts	#'Slot:'
	prbl	#8
	put2	dev_slot,cr=t

fl8	lda	minus_flags+2	print the unit
	and	#^set_l
	bne	fl9
	lda	minus_flags
	and	#set_u
	beq	fl10
fl9	puts	#'Unit:'
	prbl	#8
	put2	dev_unit,cr=t

fl10	OSVolume vlBlock	get the volume name (if any)
	jcs	fl17

	lda	minus_flags+2	print the volume name
	and	#^set_l
	bne	fl11
	lda	minus_flags
	and	#set_v
	beq	fl12
fl11	puts	#'Volume Name: '	print the volume name
	ph4	#vlName+2
	ph2	#0
	jsl	PrintOSName
	putcr

fl12	lda	minus_flags+2	print the file system
	and	#^(set_l+set_i)
	jeq	fl14
	puts	#'File System: '
	lda	vlFileSys
	cmp	#$000D
	blt	fl13
	lda	#0
fl13	asl	A
	tax
	lda	fsNames,X
	sta	r4
	lda	#^fsNames
	sta	r6
	puts	{r4}
	puts	#'  ('
	put2	vlFileSys
	putc	#')',cr=t

fl14	lda	minus_flags+2	print the free blocks
	and	#^(set_l+set_f)
	beq	fl15
	puts	#'Free:'
	prbl	#8
	put4	vlFree,cr=t

fl15	lda	minus_flags+2	print the total blocks
	and	#^(set_l+set_m)
	beq	fl16
	puts	#'Size:'
	prbl	#8
	put4	vlBlocks,cr=t

fl16	lda	minus_flags+2	print the block size
	and	#^(set_l+set_b)
	beq	fl17
	puts	#'Block size:  '
	put2	vlBlockSize,cr=t

fl17	inc	dev_num	next device
	jsr	Pause
	jcc	fl2

fl18	lda	#0	return with no error
	rts
;...............................................................
;
; Padd - padd a field with spaces
;
;...............................................................
Padd	stx	r4	save the field width
	sty	r6	save the tab column
	cmp	r4	if the name exceeds 30 cols then
	blt	pa1
	putcr		  move to the next line
	prbl	r6
	rts		else
pa1	sec
	sta	r6	  padd the name
	lda	r4
	sbc	r6
	sta	r8
	prbl	r8
	rts
;...............................................................
;
; PutChar - write a single hex character
;
;...............................................................
PutChar	and	#$000F
	ora	#'0'
	cmp	#'9'+1
	blt	pc1
	adc	#6
pc1	sta	temp
	putc	temp
	rts

temp	ds	2
;...............................................................
;
; PrintDeviceType - print the device type
;
;...............................................................
;
PrintDeviceType anop
	lda	dev_id	if the device type is unknown then
	cmp	#$0020
	blt	pd1
	lda	#$0020	  use number 20
pd1	asl	A
	tax
	lda	msgTable,X
	sta	r4
	lda	#^msgTable
	sta	r6
	puts	{r4}
	rts

msgTable dc	a'm0-1,m1-1,m2-1,m3-1,m4-1,m5-1,m6-1,m7-1,m8-1,m9-1'
	dc	a'm10-1,m11-1,m12-1,m13-1,m14-1,m15-1,m16-1,m17-1,m18-1,m19-1'
	dc	a'm20-1,m21-1,m22-1,m23-1,m24-1,m25-1,m26-1,m27-1,m28-1,m29-1'
	dc	a'm30-1,m31-1,m32-1'
m0	dw	'Apple 5.25'
m1	dw	'Profile 5 MB'
m2	dw	'Profile 10 MB'
m3	dw	'Apple 3.5'
m4	dw	'SCSI (generic)'
m5	dw	'SCSI hard disk'
m6	dw	'SCSI tape drive'
m7	dw	'SCSI CD ROM'
m8	dw	'SCSI printer'
m9	dw	'Serial modem'
m10	dw	'Console driver'
m11	dw	'Serial printer'
m12	dw	'Serial LaserWriter'
m13	dw	'AppleTalk LaserWriter'
m14	dw	'RAM disk'
m15	dw	'ROM disk'
m16	dw	'File server'
m17	dw	'Reserved'
m18	dw	'AppleDesktop bus'
m19	dw	'Hard disk (generic)'
m20	dw	'Floppy disk (generic)'
m21	dw	'Tape drive (generic)'
m22	dw	'Character device (generic)'
m23	dw	'MFM-encoded disk drive'
m24	dw	'AppleTalk network'
m25	dw	'Sequential device'
m26	dw	'SCSI scanner'
m27	dw	'Other scanner'
m28	dw	'LaserWriter SC'
m29	dw	'AppleTalk main driver'
m30	dw	'AppleTalk file server'
m31	dw	'AppleTalk RPM driver'
m32	dw	'Unknown device'
;...............................................................
;
; Local Data
;
;...............................................................
block	anop		control block for OSD_Info call
	dc	i'8'
dev_Num	ds	2
	dc	a4'name'
	ds	2
dev_size ds	4	number of blocks
dev_slot ds	2	slot number
dev_unit ds	2	unit number
dev_version ds 2	version number
dev_id	ds	2	device type ID

vlBlock	anop		control block for OSVolume call
	dc	i'6'
	dc	a4'name+2'
	dc	a4'vlName'
vlBlocks ds	4	# of blocks
vlFree	ds	4	# of free blocks
vlFileSys ds	2	file system
vlBlockSize ds 2	size of a block

vlName	dc	i'35,0',31c' '
name	dc	i'35,0',31c' '

minus_flags ds 4	storage for the command line flags
;
;  File System Names
;
fsNames	dc	a'n0-1,n1-1,n2-1,n3-1,n4-1,n5-1,n6-1,n7-1,n8-1,n9-1'
	dc	a'n10-1,n11-1,n12'

n9	anop
n0	dw	'reserved'
n1	dw	'ProDOS'
n2	dw	'DOS 3.3'
n3	dw	'DOS 3.2'
n4	dw	'Apple II Pascal'
n5	dw	'Macintosh (MFS)'
n6	dw	'Macintosh (HFS)'
n7	dw	'LISA'
n8	dw	'Apple CP/M'
n10	dw	'MS/DOS'
n11	dw	'High Sierra'
n12	dw	'ISO 9660'
	end

****************************************************************
*
*  Diasable - disable priviledges.
*
****************************************************************
*
Disable	private
	debug Disable
	using Common

	lda	#1	disable flag
	brl	Enable1
	end

****************************************************************
*
*  Echo - ECHO statement.
*
****************************************************************
*
Echo	private
	debug Echo
	using Common
minus_flags equ r0	minus flags mask

	jsr	RemoveCommand	remove the command
	aif	APW,.echo1
	flags ,(N,T)	get command line flags
	jcs	err
	ago	.echo2
.echo1
	flags ,(T)	get command line flags
	jcs	err
.echo2

	lda	#1	doReturn = true
	sta	doReturn
	lda	minus_flags+2	if the -n flag is set then
	and	#^set_n	  doReturn = false
	beq	fl1
	stz	doReturn
fl1	lda	#1	eTabs = true
	sta	eTabs
	lda	minus_flags	if the -t flag is set then
	and	#set_t
	beq	fl2
	stz	eTabs	  eTabs = false
fl2	anop

	stz	r0	get tab line for language 0
	jsr	GetTabLine
	jsr	SkipBlanks	skip leading blanks
	jsr	GetString	read the string
	bcs	err	branch if there was an error
	sta	r0	write the characters
	stx	r2
	lda	[r0]
	beq	ec5
	sta	r4
	ldy	#2
	ldx	#0
ec1	short M
	lda	[r0],Y
	cmp	#TAB
	bne	ec3
	lda	etabs
	bne	ec2a
	lda	#TAB
	bra	ec3
ec2a	cpx	#256
	blt	ec2
	lda	#' '
	bra	ec3
ec2	lda	#' '
	jsl	~stdout
	inx
	lda	tabs,X
	beq	ec2
	iny
	bra	ec4
ec3	jsl	~stdout
	inx
	iny
ec4	long	M
	dec	r4
	bne	ec1

ec5	free	r0	dispose of the buffer

	aif	APW,.echo2
	lda	doReturn
	beq	lb3
.echo2
	putcr
lb3	lda	#0	normal exit
	rts

err	lda	#$FFFF	error exit
	rts

doReturn ds	2	do a CR at the end of the echo?
eTabs	ds	2	expand tabs?
	end

****************************************************************
*
*  Edit - Edit a SRC or TXT file.
*
****************************************************************
*
Edit	start
	debug Edit
	using Common
file_not_found equ  $46

	jsr	RemoveCommand	remove the command
	flags		get command line flags

	stz	list	list = nil
	stz	list+2
	stz	buff	buff = nil
	stz	buff+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	free	li_saddr	free any old name
	stz	li_saddr	li_saddr = nil
	stz	li_saddr+2
tp1	malloc #buffSize	get a path name buffer
	sta	nxPath
	sta	r0
	stx	nxPath+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#buffSize
	sta	[r0]
	jsr	GetToken
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]	if there is no token then
	bne	in1
	free	r0	  free(buff)
	lda	list	  if list <> nil then
	ora	list+2
	jne	aa3	    go do the edit
	puts	#'File to edit: '	  prompt for a file name
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]
	jeq	rts
in1	OSInit_Wildcard iwRec	expand the path name
	bcs	in2
	OSNext_Wildcard nxRec
	bcs	in2
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	bne	in3
in2	move4 buff,giPathname	can't expand - use the token
	bra	in4
in3	add4	nxPath,#2,giPathname	get info about the file
in4	OSGet_File_Info giRec
	bcc	ed2
	cmp	#file_not_found	file not found is the only error
	beq	ed3	  allowed
	brl	perr
ed2	lda	giFileType	no error make sure file is TXT or SRC
	cmp	#TXT
	beq	ed3
	cmp	#SRC
	jne	err
ed3	ph4	giPathname	set file name
	jsl	DuplicateOSString
	sta	r0
	stx	r2
	lda	list	if list = nil then
	ora	list+2
	bne	aa1
	move4	r0,list	  list = r0
	bra	aa2	else
aa1	ph4	list	  list = concat(list^, blank, r0^)
	ph4	#blank	  {free r0, list in the process}
	ph2	#0
	jsl	AppendOSNames
	sta	r4
	stx	r6
	ora	r6
	beq	oom
	free	list
	ph4	r4
	ph4	r0
	ph2	#0
	jsl	AppendOSNames
	sta	list
	stx	list+2
	free	r0
	free	r4
	lda	list
	ora	list+2
	beq	oom
aa2	anop		endif
	brl	tp1	get the next name

aa3	move4	list,li_saddr	set the name list
	free	nxPath	free the wildcard buffer
	free	buff	free the token buffer
	free	li_paddr	free any old message
	stz	li_paddr	create a null message
	stz	li_paddr+2
	stz	li_org	set entry point to 0
	stz	li_org+2
	jsl	edit_vector

rts	rts
;
;  Error handlers
;
oom	lda	#outOfMem
	bra	perr

none	puts	#'No matching files found.',cr=t,errout=t
	bra	err1

err	puts	#'File must be SRC or TXT.',cr=t,errout=t
	bra	err1

perr	jsl	SysError	ProDOS error
err1	lda	#$FFFF
	free	nxOptionList	free(nxOptionList)
	free	nxPath	free the wildcard buffer
	free	buff	free the token buffer
	free	list	free the name list
	rts
;
;  Standard editor entry vector
;
edit_entry entry
	lla	r0,editor	call the editor
	lla	r4,0
	jsr	Call
	rtl
;
;  Local data
;
blank	dc	i'1',i1'10'	linefeeds for concatonations
list	ds	4	ptr to list of names

iwRec	dc	i'2'	OSInitWildcard record
buff	ds	4	token buffer
	dc	i'0'

giRec	dc	i'3'	OSGet_File_Info record
giPathName ds	4
	ds	2
giFileType ds	2
	end

****************************************************************
*
*  Else - ELSE statement.
*
****************************************************************
*
Else	private
	debug Else
	using Common

	jsr	RemoveCommand	remove the command
	jsr	ScanToEnd
	bcs	err
	jsr	PopStack
	lda	#0
	rts

err	lda	#$FFFF	error
	rts
	end

****************************************************************
*
*  Enable - Enable priveleges.
*
*  Notes:
*	Entered at Enable1 with A=1 for disable.
*
****************************************************************
*
Enable	private
	debug Enable
	using Common
minus_flags equ r0	minus flags mask

	lda	#0	clear disable
Enable1	entry
	sta	disable
	stz	error	no error so far
	stz	buff	buff = nil
	stz	buff+2
	stz	sessionStarted	no session started
	lda	#1	this is the first file
	sta	firstFile
	jsr	RemoveCommand	remove the command
	flags	,(P)	get command line flags
	bcc	en1
err	dec	error
	brl	rts
;
;  Read the flags and set the flag mask
;
en1	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress
	stz	flags_chosen	clear flags
	jsr	GetToken	get flags
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	lda	[r0]
	bne	en2
	puts	#'Flags to change (DNBWRI): '
	free	r0
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	lda	[r0]
	jeq	rts
en2	tay		for each character do
	iny
	short M
en3	lda	[r0],Y
	jsl	ToUpper
	ldx	#l:chars-1	  if the char is not a flag then
en4	cmp	chars,X
	beq	en5
	dbpl	X,en4
	long	M	    error exit
	puts	#'Invalid flag',cr=t,errout=t
	brl	err
	longa off

en5	lda	flags_chosen	  else set flag mask
	ora	flagList,X
	sta	flags_chosen
	dey		next character
	cpy	#1
	bne	en3
	long	M
;
;  Loop over files, changing masks
;
	OSBegin_Session bsRec	start a session
	inc	sessionStarted	note that we started one

lp1	lda	firstFile	get the file name
	ror	A
	lda	#0
	ldx	#1
	jsr	GetFileName
	sta	wcard	save the wildcard flag
	stz	firstFile	no longer on the first file
	bcc	lp2
	dec	error
	brl	rts
lp2	lda	wcard	if wildcards are used then
	beq	lp4
	lda	noProgress                 if not noProgress then
	bne	lp4
	puts	#'Files changed:',cr=t       print the header

lp3	lda	wcard	if wildcards are in use then
	beq	lp4
	lda	noProgress                 if not noProgress then
	bne	lp4
	puts	#'  '	    print the name of the file
	ph4	nxPath
	ph2	#$8000
	jsl	PrintOSName
	putcr

lp4	lda	disable	modify access field
	bne	lp5
	lda	nxAccess
	ora	flags_chosen
	bra	lp6
lp5	lda	flags_chosen
	eor	#$00FF
	and	nxAccess
lp6	sta	nxAccess
	move	#0,nxModDateTime,#8	clear the mod date
	inc	nxPath	modify flags
	inc	nxPath
	OSSet_File_Info nxRec
	dec	nxPath
	dec	nxPath
	sta	error
	bcs	rts
	lda	disable	if disable then
	beq	lp6a
	lda	flags_chosen	  if B flag selected then
	and	flagB
	and	#$00FF
	beq	lp6a
	add4	nxPath,#2,cbPath	    clear the backup bit
	OSClear_Backup cbRec
lp6a	jsr	Pause
	bcs	rts
lp7	OSNext_Wildcard nxRec	get next file name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	lp3
	brl	lp1
;
;  Clean up and return
;
rts	lda	sessionStarted	if sessionStarted then
	beq	rts1
	OSEnd_Session bsRec	  stop the session
rts1	free	buff	free(buff) {buff is the token buffer}
	lda	error	handle any error
	brl	CommandError
;
;  Local variables
;
buff	ds	4	token buffer pointer
chars	dc	c'DNBIWR'	legal flags
disable	ds	2	disable?
error	ds	2	error number
firstFile ds	2	is this the first file?
flagList dc	b'10000000'	flag bits
	dc	b'01000000'
flagB	dc	b'00100000'
	dc	b'00000100'
	dc	b'00000010'
	dc	b'00000001'
flags_chosen ds 2	flags to select or deselect
sessionStarted ds 2	did we start a session?
wcard	ds	2	wildcards in use?

cbRec	dc	i'1'	ClearBackup record
cbPath	ds	4

bsRec	ds	2	session record
noProgress ds	2	noProgress flag set?
	longa on
	end

****************************************************************
*
*  End - END statement.
*
****************************************************************
*
End	private
	debug End
	using Common

	jsr	RemoveCommand	remove the command
	jsr	EOLCheck	check for garbage at end of command
	jcs	err
	jsr	TopType	get top of stack type
	cmp	#loopsy	if loopsy then
	bne	lb1
	jsr	BackUp	  back up
	bra	lb1a

lb1	cmp	#ifsy	else if ifsy then
	bne	lb2
	jsr	PopStack	  pop the marker
lb1a	lda	#0
	rts

lb2	cmp	#forsy	else if forsy then
	bne	lb3
	jsr	ForLoop
	bra	lb1a

lb3	puts	#'Unexpected END',cr=t,errout=t
err	lda	#$FFFF	error
	rts
	end

****************************************************************
*
*  Erase - ERASE
*
*  Erase a disk.
*
****************************************************************
*
Erase	private
	debug Erase
	using Common

	lda	#0
	brl	EraseDisk
	end

****************************************************************
*
*  Execute - Execute an EXEC file, but allow changes to local variables.
*
****************************************************************
*
Execute	private
	debug Execute
	using Common
	using reccom

	jsr	RemoveCommand	remove the command
	jsr	LineLength	if the length of the line is 0 then
	tax
	bne	lb1
	puts	#'File to execute: '	  get a line to execute
	jsr	GetLine
	jsr	LineLength	if the length of the line is 0 then
	tax
	beq	lb2	  quit
lb1	inc	executeCmd	flag the special handling
	jsr	PushLine	save line info
	bcc	err
	jsr	BRun	execute the program
	pha
	jsr	PullLine	restore line
	pla
lb2	rts

err	lda	#$FFFF	out of memory in PushLine
	rts
	end

****************************************************************
*
*  Exists - EXISTS
*
*  See if a file exists on disk.  If so, return a value of 1.
*
****************************************************************
*
Exists	private
	debug Exists
	using Common

	stz	error	assume the file does no exist
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	lb1
	dec	error	return error code of -1
	bra	rts

lb1	clc		get the file name
	lda	#0
	ldx	#1
	jsr	GetFileName
	bcs	rts	quit if the file does not exist
	jsr	SkipBlanks	next file name
	jsr	LineLength
	tax
	bne	lb1
	inc	error	file exists

rts	free	nxPath	free(nxPath)
	lda	error	handle any error
	cmp	#1
	beq	rts1
	jsr	CommandError
rts1	rts
;
;  Local data
;
error	ds	2	error number
wcard	ds	2	wildcards in use?
type	ds	2	file type
	end

****************************************************************
*
*  Exit - EXIT statement.
*
****************************************************************
*
Exit	private
	debug Exit
	using Common

	jsr	RemoveCommand	remove the command
	jsr	GetToken	get return code
	sta	r0
	stx	r2
	ora	r2
	beq	lb3
	lda	[r0]	if strlen(token) != 0 then
	beq	lb1
	jsr	ReadNumber	  read the number
	bcs	lb2
lb1	sta	exec_return	save exec return code
	free	r0	free the token buffer
	lda	#true	tell EXEC to quit
	sta	exec_exit
	lda	exec_return	quit
	rts

lb2	puts	#'EXIT parameter must be a number',cr=t,errout=t
	lda	#$FFFF
	bra	lb1

lb3	lda	#outOfMem	out of memory
	jsl	SysError
	rts
	end

****************************************************************
*
*  Export - EXPORT a variable.
*
****************************************************************
*
Export	private
	debug Export
	using Common

	stz	error	no error so far
	stz	exName	exName = nil
	stz	exName+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	sv1
	dec	error
	bra	rts

sv1	jsr	GetToken	if a variable name is not given then
	sta	exName
	sta	r0
	stx	exName+2
	stx	r2
	ora	r2
	bne	sv1a
oom	lda	#outOfMem
	sta	error
	brl	rts
sv1a	lda	[r0]
	bne	sv3
	jsr	ListExport	  list the variables
	bra	rts
;
;  Loop thru each name
;
sv2	free	exName	get a new token name
	jsr	GetToken
	sta	exName
	sta	r0
	stx	exName+2
	stx	r2
	ora	r2
	beq	oom
	lda	[r0]
	bne	sv3

rts	free	exName	free the token buffer
	lda	error	return the error code
	brl	CommandError

sv3	OSExport exRec	mark the variable as exportable
	bcc	sv2

err	puts	#'No such variable: ',errout=t
	ph4	exName
	ph2	#$4000
	jsl	PrintOSName
	putcr errout=t
	dec	error
	brl	sv2

error	ds	2	error flag

exRec	dc	i'2'	OSExport record
exName	ds	4
	dc	i'1'
	end

****************************************************************
*
*  FileType - Change the file type of a file.
*
****************************************************************
*
FileType private
	debug FileType
	using Common
minus_flags equ r0	minus flags mask

	jsr	RemoveCommand	remove the command
	flags	,(P)	get command line flags
	bcc	ft1
	lda	#$FFFF
	rts
;
;  Read in the file name
;
ft1	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress

	stz	buff	zero the token buffer pointer
	stz	buff+2
	stz	err	no error so far
	stz	changeAux	not changing the aux type
	stz	sessionStarted
	sec		get the file name (must exist)
	lda	#0
	ldx	#1
	jsr	GetFileName
	bcc	ft1a
	sta	err
	brl	rts2
ft1a	sta	wcard	save the wildcard flag
;
;  Get the file type
;
	jsr	GetToken	see if file type was specified
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]
	bne	ft2
	free	r0	dispose of the token buffer
	puts	#'File type: '	ask for a file type if none specified
	jsr	GetLine
	jsr	GetToken	if line is blank return
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]
	jeq	rts

ft2	jsr	GetType	get the file type
	bcc	ft3
	puts	#'Illegal file type',cr=t,errout=t
	dec	err
	brl	rts

ft3	sta	type	save the language type
	free	buff	dispose of the token
	jsr	GetToken	get the aux type token
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]	branch if there is none
	beq	ax1
	jsr	GetAux	get the aux type
	sta	aux
	stx	aux+2
	inc	changeAux
	bcc	ax1
	puts	#'Illegal aux type',cr=t,errout=t
	dec	err
	brl	rts

ax1	jsr	EOLCheck	check for garbage at end of command
	bcc	ft3a
	dec	err
	brl	rts
ft3a	lda	wcard	if we are using wildcards then
	beq	ft4
	OSBegin_Session bsRec	  start a session
	inc	sessionStarted	  note that we started one
	lda	noProgress                 if not noProgress then
	bne	ft4
	puts	#'Files changed:',cr=t       write header
;
;  Loop thru each file and change the file type
;
ft4	lda	type	set the file type
	sta	nxFileType
	lda	changeAux	if changeAux then
	beq	ft5
	move4 aux,nxAuxtype	  set the aux type
ft5	move	#0,nxModDateTime,#8	reset mod date/time
	lda	wcard	see if we need to print name
	beq	ft8
	lda	noProgress
	bne	ft8
	puts	#'  '
	ph4	nxPath
	ph2	#$8000
	jsl	PrintOSName
	putcr
ft8	add2	nxPath,#2	modify flags
	OSSet_File_Info nxRec
	sta	err
	jcs	rts
	sub2	nxPath,#2
ft9	jsr	Pause
	bcs	rts
	OSNext_Wildcard nxRec	get next file name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	ft4

rts	free	nxPath	free the wildcard name buffer
rts2	free	buff	free the token buffer
	lda	sessionStarted	if sessionStarted then
	beq	rts3
	OSEnd_Session bsRec	  stop the session
rts3	lda	err	handle any error
	brl	CommandError
;
;  Flag an out of memory error
;
oom	lda	#outOfMem
	sta	err
	bra	rts
;
;  Local data
;
aux	ds	4	new aux type
buff	ds	4	token buffer pointer
changeAux ds	2	change the AUX type?
err	ds	2	error code
noProgress ds	2	noProgress flag set?
sessionStarted ds 2	did we start a session?
wcard	ds	2	wildcards in use?
type	ds	2	new file type

bsRec	dc	i'0'	OSBegin_Session and OSEnd_Session record
	end

****************************************************************
*
*  For - FOR statement.
*
****************************************************************
*
For	private
	debug For
	using Common

	stz	ts	ts = nil
	stz	ts+2
	stz	error	error = 0

	jsr	RemoveCommand	remove the command
	jsr	GetToken	read the variable name
	sta	buff	error if missing a loop var
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	bne	in1
oom	lda	#outOfMem	(out of memory)
	sta	error
	brl	rts
in1	lda	[r0]
	bne	in2
	puts	#'Missing loop variable',cr=t,errout=t
	dec	error
	brl	rts

in2	jsr	Push	Push(buff)
	jcs	rts
	jsr	GetToken	next token
	sta	buff
	stx	buff+2
	ora	buff+2
	beq	oom
	ph4	buff	if token == "IN" then
	ph4	#In
	jsl	CompareOSStrings
	tax
	jne	lb4
	lda	#forsy	  Stack(forsy)
	jsr	Stack
	bcc	in3
ovfl	puts	#'Statement stack overflow',cr=t,errout=t
	dec	error
	brl	rts

in3	stz	count	  count = 0
	free	buff	  Free(buff) {currently set to "IN"}
lb1	jsr	GetToken	  while strlen((buff = GetToken)!= 0) do
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	beq	lb1a
	jsr	Push	    Push
	inc	count	    count++
	bra	lb1	  endwhile
lb1a	lda	count	  while count do
	beq	lb3
lb2	jsr	Pop	    Pop
	move4 buff,r0	    PushWord(buff)
	jsr	PushWord
	jcs	err1
	dec	count	    --count
	bne	lb2	  endwhile
lb3	brl	lb7

lb4	lda	[r0]	else if strlen(buff^) then
	beq	lb4a
	puts	#'''''IN'''' expected',cr=t,errout=t flag error
	dec	error
	brl	rts
!			else
lb4a	lda	#forsy	  Stack(forsy)
	jsr	Stack
	jcs	err1
	lla	r12,pound	  read the variable
	jsr	FindVariable
	lda	r0	  if no '#' var exists then
	ora	r2
	beq	lb6	    goto lb6
	ph4	r8	  count = OSStringToLong(r8)
	jsl	OSStringToLong
	sta	count
	tax
	beq	lb6
lb5	anop		  while count do
	ph2	count	    read(digit(count),r8)
	ph4	#str
	jsl	IntToOSString
	lla	r12,str
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb5a
	move4 r8,r0	    PushWord(r8)
	jsr	PushWord
	bcs	err1
lb5a	dec	count	    --count
	bne	lb5	  endwhile
lb6	anop
lb7	anop		endif
	jsr	Pop	Pop
	move4 buff,r0	PushWord(buff)
	jsr	PushWord
	bcs	err1
	lda	#forsy	Stack(forsy)
	jsr	Stack
	bcs	err1
	jsr	ForLoop2	ForLoop2

rts	free	buff	free(buff)
rts1	lda	ts	while ts <> nil do
	ora	ts+2
	beq	rts2
	jsr	Pop	  Pop
	free	buff	  Free(buff)
	bra	rts1
rts2	lda	error	report any errors
	brl	CommandError

err1	jsr	PopStack	stack overflow
	cmp	#forsy
	bne	err1
	brl	ovfl
;
;  POP - pop a value from the stack to buff
;
Pop	free	buff	free(buff)
	move4 ts,r0	r0 = ts
	lda	[r0]	ts = r0^.next
	sta	ts
	ldy	#2
	lda	[r0],Y
	sta	ts+2
	iny		buff = r0^.buff
	iny
	lda	[r0],Y
	sta	buff
	iny
	iny
	lda	[r0],Y
	sta	buff+2
	free	r0	Free(r0)
	rts
;
;  PUSH - push buff onto the token stack
;
Push	malloc #8	r0 = malloc(8)
	sta	r0
	stx	r2
	ora	r2	error return if out of memory
	beq	ps1
	lda	ts	r0^.next = ts
	sta	[r0]
	ldy	#2
	lda	ts+2
	sta	[r0],Y
	iny		r0^.buff = buff
	iny
	lda	buff
	sta	[r0],Y
	iny
	iny
	lda	buff+2
	sta	[r0],Y
	move4 r0,ts	ts = r0
	stz	buff	buff = nil
	stz	buff+2
	clc
	rts

ps1	lda	#outOfMem	flag out of memory error
	sta	error
	sec		error return
	rts
;
;  Local variables
;
buff	ds	4	token buffer pointer
count	ds	2	variable counter
error	ds	2	error number
ts	ds	4	pointer to first stacked token
str	ds	8	numberic string buffer
	end

****************************************************************
*
*  Help - On line help facility.
*
****************************************************************
*
Help	private
	debug Help
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	hp1
err	lda	#$FFFF
	rts

hp1	stz	error	no error so far
	stz	buff	buff = nil
	stz	buff+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	malloc #buffSize	get a path buffer
	sta	nxPath
	sta	r0
	stx	nxPath+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#buffSize
	sta	[r0]
	putcr		skip a line
	jsr	GetToken	read the option
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	jeq	oom
	lda	[r0]	see if option specified
	and	#$00FF
	bne	hp2
	jsr	PrintCommandTable	list the command table
	putcr		if no options then list options
	brl	exit
;
;  Type the help file
;
hp2	ph4	#helpDir	create a help directory path name
	ph4	buff
	ph2	#0
	jsl	AppendOSNames
	sta	buff
	stx	buff+2
	free	r0
	lda	buff
	ora	buff+2
	jeq	oom
	OSInit_Wildcard iwRec	set up for wild card search
	sta	error
	jcs	rts
	stz	first
;
;  Type out each help file
;
hp5	jsr	Pause	quit if ESC was pressed
	jcs	rts
	OSNext_Wildcard nxRec	get the file name
	move4 nxPath,r0	make sure we found a file
	ldy	#2
	lda	[r0],Y
	bne	hp7
	lda	first	if this is the first file then
	bne	hp6
	ph4	buff	 flag an error
	jsr	FileNotFound
	dec	error
	bra	rts
hp6	free	buff	free(buff)
	jsr	GetToken	get next file name
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	beq	oom
	lda	[r0]
	jne	hp2
	bra	rts

hp7	stz	r0	start at line 0
	stz	r2
	stz	r4	write the entire file
	stz	r6
	stz	r8	do not print line numbers
	lda	#1	expand tabs
	sta	r10
	inc	first
	putcr
	jsr	TypeFile	type the file
	lda	r0
	sta	error
	jeq	hp5

rts	free	buff	free(buff)
	free	nxPath	free(nxPath)
	lda	error	handle any error
	brl	CommandError

oom	lda	#outOfMem	handle out of memory error
	sta	error
	bra	rts

iwRec	dc	i'2'	OSInit_Wildcard record
buff	ds	4	token buffer/pathname
	dc	i'0'	flags

error	ds	2	error number
first	ds	2	first time flag
	end

****************************************************************
*
*  History - List the last 20 commands.
*
****************************************************************
*
History	private
	debug History
	using Common
lbuff	equ	r4	line buffer pointer

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	hp1
err	lda	#$FFFF
	rts

hp1	jsr	EOLCheck	check for garbage in line
	bcs	err
	move4 line_hand,lbuff	get pointer
	move	cline,lnDisp	set disp of first line
	la	ln,20	for LN = 20 downto 1 do
hp2	jsr	Pause	  if open-apple period pressed then
	bcs	hp8	    exit the loop early
	put2	ln,#2	  write line number
	puts	#': '
	ldy	lnDisp	  X = # chars in line
	lda	[lbuff],y
	and	#$00FF
	tax
	beq	hp6
	ldy	lnDisp	  for Y = 1 to X do
	iny
hp5	lda	[lbuff],y	    write a character
	jsl	~stdout
	iny		    next char
	dbne	x,hp5
hp6	putcr		  new line
	clc
	lda	lnDisp	  update line disp
	adc	#line_len+1
	cmp	#(line_len+1)*max_lines
	blt	hp7
	lda	#0
hp7	sta	lnDisp
	dbne	ln,hp2	next line
hp8	lda	#0
	rts

lnDisp	ds	2	disp to start of line
ln	ds	2	line number
	end

****************************************************************
*
*  Home - Clear the screen and home the cursor
*
****************************************************************
*
Home   	private
	debug Home
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	lb1
err	lda	#$FFFF
	rts

lb1	jsr	EOLCheck	check for garbage in line
	bcs	err

	home

     	lda	#0
	rts
	end

****************************************************************
*
*  If - IF statement
*
*  Conditional execution in an exec file
*
****************************************************************
*
If	private
	debug If
	using Common

	jsr	RemoveCommand	remove the command
	lda	#ifsy	mark stack use
	jsr	Stack
	jsr	Expression	evaluate expression
	bcs	lb1	if false then
	jsr	DoElse	  scan to else or end
	bcs	err
	txa		if end then
	beq	lb1
	jsr	PopStack	  pop if from stack
lb1	lda	#0
rts	rts

err	lda	#$FFFF
	rts
	end

****************************************************************
*
*  Init - INIT
*
*  Initialize a disk.
*
****************************************************************
*
Init	private
	debug Init
	using Common

	lda	#1
	brl	EraseDisk
	end

****************************************************************
*
*  Input - INPUT
*
*  Read a string and assign its value to a shell variable
*
****************************************************************
*
Input	private
	aif	apw,.input
	debug Input
	using Common

	jsr	RemoveCommand	remove the command
	jsr	GetToken	get the variable name
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	lda	[r0]
	bne	lb1
	free	r0
	puts	#'Variable to set: '
	jsr	GetLine
	jsr	GetToken
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	lda	[r0]
	bne	lb1
	rts

lb1	jsr	EOLCheck	check for garbage at end of command
	bcs	err
	jsr	GetLine	read the variable value from stdin
	lda	r0	set the variable value
	ldx	r2
	phx
	pha
	jsr	SetVariable
	pla
	plx
	jsl	Free
	lda	#0
	rts

oom	lda	#outOfMem	out of memory error
	jsl	SysError
	rts

err	free	r0	error return
	lda	#$FFFF
	rts
.input
	end

****************************************************************
*
*  Link - Link one or more object modules.
*
****************************************************************
*
Link	private
	debug Link
	using Common

	jsr	Operands	read the operand
	bcs	err
	lm	li_ops,#%00000010	set operations flag
	brl	Compile	do it

err	lda	#$FFFF	error return
	rts
	end

****************************************************************
*
*  Loop - LOOP statement.
*
****************************************************************
*
Loop	private
	debug Loop
	using Common

	jsr	RemoveCommand	remove the command
	jsr	EOLCheck	check for garbage at end of command
	bcs	err
	lda	#loopsy	mark the loop
	jsr	Stack
	bcs	err
	lda	#0
	rts

err	lda	#$FFFF	error
	rts
	end

****************************************************************
*
*  Move - Move files.
*
****************************************************************
*
Move	private
	debug Move
	using Common

	jsr	RemoveCommand	remove the command
	flags ,(C,P)	get command line flags
	bcc	lb0
	lda	#$FFFF
	rts

lb0	lda	#0	do a move
	brl	FileCopy
	end

****************************************************************
*
*  Newer - NEWER
*
*  See if any of a list of files is newer than a target file.
*
****************************************************************
*
Newer	private
	debug Newer
	using Common
minus_flags equ r0	minus flags mask
;
;  Initialization
;
	stz	error	assume the file does not exist
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	RemoveCommand	remove the command
	flags	,C	get command line flags
	bcc	lb1
	dec	error	return error code of -1
	brl	rts
;
;  Get the main file compare date
;
lb1	sec		get the file name
	lda	#$4000
	ldx	#0
	jsr	GetFileName
	jcs	erts	quit if the file does not exist
	jsr	SkipBlanks	skip blanks on the command line
	add4	nxPath,#2,giPathname	get time for the first file
	OSGet_File_Info giRec
	jcs	erts
	jsr	MungeDate	get the correct date
	sta	nDate
	stx	nDate+2
	sty	nDate+4
	free	nxPath	free(nxPath)
	stz	nxPath	nxPath = nil
	stz	nxPath+2
; 
;  Check dates against files in list
;
dt1	sec		get the file name
	lda	#$3000
	ldx	#0
	jsr	GetFileName
	sta	error
	jcs	rts
	stz	error

dt2	jsr	Spin	spin the spinner
	add4	nxPath,#2,giPathname	get time for the file
	OSGet_File_Info giRec
	jcs	erts
	jsr	MungeDate	get the correct date
	cpy	nDate+4	if more recent then
	bne	dt3
	cpx	nDate+2
	bne	dt3
	cmp	nDate
dt3	ble	dt4
	lda	#1	  error = 1
	sta	error
	bra	rts	  return
dt4	OSNext_Wildcard nxRec	get next file name
	move4 nxPath,r4
	ldy	#2	if there is one then loop
	lda	[r4],Y
	bne	dt2
	free	nxPath	free(nxPath)
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	jsr	SkipBlanks	get next file name on line
	jsr	LineLength
	tax
	jne	dt1
;
;  Return to caller
;
	bra	rts
erts	sta	error	return general error
rts	free	nxPath	free(nxPath)
	lda	error	handle any error
	cmp	#1
	beq	rts1
	jsr	CommandError
rts1	rts
;
;  MungeDate - get the correct date field and place it in ascending order
;
MungeDate anop

	lda	minus_flags+2	if -c set then
	and	#^set_c
	beq	md1
	lda	cDate	  mDate = cDate
	sta	mDate
	lda	cDate+2
	sta	mDate+2
	lda	cDate+4
	sta	mDate+4
md1	anop		endif
	short	M
	lda	mDate+3
	ldx	mDate+4
	stx	mDate+3
	sta	mDate+5
	long	M
	lda	mDate
	ldx	mDate+2
	ldy	mDate+4
	rts
;
;  Local data
;
error	ds	2	error number
nDate	ds	6	main date to compare to

giRec	dc	i'7'	OSGet_File_Info record
giPathName ds	4
	ds	2
	ds	2
	ds	4
	ds	2
cDate	ds	8	create date
mDate	ds	8	mod date
	end

****************************************************************
*
*  Prefix - Set a prefix.
*
****************************************************************
*
Prefix	private
	debug Prefix
	using Common
minus_flags equ r0	minus flags mask
DIR	equ	$0F

	stz	epOut	epOut = nil
	stz	epOut+2
	stz	err	no error so far
	jsr	RemoveCommand	remove the command
	flags	,C	get command line flags
	bcc	pr1
myErr	dec	err
	brl	rts

pr1	lda	minus_flags+2	noCheck = <-c flag set>
	and	#^set_c
	sta	noCheck
	jsr	GetToken	read the path name to create
	sta	r0
	stx	r2
	lda	[r0]
	bne	pr3
pr2	free	r0	Free(r0)
	puts	#'Path name: '	none given -- ask for one
	jsr	GetLine
	jsr	GetToken
	sta	r0
	stx	r2
	lda	[r0]
	jeq	rts
pr3	lda	#8	default prefix is 8
	sta	prPrefixNum
	ph4	r0	see if the token is a number
	jsr	StrToNum
	txy
	beq	pr4
	sta	prPrefixNum	  yes - set the prefix number
	free	r0	  Free(r0)
	jsr	GetToken	  get next token
	sta	r0
	stx	r2
	lda	[r0]	  ask for name if none is given
	beq	pr2	if no name given then go get one
pr4	jsr	EOLCheck	check for garbage at end of line
	bcs	myErr
	move4 r0,epIn	expand devices and prefixes
	ph4	#epRec
	jsr	ExpandDevice
	sta	err
	tax
	jne	rts
	add4	epOut,#2,r4
	lda	noCheck	if not noCheck then
	bne	pr5
	ph4	r4                         make sure the prefix exists
	jsr	GetFileType
	cmp	#DIR
	beq	pr5
	lda	#$0044
	sta	err
	bra	rts
pr5	move4 r4,prPrefix	set the prefix
	OSSet_Prefix prRec
	sta	err

rts	free	r0	Free(r0)
	free	epout	Free(epOut)
	lda	err	handle any error
	brl	CommandError
;
;  Local data
;
err	ds	2	error number
noCheck	ds	2	check for nonexistant prefixes?

epRec	dc	i'2'	expand devices record
epIn	ds	4
epOut	ds	4

prRec	dc	i'2'	OSSet_Prefix record
prPrefixNum ds 2
prPrefix ds	4
	end

****************************************************************
*
*  Quit - Pass control to ProDOS.
*
****************************************************************
*
Quit	private
	debug Quit
	using Common

	jsr	RemoveCommand	remove the command
	jsr	EOLCheck	make sure there are no parms
	jcc   QuitShell
	lda	#$FFFF
	rts
	end

****************************************************************
*
*  Rename - Rename a file.
*
****************************************************************
*
Rename	private
	debug Rename
	using Common

	stz	error	no error so far
	stz	edIn	edIn = nil
	stz	edIn+2
	stz	edOut	edOut = nil
	stz	edOut+2
	stz	iwName	iwName = nil
	stz	iwName+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	rn1
err	lda	#$FFFF
	rts
;
;  Read in the file to rename
;
rn1	jsr	GetToken	get the new file name
	sta	r0
	sta	iwName
	stx	r2
	stx	iwName+2
	ora	r2
	jeq	oom
	lda	[r0]	if none given then
	bne	ff1
	free	r0	  free the token buffer
	puts	#'New path name: '	  ask for a new name
	jsr	GetLine
	jsr	GetToken	if line is blank return
	sta	r0
	sta	iwName
	stx	r2
	stx	iwName+2
	ora	r2
	jeq	oom
	lda	[r0]
	jeq	rts
ff1	OSInit_Wildcard iwRec	expand the name
	sta	error
	jcs	rts
	malloc #buffSize	get a wildcard name buffer
	sta	nxPath
	stx	nxPath+2
	ora	nxPath+2
	beq	oom
	move4 nxPath,r0
	lda	#buffSize
	sta	[r0]
	OSNext_Wildcard nxRec	get the first matching name
	bcs	ff2
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	beq	ff2
	add2	nxPath,#2
	bra	ff3
ff2	free	nxPath	could not expand; try to use the
	move4 iwName,nxPath	 original name
	stz	iwName
	stz	iwName+2
;
;  Get the new name
;
ff3	jsr	GetToken	get the new file name
	sta	r0
	sta	edIn
	stx	r2
	stx	edIn+2
	ora	r2
	bne	rn1a
oom	lda	#outOfMem
	sta	error
	brl	rts
rn1a	lda	[r0]	if none given then
	bne	rn2
	free	r0	  free the token buffer
	puts	#'New path name: '	  ask for a new name
	jsr	GetLine
	jsr	GetToken	if line is blank return
	sta	r0
	sta	edIn
	stx	r2
	stx	edIn+2
	ora	r2
	beq	oom
	lda	[r0]
	jeq	rts
rn2	ph4	#edRec	expand devices and prefixes
	jsr	ExpandDevice
	jsr	EOLCheck	check for garbage at end of command
	bcs	rts
;
;  Rename the files
;
rn3	move4 nxPath,cpPathname
	add4	edOut,#2,cpNewPathname
	OSChange_Path cpRec	change the path
	sta	error
;
;  Return to the command editor
;
rts	free	nxPath	free the wildcard name buffer
	free	edIn	free the new name input buffer
	free	edOut	free the new name output buffer
	free	iwName	free the input name buffer
	lda	error	report any error found
	brl	CommandError
;
;  Local data
;
error	ds	2	error number

cpRec	dc	i'2'	OSChange_Path record
cpPathname ds	4
cpNewPathname ds 4

edRec	dc	i'2'	ExpandDevice record
edIn	ds	4
edOut	ds	4

iwRec	dc	i'2'	InitWildcard record
iwName	ds	4
	dc	i'0'
	end

****************************************************************
*
*  Set - SET a variable.
*
****************************************************************
*
Set	private
	debug Set
	using Common

	stz	buff	buff = nil
	stz	buff+2
	stz	error	no error
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	in1
	dec	error
	brl	rts

in1	jsr	GetToken	if a variable name is not given then
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	ora	r2
	bne	sv0
	lda	#outOfMem
	sta	error
	brl	rts
sv0	lda	[r0]
	bne	sv1
	jsr	ListVariables	  list all the variables
	brl	rts
;
;  Print a specific variable
;
sv1	jsr	SkipBlanks	skip blanks in command line
	jsr	LineLength	if there is no string then
	tax
	jne	sv5
	move4 buff,r12	  read the variable
	jsr	FindVariable
	lda	r0
	ora	r2
	bne	sv2
	puts	#'No such variable',cr=t,errout=t
	dec	error
	bra	rts

sv2	puts	#'Set '	  print the string
	ph4	r4
	ph2	#0
	jsl	PrintOSName
	puts	#'  '
	ph4	r8
	ph2	#0
	jsl	PrintOSName
	putcr
	bra	rts
;
;  Set a variable
;
sv5	lda	buff
	ldx	buff+2
	jsr	SetVariable
;
;  Return to caller
;
rts	free	buff	free(buff)
	lda	error	report any error found
	brl	CommandError
;
;  Local data
;
buff	ds	4	string buffer
error	ds	2	error number
	end

****************************************************************
*
*  SetLanguage - Set the current language number.
*
****************************************************************
*
SetLanguage private
	debug SetLanguage
	using Common

	phy		save the language number
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcs	err
	jsr	EOLCheck	check for garbage at end of command
	bcs	err
	ply
	sty	slNum	set up for set_lang call
	Set_Lang slDCB	set the language number
rts	lda	#0
	rts

err	ply		clean up stack
	lda	#$FFFF
	rts

slDCB	anop		get/set language DCB
slNum	ds	2	language number
	end

****************************************************************
*
*  Show - Shows the settings of system attributes.
*
****************************************************************
*
Show	private
	debug Show
	using Common

	stz	error	no error so far
	stz	buff	buff = nil
	stz	buff+2
	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	sh1
	dec	error
	brl	rts

sh1	jsr	GetToken	read the option
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	lda	[r0]	see if option specified
	jne	sh5
	putcr		if no options then list options
	puts	#'Options:',cr=t	 available
	putcr
	lda	#<names
	sta	r0

sh2	dec	r0
	puts	{r0}
	puts	#'    '
	inc	r0
	sec
	lda	(r0)
	and	#$00FF
	beq	sh3
	adc	r0
	sta	r0
	bra	sh2
sh3	putcr		ask for options
	putcr
	puts	#'List desired options: '
	jsr	GetLine
;
;  Process options
;
sh4	free	buff	free the old token buffer
	jsr	GetToken	read the option
	sta	r0
	sta	buff
	stx	r2
	stx	buff+2
	lda	[r0]	if none specified then quit
	jeq	rts

sh5	lda	#<names	check for a match in the list
	sta	r4
	lda	[r0]
	xba
	sta	[r0]
	inc4	r0
sh6	short I,M
	lda	[r0]
	tay
sh7	lda	[r0],Y
	jsl	ToUpper
	cmp	(r4),Y
	bne	sh8
	dbpl	Y,sh7
	bra	sh9

sh8	long	I,M
	sec
	lda	(r4)
	and	#$00FF
	adc	r4
	sta	r4
	inc	r6
	inc	r6
	lda	(r4)
	and	#$00FF
	bne	sh6
	puts	#'Unknown option: ',errout=t
	dec	r4
	puts	{r4},cr=t,errout=t
	dec	error
	bra	rts

sh9	long	I,M
	jsr	StopSpin
	putcr
	ldx	r6
	lda	ads,x
	sta	jsr+1
jsr	jsr	jsr
	brl	sh4

rts	free	buff	free the token pointer
	lda	error	handle any error
	brl	CommandError
;
;  Local data
;
buff	ds	4	token buffer pointer
error	ds	2	error code

names	dw	LANGUAGE	option names
	dw	LANGUAGES
	dw	PREFIX
	dw	TIME
	dw	UNITS
	dc	i'0'

ads	dc	a'PrintLanguage'	option routine table
	dc	a'PrintLanguages'
	dc	a'PrintPrefix'
	dc	a'PrintTime'
	dc	a'PrintUnits'
	end

****************************************************************
*
*  Switch - Switch order of directories.
*
****************************************************************
*
Switch	private
	aif	apw,.switch_1
	debug Switch
	using Common
minus_flags equ r0	minus flags mask

	stz	wcard	no wild cards
	stz	error	no error
	stz	firstPath	firstPath = nil
	stz	firstPath+2
	stz	secondPath	secondPath = nil
	stz	secondPath+2
	stz	pathName	pathName = nil
	stz	pathName+2
	jsr	RemoveCommand	remove the command
	flags	,P	get command line flags
	bcc	sw1
	dec	error
	brl	rts

sw1	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress

	sec		get the first file name
	lda	#0
	ldx	#1
	jsr	GetFileName
	sta	wcard
	bcs	sw2
	add4	nxPath,#2,firstPath
	sec		get the second name
	lda	#0
	ldx	#1
	jsr	GetFileName
	ora	wcard
	sta	wcard
	bcc	sw2a
sw2	sta	error
	brl	rts
sw2a	add4	nxPath,#2,secondPath
	jsr	EOLCheck	check for garbage in input line
	bcc	sw2b
	dec	error
	brl	rts
sw2b	ph4	firstPath	form the directory name
	jsl	DuplicateOSString
	sta	pathName
	sta	r0
	stx	pathName+2
	stx	r2
	ora	r2
	bne	sw3
	lda	#outOfMem
	sta	error
	brl	rts
sw3	lda	[r0]
	tay
	iny
	short M
	lda	#':'
sw4	cmp	[r0],Y
	beq	sw5
	dey
	bne	sw4
	iny
sw5	long	M
	dey
	tya
	sta	[r0]
	move4 secondPath,r4	make sure paths match
	lda	[r0]
	tay
	jeq	err2
	iny
	short M
sw6	lda	[r0],Y
	cmp	[r4],Y
	jne	err2
	dey
	cpy	#1
	bne	sw6
	long	M
	jsr	RemovePath	form file names
	move4 firstPath,r4
	jsr	RemovePath
	lda	wcard	if wildcards were used then
	jeq	sw7
	lda	noProgress                 if not noProgress then
	jne	sw7
	puts	#'Files switched:',cr=t	    print the path names
	puts	#'  '
	ph4	firstPath
	ph2	#0
	jsl	PrintOSName
	putcr
	puts	#'  '
	ph4	nxPath
	ph2	#$8000
	jsl	PrintOSName
	putcr
sw7	jsl	ReadDirectory	read the directory
	sta	error
	jcs	rts
	move4 firstPath,r0	switch the files
	move4 secondPath,r4
	jsl	SwitchFiles
	jsl	WriteDirectory	write the directory

rts	free	secondPath	free the path name buffers
	free	firstPath
	free	pathName
	lda	error	handle errors
	brl	CommandError

err2	long	M
	puts	#'Files must be in the same directory',cr=t,errout=t
	dec	error
	bra	rts
;
;  RemovePath - remove the directory part from a path name
;
RemovePath anop
	lda	[r0]	for [r0] characters do
	sta	r12
	add4	r4,#1,r8
rp1	lda	[r4]	  for [r4] = [r4]-1 characters do
	dec	A
	sta	[r4]
	tax
	ldy	#2	    remove the first character
	short M
rp2	lda	[r8],Y
	sta	[r4],Y
	iny
	dex
	bne	rp2
	long	M
	dec	r12
	bne	rp1
	rts
;
;  Local data
;
error	ds	2	error number
firstPath ds	4	pointer to first path name
noProgress ds	2	noProgress flag set?
secondPath ds	4	pointer to first path name
pathName ds	4	pointer to the directory name
wcard	ds	2	wildcard flag
.switch_1
	end

****************************************************************
*
*  Touch - TOUCH
*
*  Change the mod date of a file on disk
*
****************************************************************
*
Touch	private
	debug Touch
	using Common
minus_flags equ r0	minus flags mask

	stz	error	no error so far
	stz	sessionStarted	no session started
	lda	#1	this is the first file
	sta	firstFile
	jsr	RemoveCommand	remove the command
	flags	,P	get command line flags
	bcc	en1
err	dec	error
	brl	rts
;
;  Loop over files, changing the mod date
;
en1	lda	minus_flags+2	noProgress = <-p flag set>
	and	#^set_p
	sta	noProgress
	OSBegin_Session bsRec	start a session
	inc	sessionStarted	note that we started one

lp1	lda	firstFile	get the file name
	ror	A
	lda	#0
	ldx	#1
	jsr	GetFileName
	sta	wcard	save the wildcard flag
	stz	firstFile	no longer on the first file
	bcc	lp2
	dec	error
	brl	rts
lp2	lda	wcard	if wildcards are used then
	beq	lp4
	lda	noProgress                 if not noProgress then
	bne	lp4
	puts	#'Files changed:',cr=t       print the header

lp3	lda	wcard	if wildcards are in use then
	beq	lp4
	lda	noProgress                 if not noProgress then
	bne	lp4
	puts	#'  '	    print the name of the file
	ph4	nxPath
	ph2	#$8000
	jsl	PrintOSName
	putcr

lp4	inc	nxPath	modify mod date
	inc	nxPath
	move	#0,nxModDateTime,#8
	OSSet_File_Info nxRec
	dec	nxPath
	dec	nxPath
	sta	error
	bcs	rts
	jsr	Pause
	bcs	rts
lp7	OSNext_Wildcard nxRec	get next file name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	lp3
	brl	lp1
;
;  Clean up and return
;
rts	lda	sessionStarted	if sessionStarted then
	beq	rts1
	OSEnd_Session bsRec	  stop the session
rts1	lda	error	handle any error
	brl	CommandError
;
;  Local variables
;
error	ds	2	error number
firstFile ds	2	is this the first file?
noProgress ds	2	noProgress flag set?
sessionStarted ds 2	did we start a session?
wcard	ds	2	wildcards in use?

bsRec	ds	2	session record
	end

****************************************************************
*
*  Type - List one or more files to the standard output.
*
****************************************************************
*
Type	private
	debug Type
	using Common
	using IOCom
plus_flags equ r4	plus flags mask
minus_flags equ r0	minus flags mask
DIR	equ	$0F	directory file type

	jsr	RemoveCommand	remove the command
	flags (N,T),(A,N,T)	get command line flags
	bcc	ty1
err	lda	#$FFFF
	rts

ty1	stz	error	no error so far
	stz	abortType	not aborted, yet
	stz	num_flag	initialize the number flag
	lla	line_num,1	initialize the line counter
	stz	etabs	clear the expand tabs flag
	lda	consoleRefnum	if stdout is to the console then
	cmp	~stoutRefnum
	bne	fl0
	inc	etabs	  set the expand tabs flag
fl0	lda	minus_flags	if -t then
	and	#set_t
	beq	fl1
	stz	etabs	  clear the expand tabs flag
fl1	lda	plus_flags	if +t then
	and	#set_t
	beq	fl2
	inc	etabs	  set the expand tabs flag
fl2	anop
	stz	non_ascii	initialize non-ascii flag
	lda	minus_flags+2	if non-ascii allowed then
	and	#^set_a
	bne	ty1a
	inc	non_ascii	  non_ascii = true
ty1a	lda	plus_flags+2	if +N flag is set then
	and	#^set_n
	beq	ty2
	inc	num_flag	set the number flag to true
ty2	lda	minus_flags+2	if -N flag is set then
	and	#^set_n
	beq	ty3
	stz	num_flag	set the number flag to false

ty3	sec		get the first file name
	lda	#0
	ldx	#1
	jsr	GetFileName
	bcc	ty5
	dec	error
	brl	rts
;
;  Type out each file on the input line
;
ty4	free	nxPath	dispose of the old path buffer
	clc
	lda	#0
	ldx	#1
	jsr	GetFileName
	jcs	rts
;
;  Process all the wild card files for this file
;
ty5	stz	num1	initialize the line counters
	stz	num1+2
	stz	num2
	stz	num2+2
	jsr	SkipBlanks
	jsr	LineLength	see if there are line numbers
	tax
	beq	ty7
	ldx	#0
	jsr	NextChar
	jsl	IsDigit
	bcc	ty7
	jsr	GetToken	get the first line number
	sta	r0
	stx	r2
	ora	r2
	jeq	oom
	ph4	r0
	jsl	OSStringToLong	convert string to number
	sta	num1
	stx	num1+2
	free	r0	free the token buffer
	jsr	SkipBlanks	look for second line number
	beq	ty7
	ldx	#0
	jsr	NextChar
	jsl	IsDigit
	bcc	ty7
	jsr	GetToken	get the second number
	sta	r0
	stx	r2
	ora	r2
	beq	oom
	ph4	r0
	jsl	OSStringToLong	convert string to number
	sta	num2
	stx	num2+2
	free	r0	free the token buffer

ty7	jsr	Pause	quit if esc has been pressed
	bcs	rts
	lda	abortType	quit if aborted in typeBuffer
	bne	rts
	move4 num1,r0	set up the type parameters
	move4 num2,r4
	lda	num_flag
	sta	r8
	lda	etabs
	sta	r10
	jsr	TypeFile	type the file
	lda	r0
	sta	error
	bne	rts

	OSNext_Wildcard nxRec	get next wildcard file name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	bne	ty7
	brl	ty4	no more - get a new name from the
!			 command line

oom	lda	#outofmem	out of memory error
	sta	error
	jsl	SysError
rts	free	nxPath	free the path name buffer
	lda	error	return the error
	rts

abortType entry 	abort flag (set by TypeBuffer)
	ds	2
error	ds	2	error number
etabs	ds	2	expand tabs flag
num_flag ds	2	number flag
num1	ds	4	line numbers
num2	ds	4
	end

****************************************************************
*
*  Unalias - remove a command alias
*
****************************************************************
*
Unalias	private
	debug Unalias
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	ua1
	lda	#$FFFF
	rts

ua1	stz	errflag	no error so far
	jsr	GetToken	if there is no name then
	sta	r12
	stx	r14
	ora	r14
	bne	ua1a
oom	lda	#outOfMem
	sta	errflag
	brl	rts
ua1a	lda	[r12]
	bne	ua3
	free	r0	  free the name buffer
	puts	#'Alias to unset: '	  ask for one
	jsr	GetLine
;
;  Loop thru names
;
ua2	jsr	GetToken	if no name
	sta	r12
	stx	r14
	ora	r14
	beq	oom
	lda	[r12]
	beq	rts
ua3	jsr	UnsetAlias	unset alias
	bcc	ua2	if no error get another name

	puts	#'No such alias: ',errout=t
	ph4	r12
	ph2	#$4000
	jsl	PrintOSName
	putcr errout=t
	dec	errflag

rts	lda	errflag	 return error flag
	jmp	CommandError

errflag	ds	2	error flag
	end

****************************************************************
*
*  Unset - UNSET a variable.
*
****************************************************************
*
Unset	private
	debug Unset
	using Common

	jsr	RemoveCommand	remove the command
	flags		get command line flags
	bcc	us1
	lda	#$FFFF
	rts

us1	stz	errflag
	jsr	GetToken	if there is no name then
	sta	svName
	sta	r0
	stx	svName+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	bne	us3
	puts	#'Variable to unset: '	  ask for one
	jsr	GetLine
;
;  Loop thru names
;
us2	free	svName                   free the last name
	jsr	GetToken	if no name
	sta	svName
	sta	r0
	stx	svName+2
	stx	r2
	ora	r2
	beq	oom
	lda	[r0]
	beq	rts                        quit

us3	OSUnsetVariable svRec	unset the variable
	bcc	us2	if no error get another name

	puts	#'No such variable: ',errout=t
	ph4	svName
	ph2	#$4000
	jsl	PrintOSName
	putcr
	dec	errflag	indicate error
	bra	us2

rts	free	svName
	lda	errflag
	jmp	CommandError

oom	lda	#outOfMem
	sta	errflag
	bra	rts

errflag	ds	2	error flag
svRec	dc	i'1'	unset variable record
svName	ds	4
	end

****************************************************************
*
*  AppendObj - append a name to the obj list
*
*  Inputs:
*	r0 - pointer to os string to append
*
*  Outputs:
*	objName - pointer to appended name
*
****************************************************************
*
AppendObj private
	using Common

	lda	objPtr	if objPtr = nil then
	ora	objPtr+2
	bne	ap1
	move4 r0,objPtr	  objPtr = r0
	rts		else
ap1	ph4	objPtr	  r0 = Append(Append(objPtr,' '),r0)
	ph4	#blank	  free(objPtr)
	ph2	#0	  free(r0)
	jsl	AppendOSNames
	phx
	pha
	sta	r4
	stx	r6
	free	objPtr
	ph4	r0
	ph2	#0
	jsl	AppendOSNames
	sta	objPtr
	stx	objPtr+2
	free	r0
	free	r4
	rts

blank	dc	i'1',c' '	OS string with one blank
	end

****************************************************************
*
*  BRun - Execute a program
*
*  Inputs:
*	LINE - input line with program name as first entry
*
****************************************************************
*
BRun	private
	debug BRun
	using Common

	stz	lptr	lptr = nil
	stz	lptr+2
	stz	edPathname	edPathname = nil
	stz	edPathname+2
	jsr	LineLength	reserve a name buffer
	inc	A
	inc	A
	inc	A
	jsl	Malloc
	sta	lptr
	sta	r0
	stx	lptr+2
	stx	r2
	ora	r2
	jeq	oom
	ldx	#0	copy the command name
	ldy	#2
lb1	jsr	NextChar
	sta	[r0],Y
	cmp	#RETURN
	beq	lb2
	jsl	IsWhite
	bcs	lb2
	iny
	bra	lb1
lb2	tya
	dec	A
	dec	A
	sta	[r0]
	jsr	IsUtility	if this is a utility, set the
	bcc	lb2a	 path to 6/name
	ph4	r0
	ph4	#util
	ph4	r0
	ph2	#0
	jsl	AppendOSNames
	sta	r0
	stx	r2
	pla
	plx
	jsl	Free
	lda	r0
	ora	r2
	jeq	oom
lb2a	move4 r0,edIn	expand devices and prefixes
	ph4	#edRec
	jsr	ExpandDevice
	tax
	beq	lb2b
	pha		handle an error expanding the path
	ph4	edIn
	jsr	FileNotFound
	pla
	bra	lb3

lb2b	free	r0	dispose of the original name
	add4	edPathname,#2
	move4 edPathname,r0	set the command name
	jsr	SetCommandName
	jsr	LineAddr	call the program
	sta	r4
	stx	r6
	sta	lptr
	stx	lptr+2
	ora	r6
	beq	oom
	move4 edPathname,r0
	ph4	lptr
	ph4	edPathname
	jsr	Call
	plx
	stx	edPathname
	plx
	stx	edPathname+2
	plx
	stx	lptr
	plx
	stx	lptr+2
lb3	pha
	free	lptr	dispose of the line pointer
	free	edPathname	dispose of the path name
	pla
	rts

oom	lda	#outOfMem
	jsl	SysError
	rts
;
;  Local data
;
lptr	ds	4	temp storage for pointers

edRec	dc	i'2'	OSExpandDevices record
edIn	ds	4
edPathname ds	4
	end

****************************************************************
*
*  Call - load and execute a program
*
*  The file is searched for and loaded if found.  Direct page
*  and stack space is allocated, and a user id assigned.  The
*  user ID is placed in TASK_ID and passed to the user in A.
*  The address of the input line (located in PARMS) is passed
*  in X-Y (X is bank, Y is l.s.word).  A JML is used to enter
*  the program, so the caller should do a JSL to S_CALL to get
*  control back from the program.
*
*  Inputs:
*	R0 - addr of file name of program to execute
*	R4 - points to input string (0 if none)
*
*  Outputs:
*	C - set if error during call
*
****************************************************************
*
Call	start
	debug Call
	using Common
EXEC	equ	6	EXEC file aux type

defaultStackSize equ 4096	default stack size

stack_size equ buff1	size of stack
stack_addr equ buff1+2	location of stack
addr	equ	buff1+6	address of program
entry_stack equ buff1+10	S at entry
shell_file equ $B5	shell file file type
sys16_file equ $B3	sys16 file file type
system_file equ $FF	system file file type
;
;  Form the command line string
;
	stz	prefixPtr	prefixPtr = nil
	stz	prefixPtr+2
	stz	epPathname	epPathname = nil
	stz	epPathname+2
	move4 r0,epIn	save the path name
	ph2	application_id	save volitile variables
	ph4	ctlast
	ph2	reset_app
	ph2	entry_stack
	ph2	user_call_stack
	tsx		save stack for error exit
	stx	entry_stack

	stz	allocated	allocated = false
	lda	r4	if (r4 = nil) or (r4^ = 0) then
	ora	r6
	beq	fs1
	lda	[r4]
	and	#$00FF
	bne	fs2
fs1	lla	linePtr,host	  use the BYTEWRKS line
	bra	fs8	else
fs2	ldy	#0	  find the length of the string
	short M
fs3	lda	[r4],Y
	beq	fs4
	iny
	bra	fs3
fs4	long	M
	tya		  reserve the memory
	clc
	adc	#10
	jsl	Malloc
	sta	linePtr
	sta	r8
	stx	linePtr+2
	stx	r10
	ora	r10
	jeq	oom
	sub4	r4,#8	  adjust the null string's pointer
	ldy	#0	  copy over 'BYTEWRKS'
fs5	lda	host,Y
	sta	[r8],Y
	iny
	iny
	cpy	#8
	bne	fs5
	short M	  skip leading whitespace
fs5a	lda	[r4],Y
	jsl	IsWhite
	bcc	fs6
	inc4	r4
	bra	fs5a
fs6	lda	[r4],Y	  copy over the command line
	sta	[r8],Y
	beq	fs7
	iny
	bra	fs6
fs7	long	M
	lda	#0	  set null terminator word
	sta	[r8],Y
	lda	#1	  allocated = true
	sta	allocated
fs8	anop		endif
;
;  Split based on type of file
;
sp1	ph4	r0	get the file info
	jsr	GetFileType
	tay
	bpl	sp2
	lda	#$46
	brl	err
sp2	sta	fileType
	stx	auxType
	cmp	#SRC	skip if not a source file
	bne	cl1
	cpx	#EXEC	skip if not a script file
	bne	cl1
	ph4	linePtr	save volitile variables
	ph4	prefixPtr
	ph2	allocated
	ph4	epPathName
	jsr	CallScript	go do an EXEC
	plx		restore
	stx	epPathName
	plx
	stx	epPathName+2
	plx
	stx	allocated
	plx
	stx	prefixPtr
	plx
	stx	prefixPtr+2
	plx
	stx	linePtr
	plx
	stx	linePtr+2
	sec
	brl	rtl
;
;  Call a program
;
cl1	lda	executeCmd	make sure EXECUTE only does EXEC files
	beq	cl2
	puts	#'Cannot EXECUTE files that are not EXEC',cr=t,errout=t
	lda	#$FFFF
	sec
	brl	rtl

cl2	jsr	PurgeAll	purge the output buffers
	lda	fileType	make sure its a shell file
	cmp	#shell_file
	beq	cl3
	cmp	#sys16_file	... or a SYS16 file
	beq	cl3
	cmp	#system_file	... or a SYSTEM file
	beq	cl3

	puts	#'Cannot execute a non-script file.',cr=t,errout=t
	lda	#$FFFF
	sec
	brl	rtl

cl3	stz	waitFlag	turn off waits in Pause
	ph4	#epRec	expand the path name
	jsr	ExpandPath
	tax
	jne	err
	add4	epPathname,#2
	ph4	epPathname	form the prefix
	jsl	DuplicateOSString
	sta	prefixPtr
	sta	r0
	stx	prefixPtr+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	tay
	iny
	sty	r4
	short M
cl4	lda	[r0],Y
	cmp	#':'
	beq	cl4a
	dey
	bne	cl4
cl4a	long	M
	dey
	tya
	sta	[r0]
	stz	appName	form the application name
	short M
	ldx	#0
	iny
an1	iny
	lda	[r0],Y
	sta	appName+2,X
	cpx	#l:appName-2
	beq	an2
	inx
	cpy	r4
	bne	an1
an2	stx	appName
	long	M
	OSSet_Prefix prRec	set prefix 9
	jcs	err
	lda	auxType	if the file is not GS/OS aware then
	and	#$FF00
	cmp	#$DB00
	bne	ft1
	lda	auxType
	lsr	A
	bcs	ft2
ft1	jsr	SetOldPrefixes	  set the old 8 bit prefixes
	tax
	jne	err
ft2	lda	fileType	branch if SYS16 or SYSTEM
	cmp	#sys16_file
	jeq	sys16
	cmp	#system_file
	jeq	sys16
	pha		make room for return values from the
	pha		 loader
	pha
	pha
	pha
	lda	ctlast	if the application is reentrant and
	ora	ctlast+2
	beq	rs1
	pha		  it is already in memory then
	ph4	epPathname
	_GetUserID2
	pla
	bcs	rs1
	pha		    restart
	_Restart
	bra	rs2	else
rs1	ph2	#0	  initial load
	ph4	epPathname
	ph2	#0
	ph2	#1
	_InitialLoad2
rs2	sta	r0	save any possible error code
	pl2	application_id	recover application's user id
	pl4	addr	recover other parameters
	pl2	stack_addr
	pl2	stack_size
	lda	r0	restore any possible error code
	jcs	err
	lda	ctlast	if ctlast != nil then
	ora	ctlast+2
	beq	cl4c
	move4 ctlast,r0	  *ctlast = application_ID
	ldy	#disp_user_id
	lda	application_id
	sta	[r0],Y
cl4c	anop		endif
	lda	stack_size
	ora	stack_addr	if there is no stack assigned then
	bne	cl5
	ph4	#0	  get one
	ph4	#defaultStackSize
	ph2	application_id
	ph2	#$C105
	ph4	#0
	_NewHandle
	pl4	r0
	jcs	err
	lda	[r0]
	sta	stack_addr
	lda	#defaultStackSize
	sta	stack_size
cl5	OSGet_Level levelRec	inc ProDOS nest level
	inc	level
	OSSet_Level levelRec
	lda	#1	in an application
	sta	reset_app
	phd		save my DP, data bank
	phb
	tsx		save my stack
	lda	stack_addr	set DP
	pha
	pld
	clc		set stack pointer
	lda	stack_addr
	adc	stack_size
	dec	a
	tcs
	sec		set stack for QUIT call
	sbc	#6
	sta	user_call_stack
	phx		save our stack
	lda	addr	set the call address
	sta	jsl+1
	lda	addr+1
	sta	jsl+2
	ldx	linePtr+2	set addr of command line
	ldy	linePtr
	lda	application_id	set ID
	php
jsl	jsl	jsl	call the program
	plp
	plx		back to my stack
	txs
	plb		restore data bank, DP
	pld
	pha
	jsl	ShutDownApp	shut down the application
	pla
	clc
	bra	rtl
;
;  Error handlers
;
oom	lda	#outOfMem	out of memory error

err	pha		save the error number
	jsr	SetNewPrefixes	reset GS/OS prefixes (if needed)
	pla		recover the error number

	sta	level	error exit
	cmp	#$46
	bne	err1
	ph4	epIn
	jsr	FileNotFound
	lda	#$FFFF
	sta	level
	bra	err2
err1	jsl	SysError
err2	ldx	entry_stack
	txs
	lda	level
	sec
;
;  Restore volitile variables
;
rtl	php		save P in r0 reg
	php
	ply
	sty	r0
	sta	r2	save error number in r2
	pl2	user_call_stack	restore volitile variables
	pl2	entry_stack
	pl2	reset_app
	pl4	ctlast
	pl2	application_id
	lda	allocated	if allocated then
	beq	rtl1
	free	linePtr	  free(linePtr)
rtl1	free	prefixPtr	free(prefixPtr)
	free	epPathName	free(epPathname)
         lda	#1	turn pauses back on
	sta	waitFlag
	lda	r2	restore the error number
	ldy	r0	restore P
	phy
	plp
	plp
	rts
;
;  Call a SYS16 File
;
sys16	entry		enter & set the path address
	lda	epPathname
	ldx	epPathname+2
sys16_2	entry
	sta	qtPathname
	stx	qtPathname+2

	jsr	ShutDown	shut down restartable applications
	ph2	user_id	get rid of all of our stuff
	_DisposeAll
	php		disable interrupts
	sei
	lda	ProDOS	reset ProDOS hook
	sta	>pro_entry
	lda	ProDOS+2
	sta	>pro_entry+2
	lda	StackProDOS	reset ProDOS stack hook
	sta	>pro_stack_entry
	lda	StackProDOS+2
	sta	>pro_stack_entry+2
	plp		enable interrupts
	OSQuit qtRec	do it
;
;  ShutDownApp - shut down the application
;
ShutDownApp entry

	phb
	phk
	plb
	pha		make space for User ID
	ph2	application_id	push user ID to shut down
	lda	ctlast	if application is restartable then
	ora	ctlast+2
	beq	lb1
	move4	ctlast,r0
	ldy	#disp_restart
	lda	[r0],Y
	beq	lb1
	ph2	#$C000	  push $C000
	bra	lb2	else
lb1	ph2	#0	  push 0
lb2	anop		endif
	_UserShutDown	shut down the application
	bcc	lb3
	jsl	SysError
lb3	pla		dump the user ID
	ldx	#1	set the purge level to 1
	phx
	pha
	_SetPurgeAll
	OSClose clRec	close all open files
	dec	level	reset ProDOS nest level
	OSSet_Level levelRec
	jsr	SetNewPrefixes	use new prefixes
	plb
	rtl
;
;  Local data
;
allocated ds	2	was linePtr allocated dynamically?
fileType ds	2	type of the file to execute
auxType	ds	2	aux type (lsw only) of the file
host	dc	c'BYTEWRKS',i'0'	shell identifier
linePtr	ds	4	pointer to the command line buffer

prRec	dc	i'2'	OSSet_Prefix record
	dc	i'9'
prefixPtr ds	4	pointer to the prefix

epRec	dc	i'3'	ExpandPath record
epIn	ds	4
epPathname ds	4
	dc	i'0'

clRec	dc	i'1,0'	OSClose for closing all files

levelRec dc	i'1'	OSGet_Level, OSSet_Level record
level	ds	2	purge level

qtRec	dc	i'2'	quit DCB to call another application
qtPathname ds	4
	dc	i'$8000'
	end

****************************************************************
*
*  CallLanguage - call a language processor
*
*  Inputs:
*	SOURCE - source file to process
*
*  Outputs:
*	C - set if an error occurred during load
*
****************************************************************
*
CallLanguage private
	debug CallLanguage
	using Common
;
;  Handle automatic keep
;
	lda	li_oaddr	if keep name exists then
	ora	li_oaddr+2
	beq	kp1
	jsr	ExpandKeep	  expand possible $ %
	lda	li_keep
	bne	kp2
	inc	li_keep
	bra	kp2
kp1	lda	li_saddr	else if ExpandKeepName(Source,Output) then
	ldx	li_saddr+2
	jsr	ExpandKeepName
	bcc	kp2
	sta	li_oaddr
	stx	li_oaddr+2
	lm	li_keep,#1	  LI_KEEP = 1
kp2	anop
	lda	multiple
	beq	kp2a
	lda	expkeep
	beq	kp2a
	puts	#'Explicit keep not allowed on multiple compile',cr=t,errout=t
	sec
	rts
!			endif
;
;  Get the language number for the processor
;
kp2a	move4 li_saddr,giPathname	get the file's info
	OSGet_File_Info giRec
	jcs	err1
	lda	giFileType	error if not TXT or SRC
	cmp	#SRC
	beq	gl1
	cmp	#TXT
	jne	err2
	lda	#1	TXT files use language number 1
	bra	fn1
gl1	lda	giAuxType	SRC files have language number in AUX
;
;  Find the name of the language for that number
;
fn1	ora	#$8000	turn it into a language number
	pha
	jsr	FindCommand2	get language pointer
	pla
	jcc	err3	branch if language does not exist
	jsr	InitParms	set up language dependent parameters
;
;  Call the language processor
;
	short M	form the language name
	lda	#'1'	  set the prefix info
	sta	fname+2
	lda	#'6'
	sta	fname+3
	lda	#':'
	sta	fname+4
	ldx	#3	  X is number of characters
	ldy	#1	  Y is disp into name string
	lda	[r0]	  R4 is number of characters in name
	sta	r4
cl1	lda	[r0],Y	  move the name into FNAME
	iny
	inx
	sta	fname+1,X
	dbne	r4,cl1
	long	M
	txa		  set the name length
	sta	fname
	ph4	#fname	make sure the language exists
	jsr	GetFileType
	inc	A
	jeq	err4
	lla	r0,fname	call the language
	stz	r4
	stz	r6
	jsr	Call
	lda	li_oaddr	if an object file was produced then
	ora	li_oaddr+2
	beq	cl2
	lda	li_ops	  and we're done compiling then
	lsr	A
	bcs	cl2
	move4 li_oaddr,r0	    AppendObj(li_oaddr);
	jsr	AppendObj
	stz	li_oaddr	    li_oaddr = nil
	stz	li_oaddr+2
cl2	clc
	rts
;
;  Errors
;
err1	puts	#'Could not find ',errout=t
	ph4	li_saddr
	ph2	#$4000
	jsl	PrintOSName
	putcr errout=t
	sec
	rts

err2	puts	#'The input file must be SRC or TXT',cr=t,errout=t
	sec
	rts

err3	and	#$7FFF
	sta	r0
	puts	#'No language exists for language number ',errout=t
	put2	r0,cr=t,errout=t
	sec
	rts

err4	puts	#'The language ',errout=t
	ph4	#fname
	ph2	#$4000
	jsl	PrintOSName
	puts	#' does not exist',cr=t,errout=t
	sec
	rts
;
;  Local data
;
fname	ds	20	file name buffer

giRec	dc	i'4'	OSGet_File_Info record
giPathName ds	4
	ds	2
giFileType ds	2
giAuxType ds	4
	end

****************************************************************
*
*  CallLinker - call the link editor
*
*  Inputs:
*	li_saddr - pointer to file name to link
*
*  Outputs:
*	C - set if an error occurred during load
*
****************************************************************
*
CallLinker private
	debug CallLinker
	using Common
;
;  Call the linker
;
	lda	li_oaddr	if there is no output name then
	ora	li_oaddr+2
	bne	lb4
	lla	r12,linkName	  li_oaddr = linkname variable
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb1
	ph4	r8
	jsl	DuplicateOSString
	sta	li_oaddr
	stx	li_oaddr+2
	ora	li_oaddr+2
	bne	lb4
lb1	lda	li_keep	  if the {LinkName} expansion failed
	and	#$00FF
	beq	lb5	    and li_keep <> 0 then
	ph4	li_saddr	      li_oaddr =
	jsl	DuplicateOSString		DuplicateOSString(li_saddr)
	sta	li_oaddr
	sta	r0
	stx	li_oaddr+2
	stx	r2
	ora	r2
	beq	lb5
	ldy	#2	      truncate at first whitespace
	lda	[r0]
	beq	lb3
	tax
lb2	lda	[r0],Y
	and	#$00FF
	jsl	IsWhite
	bcs	lb3
	iny
	dex
	bne	lb2
lb3	dey
	dey
	tya
	sta	[r0]
lb4	jsr	ExpandKeep	expand keep name
lb5	short M	li_keep := li_oaddr <> nil
	stz	li_keep
	long	M
	lda	li_oaddr
	ora	li_oaddr+2
	beq	lb6
	inc	li_keep
lb6	lda	#12	last command was link for restart
	jsr	FindCommand2
	ph4	#linker	if the system linker exists, call it
	jsr	GetFileType
	inc	A
	beq	lb7
	lla	r0,linker
	bra	lb8

lb7	ph4	#linked	else call LINKED
	jsr	GetFileType
	inc	A
	beq	err
	lla	r0,linked
lb8	stz	r4
	stz	r6
	jsr	Call
	rts
;
;  Errors
;
err	puts	#'Could not find the linker',cr=t,errout=t
	sec
	rts
	end

****************************************************************
*
*  CallScript - Execute an EXEC file
*
*  Inputs:
*	GI_DCB - DCB from get file info on the file
*	R0 - pointer to the name of the file (as a string)
*	EXECUTE - Doing an EXECUTE command?
*
****************************************************************
*
CallScript private
	debug CallScript
	using Common
	using RecCom
exec_type equ	6	EXEC language number
;
;  Initialization
;
	move4 r0,opPathname	read the file into a buffer
	jsr	PushScript	push the exec record
	OSOpen opRec
	bcc	ex1a
	jsl	SysError
	rts
ex1a	jsr	LineAddr	get a pointer to the command line
	sta	linePtr
	stx	linePtr+2
	lda	opRefnum	set reference numbers
	sta	gtRefnum
	sta	clRefnum
	sta	rdRefnum
	OSGet_EOF gtRec	see how big the file is
	jcs	err
	clc		reserve memory for it
	lda	gtEOF
	sta	rdRequestCount
	adc	#2
	sta	gtEOF
	lda	gtEOF+2
	sta	rdRequestCount+2
	adc	#0
	sta	gtEOF+2
	mlalloc gtEOF
	sta	execCommand
	sta	ex_ptr
	stx	execCommand+2
	stx	ex_ptr+2
	ora	execCommand+2
	bne	ex2
	lda	#outofmem
	brl	err
ex2	move4 execCommand,rdDatabuffer
	OSRead rdRec	read the EXEC file
	jcs	err
	OSClose clRec	close it
	add4	rdDatabuffer,rdTransfer,r8 place an end of file marker
	lda	#0
	sta	[r8]
;
;  Create new tables
;
	lda	executeCmd	if !EXECUTE then
	bne	pr0
	jsr	PushAlias	  create a local alias table
	PushVariables 0	  create a local variable table
;
;  Set the variables parameter
;
pr0	move4 linePtr,r4	get addr of command line
pr1	lda	[r4]	skip blanks
	and	#$00FF
	jsl	IsWhite
	bcc	pr2
	inc4	r4
	bra	pr1
pr2	lda	[r4]	skip command name
	and	#$00FF
	beq	pr4
	jsl	IsWhite
	bcs	pr3
	inc4	r4
	bra	pr2
pr3	lda	[r4]	skip blanks
	and	#$00FF
	jsl	IsWhite
	bcc	pr4
	inc4	r4
	bra	pr3
pr4	lla	svName,parameters	set the parameters variable
	ph4	r4
	jsl	CtoOSString
	sta	svValue
	stx	svValue+2
	stz	svExport
	OSSet svRec
	free	svValue
;
;  Set the numbered parameter shell variables
;
pr5	lda	#$FFFF
	sta	num_parms
	ph4	linePtr
lb1	pl4	r0	  skip blanks in command line
lb2	lda	[r0]
	and	#$00FF
	jsl	IsWhite
	bcc	lb3
	inc4	r0
	bra	lb2
lb3	lda	[r0]	  quit if at end of string
	and	#$00FF
	jeq	lb6
	short M	  get the length of the line
	ldy	#0
ln1	lda	[r0],Y
	beq	ln2
	iny
	bra	ln1
ln2	long	M	  reserve space for the shell variable
	tya		   value
	inc	A
	inc	A
	jsl	Malloc
	sta	svValue
	sta	r4
	stx	svValue+2
	stx	r6
	ora	r6
	bne	ln3
	lda	#outOfMem
	brl	err
ln3	ldy	#2	  place string in ^r4
	lda	[r0]	  ... branch if its quoted
	and	#$00FF
	cmp	#'"'
	beq	lb4a
lb4	lda	[r0]	  ... read unquoted string
	and	#$00FF
	beq	lb5
	jsl	IsWhite
	bcs	lb5
	sta	[r4],Y
	inc4	r0
	iny
	bra	lb4
lb4a	inc4	r0	  ... read quoted string
	lda	[r0]
	and	#$00FF
	beq	lb5
	sta	[r4],Y
	iny
	cmp	#'"'
	bne	lb4a
	inc4	r0
	lda	[r0]
	and	#$00FF
	cmp	#'"'
	beq	lb4a
	lda	#0
	dey
	sta	[r4],Y

lb5	dey		  set the variable length
	dey
	tya
	sta	[r4]
	ph4	r0	  save pointer
	inc	num_parms	  update parameter number
	ph2	num_parms	  get the string value of number
	ph4	#numbered_name
	jsl	IntToOSString
	lla	svName,numbered_name	  set the variable
	stz	svExport
	OSSet svRec
	free	svValue
	brl	lb1	next parameter
;
;  Set the number of parameters (# variable)
;
lb6	ph2	num_parms	format the number
	ph4	#numbered_name
	jsl	IntToOSString
	lla	svName,pound	set number of parameters
	lla	svValue,numbered_name
	stz	svExport
	OSSet svRec
;
;  Set the exit variable
;
	lla	svName,exitStr	set EXIT to true
	lla	svValue,trueStr
	stz	svExport
	OSSet svRec
;
;  Execute the file
;
	ph2	executeCmd	save EXECUTE value
	lda	executeCmd	set up EXEC_FLAG
	lsr	a
	lda	#0
	ror	a
	sta	execFlag
	inc	exec_level
	Execute execRec	execute the file
	dec	exec_level
	sta	retVal	save return value
	pl2	executeCmd	restore it
	lda	executeCmd	if !EXECUTE then
	bne	lb7
	jsr	PopAlias	  pop local alias table
	PopVariables 0	  pop local variable table
lb7	jsr	PopScript	release the buffer
	free	linePtr	free(linePtr)
	lda	retVal	set return value
	rts

err	pha		save the error code
	OSClose clRec	close the file
	free	linePtr	free(linePtr)
	pla
	jsl	SysError	write out the error
	rts
;
;  Local variables
;
opRec		dc    i'3'	OSOpen record
opRefnum		ds    2
opPathname	ds    4
		dc    i'$0001'

clRec		dc    i'1'	OSClose record
clRefnum		ds    2

rdRec		dc    i'4'	OSRead record
rdRefnum		ds    2
rdDatabuffer	ds    4
rdRequestCount ds    4
rdTransfer	ds    4

gtRec		dc    i'2'	OSGet_EOF
gtRefnum		ds    2
gtEOF		ds    4

svRec		dc    i'3'	OSSet record
svName		ds    4
svValue		ds    4
svExport		ds    2

execRec		anop	execute DCB
execFlag		ds    2	flags
execCommand	ds    4	address of commands

linePtr		ds    4	line pointer
numbered_name	ds    8	numbered name parameter
num_parms 	ds    2	counter for number of parameters
retVal		ds    2	returned value
	end

****************************************************************
*
*  CommandError - handle a command error
*
*  Inputs:
*	A - error number
*
*  Outputs:
*	A - unchanged
*
*  Notes:
*	No error is printed if A is 0 or -1.
*
****************************************************************
*
CommandError private
	debug CommandError

	pha
	tax
	beq	ce1
	inc	A
	beq	ce1
	dec	A
	jsl	SysError
ce1	pla
	rts
	end

****************************************************************
*
*  Compile - Compile, link or execute a program
*
*  Inputs:
*	LI_DCB - language interface information
*
****************************************************************
*
Compile	private
	debug Compile
	using Common

	lda	li_ops	if first time in and doing a link but
	lsr	A	 not a compile, go do it.
	bcs	cp0
	lsr	A
	bcs	cp3
cp0	jsr	MultiNameInit	init for multiple name scan
cp1	short M
	lda	li_merrf	if MERRF > MERR then
	cmp	li_merr
	long	M
	ble	cp2
	and	#$0080	  if MERRF & $80 then
	beq	err
	lda	#9	    last command was edit for restart
	jsr	FindCommand2
	lla	r0,Editor	    enter editor
	lla	r4,0
	jsr	Call
err	lda	#$FFFF	  error return to EXEC
	rts		endif

cp2	jsr	NextName	get next name to compile
	lda	li_ops	if LI_OPS & 1 then
	lsr	A
	bcc	cp3
cp2a	jsr	CallLanguage	  call the language
	bcs	err
	lda	li_ops	  check for a call to itself in SMALLC
	lsr	A
	bcs	cp2a
	bra	cp1

cp3	lda	li_ops	else if LI_OPS & 2 then
	and	#$0002
	beq	cp4
cp3a	ph4	#nameListPtr	while PopName do
	jsr	PopName
	sta	r0
	stx	r2
	ora	r2
	beq	cp3b
	jsr	AppendObj	  AppendObj(r0)
	bra	cp3a
cp3b	move4 objPtr,li_saddr	li_saddr = objPtr
	jsr	CallLinker	  call the linker
	bcc	cp1
	bra	err

cp4	lda	li_ops	else if LI_OPS & 4
	and	#$0004
	beq	cp5
	stz	ctlast	  no command for restart
	stz	ctlast+2
	move4 li_saddr,r0	  call the application
	lla	r4,0
	jsr	Call
cp5	lda	#0	else return 0
	rts
	end

****************************************************************
*
*  DiskCopy - do a block by block copy of a disk
*
*  Inputs:
*	LINE - possible list of volume names
*
****************************************************************
*
DiskCopy private
	debug DiskCopy
	using Common
block_count equ buff1	number of blocks left to copy
buff_hand equ	buff1+2	handle of copy buffer
loop_count equ buff1+6	# of blocks to copy in current loop
loop_size equ	buff1+8	# blocks to copy in one loop
buff_ptr equ	r0	pointer to copy buffer
;
;  Get the device numbers
;
	stz	err	assume there will be no error
	stz	destName	no destination name, yet
	stz	destName+2
	stz	rdBuffer	no buffer allocated, yet
	stz	rdBuffer+2
	jsr	GetToken	get the source disk name
	sta	r0
	sta	sourceName
	stx	r2
	stx	sourceName+2
	lda	[r0]
	bne	lb1
	free	r0	Free(r0)
	puts	#'Source disk: '	none given -- ask for one
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	sourceName
	stx	r2
	stx	sourceName+2
	lda	[r0]
	jeq	rts
lb1	jsr	GetToken	get the destination disk name
	sta	r0
	sta	destName
	stx	r2
	stx	destName+2
	lda	[r0]
	bne	lb2
	free	r0	Free(r0)
	puts	#'Destination disk: '	none given -- ask for one
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	destName
	stx	r2
	stx	destName+2
	lda	[r0]
	jeq	rts
lb2	move4 sourceName,gdDevName
	OSGet_Dev_Number gdRec
	sta	err
	jcs	rts
	lda	gdDevNum
	sta	rdDevNum
	move4 destName,gdDevName
	OSGet_Dev_Number gdRec
	sta	err
	jcs	rts
	lda	gdDevNum
	sta	wdDevNum
	cmp	rdDevNum	volumes must be different
	bne	ms1
	puts	#'Volumes must be different',cr=t,errout=t
	dec	err
	brl	rts
;
;  Make sure volume sizes match
;
ms1	lda	rdDevNum	get the size/name of the input device
	sta	riDevNum
	OSD_Info riRec
	sta	err
	jcs	rts
	OSVolume vlRec	get the block size of the input device
	sta	err
	jcs	rts
	lda	vlBlockSize
	sta	rdBlockSize
	lda	wdDevNum	get the size/name of the output device
	sta	wiDevNum
	OSD_Info wiRec
	sta	err
	jcs	rts
	lda	riTotalBlocks
	cmp	wiTotalBlocks
	bne	ms2
	lda	riTotalBlocks+2
	cmp	wiTotalBlocks+2
	beq	ms3
ms2	puts	#'Volumes must be the same size',cr=t,errout=t
	dec	err
	brl	rts
ms3	OSVolume vlRec	get the block size of the output device
	sta	err
	jcs	rts
	lda	vlBlockSize
	sta	wdBlockSize
	cmp	rdBlockSize	check for matching volume sizes
	beq	bs1
	puts	#'The devices must use the same block size',cr=t,errout=t
	dec	err
	brl	rts
;
;  Allocate a copy buffer
;
bs1	lda	wdBlockSize	reserve a buffer
	sta	rdRequestCount
	stz	rdRequestCount+2
	mul4	rdRequestCount,riTotalBlocks
	mlalloc rdRequestCount
	sta	rdBuffer
	sta	wdBuffer
	stx	rdBuffer+2
	stx	wdBuffer+2
	ora	rdBuffer+2
	bne	bs2
	lsr	riTotalBlocks+2	unsuccessful -- halve the block count
	ror	riTotalBlocks	 and try again
	lda	riTotalBlocks
	ora	riTotalBlocks+2
	bne	bs1
	puts	#'Insufficient memory',cr=t,errout=t
	dec	err
	brl	rts

bs2	anop
;
;  Copy the disk
;
	stz	rdStartingBlock	start at block 0
	stz	rdStartingBlock+2
	stz	wdStartingBlock
	stz	wdStartingBlock+2
	move4 rdRequestCount,wdRequestCount write what we read

cd1	OSD_Read rdRec	read blocks from the source device
	sta	err
	bcs	rts
	OSD_Write wdRec	write blocks to the dest device
	sta	err
	bcs	rts
	add4	rdStartingBlock,riTotalBlocks update the # of blocks copied
	add4	wdStartingBlock,riTotalBlocks
	cmp4	rdStartingBlock,wiTotalBlocks loop if there are more
	blt	cd1
;
;  Shut down and return
;
rts	free	r0	Free the input name buffer
	free	r4	Free the output name buffer
	free	rdBuffer	Free the read/write buffer
	lda	err	if there was an error then
	beq	rts2
	inc	A	  if it has not been reported then
	beq	rts2
	dec	A	    report the error
	jsl	SysError
	lda	err
rts2	rts
;
;  Local data
;
blocksCopied ds 4	number of blocks copied so far
destName ds	4	ptr to dource device name
err	ds	2	error number
sourceName ds	4	ptr to dource device name

gdRec	dc	i'2'	OSGetDevNumber record
gdDevName ds	4
gdDevNum ds	2

riRec	dc	i'4'	OSDInfo record for input device
riDevNum ds	2
	dc	a4'volumeName'
	ds	2
riTotalBlocks ds 4

wiRec	dc	i'4'	OSDInfo record for output device
wiDevNum ds	2
	dc	a4'volumeName'
	ds	2
wiTotalBlocks ds 4

vlRec	dc	i'6'	OSVolume record
	dc	a4'volumeName+2'
	dc	a4'volumeName'
	ds	4
	ds	4
	ds	2
vlBlockSize ds 2

rdRec	dc	i'6'	OSD_Read record
rdDevNum ds	2
rdBuffer ds	4
rdRequestCount ds 4
rdStartingBlock ds 4
rdBlockSize ds 2
rdTransferCount ds 4

wdRec	dc	i'6'	OSD_Write record
wdDevNum ds	2
wdBuffer ds	4
wdRequestCount ds 4
wdStartingBlock ds 4
wdBlockSize ds 2
wdTransferCount ds 4
	end

****************************************************************
*
*  EOLCheck - Checks to make sure there is not excess garbage at
*	   the end of the line
*  Inputs:
*	LINE - command line with command, flags removed
*
*  Outputs:
*	C - set if an error occurred - error message is printed
*
****************************************************************
*
EOLCheck private
	debug EOLCheck
	using Common

	jsr	SkipBlanks	skip blanks
	jsr	GetChar
	cmp	#RETURN
	beq	ok
	puts	#'Unknown parameters on line',cr=t,errout=t
	sec
	rts

ok	clc
	rts
	end

****************************************************************
*
*  EraseDisk - Erase or format a disk
*
*  Inputs:
*	A - 0 for Erase, 1 for Format
*	line - command line
*
*  Outputs:
*	A - error flag
*
****************************************************************
*
EraseDisk private
	debug EraseDisk
	using Common

minus_flags equ r0	minus flags mask
;
;  read the command line flags
;
	sta	format	save the format flag
	jsr	RemoveCommand	remove the command
	flags ,(C)	get command line flags
	bcc	lb1
	lda	#$FFFF
	rts
;
;  Read the name of the device.  Convert the name to a device number.
;
lb1	stz	error	no error so far
	lda	minus_flags+2	set the check flag
	and	#^set_c
	sta	check
	jsr	GetToken	get the name of the device
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	bne	lb2
	puts	#'Device name: '
	jsr	GetLine
	jsr	GetToken
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	jeq	rts
lb2	move4 buff,GDNPathname	convert the name to a device number
	OSGet_Dev_Number GDN
	sta	error
	jcs	rts
	lda	dev_num	convert the number back to a device name
	sta	Ddev_num
	OSD_Info DI
	sta	error
	jcs	rts
;
;  Set the file system ID
;
	lda	#1	assume ProDOS format
	sta	file_sys_id
	free	buff	free(buff)
	jsr	GetToken	if there is a token then
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]
	beq	vn1
	ldy	#2	  if it is a number then
	lda	[r0],Y
	and	#$00FF
	cmp	#'0'
	blt	vn1
	cmp	#'9'+1
	bge	vn1
	jsr	ReadNumber	    convert it
	bcc	rn1
	puts	#'Bad file system ID number.',cr=t
err1	lda	#$FFFF
	rts

rn1	sta	file_sys_id
	free	buff	    free the token
	jsr	GetToken	    read the next token
	sta	buff
	sta	r0
	stx	buff+2
	stx	r2
	ora	r2
	jeq	oom
;
;  Set the initial volume name
;
vn1	move4 buff,r0	if no name is provided then
	lda	[r0]
	bne	vn2
	lla	vol_name,bs	  use "BLANK"
	bra	vn3	else
vn2	move4 r0,vol_name	  use the token
vn3	jsr	EOLCheck	check for garbage at end of command
	bcs	err1
;
;  Format/erase the disk
;
	lda	check	if there is a formatted volume in the
	jne	er1	  drive, make sure they want to erase
	lda	#line_len-1	  it
	sta	buff1
	OSVolume VL
	bcc	os2
	cmp	#$52
	beq	os0
	cmp	#$27
	bne	os0a
os0	lda	#1
	sta	format
	brl	er1
os0a	sta	error
	brl	rts

os1	move	otherFST,buff1,#otherFSTe-otherFST
os2	lda	format
	beq	fm1
	puts	#'Reformat '
	bra	fm2
fm1	puts	#'Erase '
fm2	ph4	#buff1
	ph2	#$8000
	jsl	PrintOSName
	puts	#'? '
	jsr	YesOrNo
	cmp	#'Y'
	jne	rts
er1	lda	format	decide what to do...
	beq	er2
	OSFormat ED	format the disk
	bra	er3
er2	OSErase_Disk ED	erase the disk
er3	bcc	er4	if error then
	ldx	ED	  if pCount = 5 then
	cpx	#5
	bne	er4
	dec	ED	    pCount = 4
	bra	er1	    try again
er4	sta	error

rts	free	buff	free(buff)
	lda	error	handle errors
	brl	CommandError
;
;  Out of memory error
;
oom	lda	#outOfMem
	sta	error
	brl	rts
;
;  Local data
;
otherFST dc	2i'l:FSTmsg'
FSTmsg	dc	c'as a ProDOS disk'
otherFSTe anop

DI	dc	i'2'	D_Info call block
Ddev_num ds	2
	dc	a4'dev_name'

GDN	dc	i'2'	Get_Dev_Number call block
GDNPathname ds 4
dev_num	ds	2

VL	dc	i'2'	Volume call block
	dc	a4'dev_name+2'
	dc	a4'buff1'

ED	dc	i'5'	OSErase_Disk call block
	dc	a4'dev_name+2'
vol_name ds	4
	ds	2
file_sys_id ds 2
	dc	i'$2000'

RN	dc	i'2'	OSChange_Path call block
	dc	a4'dev_name+2'
new_path ds	4

check	ds	2	check before erase?
dev_name dc	i'31,0',31c' '

buff	ds	4	token buffer pointer
error	ds	2	error number
format	ds	2	format (or erase) the disk?
	end

****************************************************************
*
*  ExpandKeep - Keep name expansion
*
*  If the variable keepPtr^ is present, a keep name is created
*
*  Inputs:
*	li_oaddr - output file name
*	li_saddr - source file name
*
*  Outputs:
*	li_oaddr - name expanded for possible % $
*	C - set if name created, else clear
*
****************************************************************
*
ExpandKeep private
	debug ExpandKeep
	using Common

	move4 li_saddr,r0	expand the keep name
	move4 li_oaddr,r4
	jsr	ExpandSpecial
	sta	r0
	stx	r2
	free	li_oaddr
	move4 r0,li_oaddr
	rts
	end

****************************************************************
*
*  ExpandKeepName - Keep name expansion
*
*  If the variable {KeepName} is present, a keep name is created
*
*  Inputs:
*	X-A - address of source file name
*
*  Outputs:
*	C set for success, clear for error or no name
*	X-A - address of keep file name
*
****************************************************************
*
ExpandKeepName private
	debug ExpandKeepName
	using Common

	phx		save the input name
	pha
	lla	r12,keepName	read the keep string
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb1
	move4 r8,r4	expand the special characters
	pl4	r0
	jsr	ExpandSpecial
	bcc	lb2
	rts

lb1	pla		remove the input name ptr from the stack
	pla
lb2	stz	expkeep	explicit keep = false
	clc		no keep name formed
	rts
	end

****************************************************************
*
*  ExpandSpecial - Expand the special characters in a keep name
*
*  If the variable {KeepName} is present, a keep name is created
*
*  Inputs:
*	r0 - address of source file name
*	r4 - address of the keep name
*
*  Outputs:
*	C set for success, clear for error
*	X-A - address of keep file name
*	expKeep - was the keep name explicit?
*
****************************************************************
*
ExpandSpecial private
	debug ExpandSpecial
	using Common
;
;  Set up the various name buffers
;
	move4 r0,source	save the input name
	move4 r4,keepNameVar
	stz	keep	keep = nil
	stz	keep+2
	stz	directory	directory = nil
	stz	directory+2
	stz	fnames	fnames = nil
	stz	fnames+2
	stz	fname	fname = nil
	stz	fname+2
	lda	#1	expKeep = true
	sta	expKeep

	ph4	source	get the directory part of the name
	jsl	DuplicateOSString	duplicate the full path name
	sta	directory
	sta	r0
	stx	directory+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]	remove the path name part
	tay
	iny
	ldx	#0
	short M
lb1	lda	[r0],Y
	cmp	#':'
	beq	lb2
	inx
	dey
	bne	lb1
lb2	long	M
	dey
	tya
	sta	[r0]

	iny		get space for the file name part
	sty	r4
	stx	r6
	inx
	inx
	txa
	jsl	Malloc
	sta	fname
	sta	r10
	stx	fname+2
	stx	r12
	ora	r12
	jeq	oom
	lda	#2	create the file name part
	sta	r8
	lda	r6
	sta	[r10]
	tax
	short M
lb3	ldy	r4
	iny
	sty	r4
	lda	[r0],Y
	ldy	r8
	sta	[r10],Y
	iny
	sty	r8
	dex
	bne	lb3
	long	M

	ph4	fname	create a name with the extension removed
	jsl	DuplicateOSString	duplicate the file name
	sta	fnames
	sta	r0
	stx	fnames+2
	stx	r2
	ora	r2
	jeq	oom
	lda	[r0]	remove the extension
	tay
	iny
	ldx	#0
	short M
lb4	lda	[r0],Y
	cmp	#'.'
	beq	lb5
	inx
	dey
	cpy	#1
	bne	lb4
	long	M
	bra	lb6
lb5	long	M
	dey
	dey
	tya
	sta	[r0]
lb6	anop
;
;  Form the new keep name
;
	malloc #2	start with an empty keep name
	sta	keep
	sta	r0
	stx	keep+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#0
	sta	[r0]
	move4 keepNameVar,r0	r4 = # chars in source name
	lda	[r0]
	sta	r4
	add4	r0,#2	r0 = addr of first character

fk1	lda	r4	while there are characters left do
	beq	rt1
	lda	[r0]	if r0^ = '$' then
	and	#$00FF
	cmp	#'$'
	bne	fk2
	stz	expkeep	  explicit keep = false
	ph4	keep	  keep = concat(keep,fnames)
	ph4	fnames
	bra	fk5	  skip the $ character & loop

fk2	cmp	#'%'	else if r0^ = '%' then
	bne	fk4
	stz	expkeep	  explicit keep = false
	ph4	keep	  keep = concat(keep,fname)
	ph4	fname
	bra	fk5	  skip the % character & loop

fk4	sta	ch	else
	ph4	keep	  keep = concat(keep,char)
	ph4	#chRec
fk5	ph2	#0
	jsl	AppendOSNames
	sta	r8
	stx	r10
	ora	r10
	beq	oom
	free	keep
	move4 r8,keep
	inc4	r0	  skip the character
	dec	r4
	bra	fk1	  loop
;
;  Return with a keep name found
;
rt1	sec		success flag
rt2	php
	free	directory	dispose of dynamic areas
	free	fnames
	free	fname
	lda	keep	return the keep name
	ldx	keep+2
	plp		return success flag
	rts
;
;  Out of memory error
;
oom	clc
	bra	rt2
;
;  Local data
;
chRec	dc	i'1'	OS string for one character
ch	ds	2	the character in the OS string

keep	ds	4	keep name pointer
keepNameVar ds 4	pointer to the keep name variable
source	ds	4	source name pointer
directory ds	4	directory part of source name
fnames	ds	4	file name sans extension
fname	ds	4	file name
	end

****************************************************************
*
*  FileCopy - Copy or move disk files
*
*  Inputs:
*	A - 1 for copy, 0 for move
*
****************************************************************
*
FileCopy private
	using Common
	debug FileCopy
minus_flags equ r0	minus flags mask
plus_flags equ r4	plus flags mask

DIR	equ	$0F	directory file type
;
;  Initialization
;
	sta	doCopy	set the doCopy flag
	lda	minus_flags+2	check = !<-c flag set>
	and	#^set_c
	eor	#^set_c
	sta	check
	lda	minus_flags+2	dataFork = <-f flag set>
	and	#^set_f
	sta	dataFork
	lda	minus_flags+2	noprogress = <-p flag set>
	and	#^set_p
	sta	noprogress
	lda	minus_flags	resourceFork = <-r flag set>
	and	#set_r
	sta	resourceFork
	lda	dataFork	if dataFork and resourceFork then
	beq	in1
	lda	resourceFork
	beq	in1
	stz	resourceFork	  clear both flags
	stz	dataFork
in1	stz	sessionStarted	no session started
	stz	err	no error found
	stz	inName	no input name
	stz	inName+2
	stz	destName	no destination directory name
	stz	destName+2
	stz	destFile	no destination file name
	stz	destFile+2
	stz	epOut	no expanded input name
	stz	epOut+2
	stz	rwDataBuffer	no data buffer
	stz	rwDataBuffer+2
	stz	dirList	no directories in list
	stz	dirList+2
	stz	dirDelList	no directories in the delete list
	stz	dirDelList+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	malloc #buffSize	allocate a copy name buffer
	sta	nxPath
	sta	r0
	stx	nxPath+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#buffSize	set the buffer length
	sta	[r0]
	malloc #optionBuffSize	allocate an option list buffer
	sta	nxOptionList
	sta	r0
	stx	nxOptionList+2
	stx	r2
	ora	r2
	jeq	oom
	lda	#optionBuffSize	set the buffer length
	sta	[r0]
;
;  Get the source file name
;
	jsr	GetToken	get source file name
	sta	r0
	sta	inName
	stx	r2
	stx	inName+2
	lda	[r0]
	bne	in2
	free	r0
	puts	#'Source file name: '
	jsr	GetLine
	jsr	GetToken
	sta	r0
	sta	inName
	stx	r2
	stx	inName+2
	lda	[r0]
	jeq	rts

in2	ph4	inName	replace slashes with colons
	jsl	SlashToColon
	lda	#1	noFiles = true
	sta	noFiles
	move4 inName,epIn	expand the source path
	ph4	#epRec
	jsr	ExpandDevice
	sta	err
	tax
	jne	rts
;
;  Form the destination path
;
	jsr	GetToken	get the destination name
	sta	r0
	sta	destName
	sta	dpPath
	stx	r2
	stx	destName+2
	stx	dpPath+2
	lda	[r0]
	jne	fd1
	free	r0	no name provided - dispose of buffer
	lla	dpPath,p0	use the default prefix

fd1	ph4	#dpRec	expand prefixes
	jsr	ExpandDevice
	sta	err
	tax
	jne	rts
	add4	dpName,#2,destName
	ph4	destName	convert slashes to colons
	jsl	SlashToColon
	move4 destName,r0	if the name is not a full pathname then
	ldy	#2
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	jeq	fd1a
	lla	dpPath,p0	  expand prefix 0
	ph4	#dpRec
	jsr	ExpandDevice
	sta	err
	tax
	jne	rts
	ph2	dpName+2	  append a ':' character
	lda	dpName
	inc	A
	inc	A
	pha
	ph4	#colon
	ph2	#0
	jsl	AppendOSNames
	sta	r0
	stx	r2
	free	dpName	  Free(dpName)
	lda	r0	  check for out of memory
	ora	r2
	jeq	oom
	ph4	r0	  append the partial name to 8:
	ph4	destName
	ph2	#0
	jsl	AppendOSNames
	sta	r4
	stx	r6
	free	r0	  Free(r0)
	lda	r4	  check for out of memory
	ora	r6
	jeq	oom
	free	destName	  Free(destName)
	move4 r4,destName	  destName = r4
fd1a	ph4	destName	outType = GetFileType(destName)
	jsr	GetFileType
	sta	outType
	cmp	#DIR	if the file is a directory then
	bne	fd2
fd1b	ldx	#1	  isDirectory = true
	bra	fd3	else
fd2	ldx	#0	  isDirectory = false
fd3	stx	isDirectory
;
;  Get the list of files to copy
;
	move4 inName,iwName	create a list of names
	OSInit_Wildcard iwRec
	sta	err
	jcs	rts
	ph2	epOut+2	if the source has wildcards then
	lda	epOut
	inc	A
	inc	A
	pha
	jsr	IsWildcard
	tax
	beq	gf4
	move4 epOut,r0	  get the length of the directory name
	ldy	#2
	lda	[r0],Y
	tax
	tay
	iny
	iny
	iny
	short M
	lda	#':'
gf2	cmp	[r0],Y
	beq	gf3
	dey
	dex
	bne	gf2
gf3	long	M
	stx	dirLength
	bra	gf8	else
gf4	add4	epOut,#2,r0	  dirLength = length(epOut)
	lda	[r0]
	sta	dirLength
	ph4	r0	  if GetFileType(epOut) = DIR
	jsr	GetFileType
	cmp	#DIR
	bne	gf5
	lda	outType	    and GetFileType(destName) = -1 then
	cmp	#-1	    {-1 implies does not exist}
	bne	gf5
	lda	#1	    isDirectory = true
	sta	isDirectory
	bra	gf8	  else
gf5	lda	isDirectory	    if isDirectory then
	beq	gf8
	lda	[r0]	      set dirLength to length(path-file)
	tay
	iny
	tax
	short M
	lda	#':'
gf6	cmp	[r0],Y
	beq	gf7
	dey
	dex
	bne	gf6
gf7	long	M
	stx	dirLength
gf8	anop		endif
;
;  Copy the files
;
	OSBegin_Session bsRec	Start the session
	inc	sessionStarted	note that we started one
cf1	OSNext_Wildcard nxRec	get the name of the file to copy
	sta	err
	jcs	rts
	move4 nxPath,r0	quit if there are no more names
	ldy	#2
	lda	[r0],Y
	jeq	cf99

	stz	noFiles	we copied one...

!			{form the destination name}
	lda	isDirectory	if not isDirectory then
	bne	cf4
	move4 destName,r0	  destFile = Malloc(length(destName)+2)
	lda	[r0]
	inc	A
	inc	A
	jsl	Malloc
	sta	r4
	sta	destFile
	stx	r6
	stx	destFile+2
	ora	r6
	bne	cf2
oom	lda	#outOfMem
	sta	err
	brl	rts
cf2	lda	[r0]	  destFile^ = destName^
	tay
	short M
	iny
cf3	lda	[r0],Y
	sta	[r4],Y
	dey
	bpl	cf3
	long	M
	brl	cf10	else
cf4	move4 nxPath,r0	  destFile = Malloc(length(nxPath)-
	move4 destName,r4	    dirLength+length(destName)+3)
	clc
	lda	[r4]
	ldy	#2
	adc	[r0],Y
	sec
	sbc	dirLength
	inc	A
	inc	A
	inc	A
	jsl	Malloc
	sta	r8
	sta	destFile
	stx	r10
	stx	destFile+2
	ora	r10
	beq	oom
	lda	[r4]	  copy over destName
	tax
	ldy	#2
	short M
cf5	lda	[r4],Y
	sta	[r8],Y
	iny
	dex
	bne	cf5
	cmp	#':'	  if the last char was not a : then
	beq	cf6
	lda	#':'	    append a :
	sta	[r8],Y
	iny		  save the length
cf6	dey
	dey
	sty	r12
	long	M
	tya
	sta	[r8]
	clc		  r14 = new save position
	adc	r8
	sta	r14
	lda	r10
	adc	#0
	sta	r16
	sec		  X = # of chars to copy from nxPath
	ldy	#2
	lda	[r0],Y
	sbc	dirLength
	tax
	lda	r0	  point r0 to the load position
	clc
	adc	#2
	adc	dirLength
	sta	r0
	bcc	cf7
	inc	r2
cf7	lda	[r0],Y	  if the first char is a : then
	and	#$00FF
	cmp	#':'
	bne	cf8
	dex		    --X
	beq	cf10
	inc4	r0	    ++r0
cf8	txa		  set the length of the name
	clc
	adc	r12
	sta	[r8]
	short M	  copy the characters
cf9	lda	[r0],Y
	sta	[r14],Y
	iny
	dex
	bne	cf9
	long	M
cf10	anop		endif {dest name formed}

	lda	noprogress	if not noprogress then
	bne	cf10a
	puts	#'   '	  {print the copy message}
	ph4	nxPath	  print the source file
	ph2	#$8000
	jsl	PrintOSName
	puts	#' --> '	  print the "copy" arrow
	ph4	destFile	  print the destination file
	ph2	#0
	jsl	PrintOSName
	putcr
cf10a	anop		endif

	lda	nxFileType	if the source is a directory then
	cmp	#DIR
	bne	cf11
	jsr	CopyDirectory	  copy the directory
	bra	cf12	else
cf11	jsr	CopyFile	  copy the file
cf12	anop		endif

	free	destFile	Free(destFile)
	stz	destFile	destFile = null
	stz	destFile+2
	jsr	Pause	Pause
	jcc	cf1	if exit not requested then loop

cf99	lda	noFiles	if no files were copied, flag the error
	beq	rts
	ph4	inName
	jsr	FileNotFound
	dec	err
;
;  Exit processing
;
rts	anop
rts0	lda	dirList	while dirList <> nil do
	ora	dirList+2
	beq	dd1
	move4 dirList,r0	  get the record pointer
	add4	r0,#20,giPathName	  get the current directory info
	OSGet_File_Info giRec
	ldx	err
	bne	rts0a
	sta	err
rts0a	bcs	dd1
	ldy	#18
rts1	lda	[r0],Y
	sta	giCreateDateTime-4,Y
	dey
	dey
	cpy	#2
	bne	rts1
	OSSet_File_Info giRec	  set the original date/time
	ldx	err
	bne	rts1a
	sta	err
rts1a	bcs	dd1
	ldy	#2
	lda	[r0]	  dirList = dirList^.next
	sta	dirList
	lda	[r0],Y
	sta	dirList+2
	free	r0	  Free(r0)
	bra	rts0	endwhile

dd1	lda	dirDelList	while dirDelList <> nil do
	ora	dirDelList+2
	beq	rts2
	move4 dirDelList,r0	  get the record pointer
	ldy	#4	  get the directory name
	lda	[r0],Y
	sta	dsPathName
	iny
	iny
	lda	[r0],Y
	sta	dsPathName+2
	ora	dsPathName
	beq	dd2
	OSDestroy dsRec	  destroy the file
	free	dsPathName	  dispose of the path name buffer
dd2	ldy	#2	  dirDelList = dirDelList^.next
	lda	[r0]
	sta	dirDelList
	lda	[r0],Y
	sta	dirDelList+2
	free	r0	  Free(r0)
	bra	dd1	endwhile

rts2	lda	sessionStarted	if sessionStarted then
	beq	rt1
	OSEnd_Session bsRec	  stop the session
rt1	free	inName	Free(inName)
	free	destName	Free(destName)
	free	destFile	Free(destFile)
	free	epOut	Free(epOut)
	free	nxPath	Free(nxPath)
	free	nxOptionList	Free(nxOptionList)
	free	rwDataBuffer	Free(rwDataBuffer)
	lda	err	handle any error
	brl	CommandError
;
;  Local data
;
check	ds	2	check before overwriting existing file?
colon	dc	i'1',c':'	colon; used for appends
dataFork ds	2	copy only the data fork?
destFile ds	4	destination file name pointer
destName ds	4	destination directory name pointer
dirLength ds	2	length of the source directory
dirDelList ds	4	list of directories to delete
dirList	ds	4	list of directory entries for SetFileInfo
doCopy	ds	2	do a copy? (or a move?)
err	ds	2	error code
inName	ds	4	input name pointer
isDirectory ds 2	is the destination a directory?
noFiles	ds	2	have we copied any files?
noprogress ds	2	suppress progress info?
outType	ds	2	file type for output path
resourceFork ds 2	copy only the resource fork?
sessionStarted ds 2	has a session been started?

bsRec	dc	i'0'	OSBegin_Session and OSEnd_Session record

dpRec	dc	i'2'	OSExpandDevices record for expanding 8/
dpPath	ds	4
dpName	ds	4
p0	dc	i'2',c'8:'

epRec	dc	i'2'	expand devices record
epIn	ds	4
epOut	ds	4

iwRec	dc	i'2'	OSInit_Wildcard record
iwName	ds	4
	dc	i'$2000'

giRec	dc	i'7'	OSGetFileInfo/OSSetFileInfo record
giPathName ds	4	path name
	ds	2	access
	ds	2	file type
	ds	4	aux type
	ds	2	storage type
giCreateDateTime ds 8	create date
	ds	8	mod date
	ds	4	option list
;...............................................................
;
;  CopyDirectory - copy a directory
;
;...............................................................
;
CopyDirectory anop

	lda	doCopy	if !docopy then
	bne	cd0
	malloc #8	  r0 = malloc(8)
	sta	r0
	stx	r2
	ora	r0	  if r0 <> nil then
	beq	cd0
	ldy	#2	    r0^.next := dirDelList
	lda	dirDelList
	sta	[r0]
	lda	dirDelList+2
	sta	[r0],Y
	move4	r0,dirDelList	    dirDelList := r0
	lda	nxPath	    dirDelList^.pathname :=
	ldx	nxPath+2	      DuplicateOSString(nxPath)
	inc	A
	inc	A
	phx
	pha
	jsl	DuplicateOSString
	ldy	dirDelList
	sty	r0
	ldy	dirDelList+2
	sty	r2
	ldy	#4
	sta	[r0],Y
	iny
	iny
	txa
	sta	[r0],Y

cd0	ph4	destFile	if the output file exists then
	jsr	GetFileType
	cmp	#-1
	beq	cd1
	cmp	#DIR	  if the file is not a directory then
	jeq	cd4
	puts	#'Cannot create the directory '
	ph4	destFile	    flag the error
	ph2	#0
	jsl	PrintOSName
	putcr
	dec	err
	brl	subErr
!			else
cd1	move4 crPathName,r0	  if the path ends in ':', remove it
	lda	[r0]
	tay
	iny
	lda	[r0],Y
	and	#$00FF
	cmp	#':'
	bne	cd1a
	lda	[r0]
	dec	A
	sta	[r0]
cd1a	move4 destFile,crPathName	  create the directory
	OSCreate crRec
	sta	err
	bcs	subErr
	move4 destFile,r0	  save the directory entry for later
	lda	[r0]	    call to SetFileInfo
	clc
	adc	#2+4+16
	jsl	Malloc
	sta	r4
	stx	r6
	ora	r6
	beq	soom
	lda	dirList
	sta	[r4]
	ldy	#2
	lda	dirList+2
	sta	[r4],Y
	ldy	#18
cd2	lda	nxCreateDateTime-4,Y
	sta	[r4],Y
	dey
	dey
	cpy	#2
	bne	cd2
	move4 r4,dirList
	add4	r4,#20
	lda	[r0]
	tay
	iny
	short M
cd3	lda	[r0],Y
	sta	[r4],Y
	dey
	bpl	cd3
	long	M
cd4	rts

soom	lda	#outOfMem	flag an out of memory error
	sta	err
subErr	pla		hande an error in the subroutine
	brl	rts

crRec	dc	i'5'	OSCreate record
crPathName ds	4
	dc	i'$C3'
	dc	i'DIR'
	dc	i4'0'
	dc	i'$000D'
;...............................................................
;
;  CopyFile - copy a file
;
;...............................................................
;
CopyFile anop

	lda	doCopy	if !docopy then
	bne	cp1
	add4	nxPath,#2,cpPathName	  try a change path
	move4 destFile,cpNewPathName
	OSChange_Path cpRec
	jcc	cp10

cp1	ph4	destFile	if destination file exists then
	jsr	GetFileType
	inc	A
	jeq	cp7
	lda	check	  if we should check before deleting then
	jeq	cp3
	lda	dataFork
	ora	resourceFork
	jne	cp3
	puts	#'    File exists - replace it? '
	jsr	YesOrNo
	cmp	#'Y'
	beq	cp3
	cmp	#'Q'
	bne	cp2
	pla
	brl	rts

cp2	puts	#'    New path name: '	get a new name for the file
	jsr	GetLine
	free	destFile
	jsr	GetToken
	sta	destFile
	sta	r0
	stx	destFile+2
	stx	r2
	ora	r2
	jeq	soom
	lda	[r0]
	jeq	cp10
	brl	cp1

cp3	add4	nxPath,#2,r4	quit if files are the same
	ph4	r4
	ph4	destFile
	jsl	CompareOSStrings
	tax
	jeq	cp10
	lda	dataFork	if not copying only one fork then
	ora	resourceFork
	bne	cp7
	move4 destFile,dsPathName	  destroy the old file
	OSDestroy dsRec

cp7	ph4	destFile	if destination file does not exist then
	jsr	GetFileType
	inc	A
	bne	cp8
	lda	nxStorageType	  set the storage type
	sta	cfStorageType
	lda	dataFork	  if we are copying only the data fork
	beq	cp7a	    then
	lda	#1	    create a standard file (not
	sta	cfStorageType	      extended)
cp7a	jsr	CreateFile	  create the file

cp8	lda	resourceFork	if not copying only the resource fork then
	bne	cp8b
	lda	#0	  copy the data fork
	jsr	Copy
cp8b	lda	nxStorageType	if the file is an extended file then
	cmp	#5
	bne	cp9
	lda	dataFork	  if not copying only the data fork then
	bne	cp9
	ph4	destFile	    if destination file exists then
	jsr	GetFileType
	inc	A
	beq	cp8c
	lda	#$8005	      change the file to an extended file
	sta	cfStorageType
	jsr	CreateFile
cp8c	lda	#1	    copy the resource fork
	jsr	Copy

cp9	move	nxPath,giPathName,#34	set dest file info
	move4 destFile,giPathName
	inc	giRec
	OSSet_File_Info giRec
	sta	err
	dec	giRec
	jcs	subErr
	lda	doCopy	if !docopy then
	bne	cp10
	add4	nxPath,#2,dsPathName	  destroy the source file
	OSDestroy dsRec
cp10	rts

cpRec	dc	i'2'	OSChangePath record
cpPathName ds	4
cpNewPathName ds 4

cfRec	dc	i'5'	OSCreate record for creating a new file
cfPathName ds	4
cfAccess	ds	2
	ds	2
	ds	4
cfStorageType ds 2
cfLen	equ	16
;...............................................................
;
;  CreateFile - create a file (or change one to an extended file)
;
;...............................................................
;
CreateFile ldy #cfLen-4	copy the existing file's info to the
cr1	lda	nxPath-2,Y	 Create record
	sta	cfRec,Y
	dey
	dey
	bne	cr1
	move4 destFile,cfPathName
	lda	#$C3	allow file modifications (for now, anyway)
	sta	cfAccess
	OSCreate cfRec	create the file
	bcc	cr1a
	cmp	#$0070	(allow a resource exists error)
	bne	cr2
cr1a	rts

cr2	sta	err	flag an error and return
	brl	subErr2
;...............................................................
;
;  Copy - copy the data or resource fork of a file
;
;  Inputs:
;	A - 1 for resource fork, 0 for data fork
;
;...............................................................
;
Copy	anop
	sta	opResourceNumber	save the resource flag
	stz	rdRefNum	zero the reference numbers
	stz	wrRefNum
	tay		set EOF to the length of the
	bne	cy1	  current fork
	move4 nxEOF,EOF
	bra	cy2
cy1	move4 nxResourceEOF,EOF
cy2	anop
	lda	EOF	quit if the file is zero length
	ora	EOF+2
	jeq	cy6

	add4	nxPath,#2,opPathName	open the source file
	OSOpen opRec
	sta	err
	jcs	subErr2
	lda	opRefNum
	sta	rdRefNum
	move4 destFile,opPathName	open the destination file
	OSOpen opRec
	sta	err
	jcs	subErr2
	lda	opRefNum
	sta	wrRefNum
	sta	efRefNum
	OSSet_EOF efRec	set the EOF to 0
	sta	err
	jcs	subErr2

	move4 EOF,rwRequestCount	find some memory
cy3	mlalloc rwRequestCount
	sta	rwDataBuffer
	stx	rwDataBuffer+2
	ora	rwDataBuffer+2
	bne	cy4
	lsr	rwRequestCount+2	try half that number of blocks
	ror	rwRequestCount
	lda	rwRequestCount
	ora	rwRequestCount+2
	bne	cy3
	lda	#outOfMem
	sta	err
	brl	subErr2

cy4	stz	r0	initialize start counter
	stz	r2

cy5	lda	rdRefNum	read from source
	sta	rwRefNum
	OSRead rwRec
	sta	err
	bcs	subErr2
	lda	wrRefNum	write to destination
	sta	rwRefNum
	move4 rwTransferCount,rwRequestCount set the number of bytes to write
	OSWrite rwRec
	sta	err
	bcs	subErr2
	add4	r0,rwTransferCount	advance counter
	cmp4	r0,EOF	are we finished
	blt	cy5

	free	rwDataBuffer	Free(rwDataBuffer)
	stz	rwDataBuffer
	stz	rwDataBuffer+2
	OSClose clRec	close the source file
	OSClose cwRec	close the destination file

cy6	rts

subErr2	lda	rdRefNum	if rdRefNum <> 0 then
	beq	se1
	OSClose clRec	  close the source file
se1	lda	wrRefNum	if wrRefNum <> 0 then
	beq	se2
	OSClose cwRec	  close the destination file
se2	lda	resourceFork	if not (resourceFork or dataFork) then
	ora	dataFork
	bne	se3
	move4 destFile,dsPathName	  destroy(destFile)
	OSDestroy dsRec
se3	pla		return from an error 2 levels deep
	pla
	brl	rts
;
;  Local data for Copy
;
EOF	ds	4	length of the current fork

opRec	dc	i'4'	OSOpen record
opRefNum ds	2
opPathName ds	4
	ds	2
opResourceNumber ds 2

efRec	dc	i'3'	OSSetEOF record (for setting eof to 0)
efRefNum ds	2
	dc	i'0'
	dc	i4'0'

dsRec	dc	i'1'	OSDestroy record
dsPathName ds	4

clRec	dc	i'1'	OSClose for the read file
rdRefNum ds	2	read refNum

cwRec	dc	i'1'	OSClose for the write file
wrRefNum ds	2	write refNum

rwRec	dc	i'4'	OSRead/OSWrite record
rwRefNum ds	2
rwDataBuffer ds 4
rwRequestCount ds 4
rwTransferCount ds 4
	end

***************************************************************
*
*  FileNotFound - Print a file not found error
*
*  Inputs:
*	ptr - pointer to the path name
*
****************************************************************
*
FileNotFound private

	sub	(4:ptr),0

	puts	#'File not found: ',errout=t
	ph4	ptr
	ph2	#$4000
	jsl	PrintOSName
	putcr errout=t

	ret
	end

****************************************************************
*
*  Flags - Read flags from the command line
*
*  Inputs:
*	LINE - command line
*	R0 - legal "-" flags mask
*	R4 - legal "+" flags mask
*
*  Outputs:
*	LINE - command line with flags removed
*	R0 - "-" flags mask
*	R4 - "+" flags mask
*	C - set if error occurred
*
****************************************************************
*
Flags	private
	debug Flags
	using Common

	ldx	#6	save legal flags mask, zero work area
fl1	lda	r0,X
	sta	minus,X
	lda	#0
	sta	r0,X
	dex
	dbpl	X,fl1
fl2	long	M	see if there is a flag on the line
	jsr	LineLength
	tax
	jeq	fl9
	short M
	ldx	#0
	jsr	NextChar
	cmp	#'-'
	beq	fl2a
	cmp	#'+'
	bne	fl9
fl2a	sta	flagsign	save the sign in case of error
	pha		yes - save its kind
	jsr	GetChar
	jsr	GetChar	get the letter
	jsl	ToUpper
	sta	flag_ch
	jsl	IsAlpha
	long	M
	bcc	err1
	and	#$00FF	turn the letter into an index
	sec
	sbc	#'A'-1
	tax		form a bit mask with it
	stz	r8
	stz	r10
	sec
fl3	ror	r10
	ror	r8
	dbne	X,fl3
	short M	see if its legal, and save it if it is
	pla
	long	M
	and	#$00FF
	cmp	#'-'
	beq	fl6
	lda	plus
	and	r8
	bne	fl5
	lda	plus+2
	and	r10
	beq	err
fl5	lda	r4
	ora	r8
	sta	r4
	lda	r6
	ora	r10
	sta	r6
	bra	fl8
fl6	lda	minus
	and	r8
	bne	fl7
	lda	minus+2
	and	r10
	beq	err
fl7	lda	r0
	ora	r8
	sta	r0
	lda	r2
	ora	r10
	sta	r2
fl8	jsr	SkipBlanks	skip blanks
	brl	fl2	next flag

fl9	long	M	good return
	clc
	rts

err1	short M
	pla
	long	M
	puts	#'Flag character not found',cr=t,errout=t
	sec
	rts

err	long	M	bad return
	lda	flagsign
	jsl	~errout
	lda	flag_ch
	jsl	~errout
	puts	#' is not a valid flag',cr=t,errout=t
	sec
	rts

minus	ds	4	plus flags
plus	ds	4	minus flags
flag_ch	ds	2	flag character for error message
flagsign ds	2	flag sign for error message
	end

****************************************************************
*
*  GetAux - Read an aux type from a token
*
*  Inputs:
*	r0 - pointer to the token
*
*  Outputs:
*	A - file type ID number
*	C - clear is successful else set
*
****************************************************************
*
GetAux	private
	debug GetAux
	using Common
;
;  See if the string is valid
;
	stz	r4	r4 = 0
	stz	r6
	lda	[r0]	X is the length of the string
	tax
	ldy	#2	split based on first character
	lda	[r0],Y
	and	#$00FF
	jsl	IsDigit
	bcs	gt4
	cmp	#'$'
	beq	hx1
err	long	M	flag an error and return
	sec
	rts
;
;  Read a hex number
;
hx1	iny		next character
	dex
	beq	rts
	lda	[r0],Y
	and	#$00FF
	jsl	IsXDigit	error if not a hex digit
	bcc	err
	jsr	ToInt	roll in the value
	asl	r4
	rol	r6
	asl	r4
	rol	r6
	asl	r4
	rol	r6
	asl	r4
	rol	r6
	ora	r4
	sta	r4
	bra	hx1
;
;  Read a decimal number
;
gt4	lda	[r0],Y	get a character
	and	#$00FF
	jsl	IsDigit	error if not a digit
	bcc	err
	pha		add in the value
	asl	r4
	rol	r6
	ph2	r6
	lda	r4
	asl	r4
	rol	r6
	asl	r4
	rol	r6
	clc
	adc	r4
	sta	r4
	pla
	adc	r6
	sta	r6
	pla
	and	#$0F
	clc
	adc	r4
	sta	r4
	bcc	gt6
	inc	r6
gt6	iny		next character
	dex
	bne	gt4

rts	lda	r4	return the value
	ldx	r6
	clc
	rts
	end

****************************************************************
*
*  GetFileName - Get the file name from the command line
*
*  Inputs:
*	LINE - command line with command, flags removed
*	C - set if the file must exist, else clear
*	X - flags word
*	   0,1 - 0 if GetFileName should not flag errors; 1
*		to flag errors as an error; 2 to flag errors
*		as a DELETE warning
*	   2 - 1 for DELETE directory prompt, 0 if not
*	A - Init_Wildcard flags
*
*  Outputs:
*	C - set if there are no more files.  In this case,
*		A is the error code.  It is 0 for no error, or
*		-1 for an error.
*	nxRec - next wildcard record; returned by the first
*		matching file
*	A -
*	   0 - no wildcard character
*	   1 - there is a wildcard character
*	   2 - there was an = or ...:= wildcard
*
*  Notes:
*	The file name is returned in a dynamically allocated
*	buffer.  It is the caller's responsibility to dispose
* 	of the buffer.  If there is an error then no buffer
*	is reserved, and nxPath is set to nil.
*
****************************************************************
*
GetFileName private
	debug GetFileName
	using Common
DIR	equ	$0F	directory file type
;
;  Initialization
;
	stz	nxPath	nxPath = nil
	stz	nxPath+2
	stz	nxOptionList	nxOptionList = nil
	stz	nxOptionList+2
	stz	buff	buff = nil
	stz	buff+2
	sta	inFlags	save flags
	lda	#0	set the must exist flag
	rol	a
	sta	must_exist
	txa		save the error flag
	and	#$0003
	sta	flagError
	txa		save the check directories flag
	and	#$0004
	sta	checkDirectories
	stz	inflag	first time call
	stz	wcflag	set wildcard flag
rn	jsr	SkipBlanks	skip blanks
	jsr	GetToken	get the file name
	sta	r0
	stx	r2
	ora	r2
	jeq	rn2a
;
;  Check for permission before deleting directories
;
	lda	checkDirectories	if checkDirectories then
	jeq	cd2
	ph4	r0	  if GetFileType(r0^) = DIR then
	jsr	GetFileType
	cmp	#DIR
	jne	cd2
	puts	#'Delete the directory '	    check before using name
	ph4	r0
	ph2	#0
	jsl	PrintOSName
	puts	#' and its contents? '
	jsr	YesOrNo
	cmp	#'Y'
	beq	cd2
	cmp	#'N'
	beq	cd1
	free	r0	    abort processing
	sec
	rts

cd1	free	r0	    skip this name
	inc	inflag	    don't prompt for a name
	brl	rn

cd2	anop
;
;  Wildcard check
;
	lda	[r0]	check the name for wildcards
	beq	rn2
	tax
	ldy	#2
	short M
rn1	lda	[r0],Y
	cmp	#'='
	bne	rn1a
	stz	wcflag	set wild card flag to 1
	inc	wcflag
rn1a	iny
	dex
	bne	rn1
	long	M
	bra	rn3
;
;  Prompted input
;
rn2	free	r0	free the file name buffer
	lda	inflag	only ask for a file name once
	jne	errExit
	lda	must_exist	don't ask if it's optional
	jeq	errExit
	puts	#'File name: '	ask for a file name
	jsr	GetLine
	inc	inflag
	brl	rn

rn2a	lda	#outOfMem	out of memory error
	jsl	SysError
	brl	errExit
;
;  Set wildcard flag
;
rn3	lda	[r0]	if the file name is = then
	tay
	iny
	lda	[r0],Y
	and	#$00FF
	cmp	#'='
	bne	wc2
	cpy	#2
	beq	wc1
	dey
	lda	[r0],Y
	and	#$00FF
	cmp	#'/'
	beq	wc1
	cmp	#':'
	bne	wc2
wc1	inc	wcFlag	  wcFlag = 2
;
;  Set up for a wildcard serach
;
wc2	move4 r0,inPathname	set up a wildcard search
	OSInit_Wildcard inRec
	malloc #buffSize	allocate a file name buffer
	sta	nxPath
	sta	r0
	stx	nxPath+2
	stx	r2
	ora	r2
	beq	rn2a
	lda	#buffSize	set the buffer length
	sta	[r0]
	OSNext_Wildcard nxRec	get the first matching name
	move4 nxPath,r0
	ldy	#2
	lda	[r0],Y
	jne	rn4
	free	r0	dispose of the buffer
	lda	must_exist	if the file must exist then
	beq	rn3a
	lda	flagError	  if we should flag errors then
	beq	errExit
	dec	A	    if error message = 1 then
	bne	em1
	ph4	inPathname	      write file not found error
	jsr	FileNotFound
	bra	em2	    else
em1	puts	#'Warning: '	      print warning message
	ph4	inPathname
	ph2	#0
	jsl	PrintOSName
	puts	#' was not found.',cr=t
em2	anop		  endif
errExit	lda	#$FFFF	return with error
rn3a	pha		dispose of the token buffer
	free	buff
	pla
	sec		return with no name selected
	rts

rn4	free	buff	dispose of the token buffer
	lda	wcFlag	return the wilcard flag
	clc
	rts
;
;  Local data
;
checkDirectories ds 2	check directories for DELETE?
flagError ds	2	flag file not found errors?
buff	ds	4	token buffer

wcflag	ds	2	wild card flag
inflag	ds	2
must_exist ds	2	file must exist flag

inRec	dc	i'2'	OSInit_Wildcard record
inPathname ds	4
inFlags	dc	i'0'
	end

****************************************************************
*
*  GetTabLine - Get The Tab Line
*
*  Inputs:
*	r0 - language number for the tab line to read
*
*  Outputs:
*	tabs - tab line
*
****************************************************************
*
GetTabLine private
	debug GetTabLine
	using Common

	lda	tabLanguage	if the language numbers match then
	cmp	r0
	jeq	rts	  return

	move	#0,tabs,#256	default to tabs every 8 spaces
	ldy	#7
gt1	lda	#1
	sta	tabs,Y
	tya
	clc
	adc	#8
	tay
	cpy	#256
	blt	gt1
	lda	#2	set end of line marker
	sta	tabs+79

	stz	ffAction	read the tab file
	ph2	r0
	OSFastFile ffRec
	pl2	r0
	bcs	rts	if error or no file then use default

gt2	move4 ffFileHandle,r8	dereference the handle
	ldy	#2
	lda	[r8]
	sta	r4
	lda	[r8],Y
	sta	r6
	jsr	FindLn	find the tab line
	bcc	rt1
;
;  Get the tab line
;
	lda	r0	tabLanguage = r0
	sta	tabLanguage
	ldy	#0	read in the tab line
gt3	phy
	jsr	GetC
	ply
	bcs	gt4
	and	#$000F
	cmp	#RETURN
	beq	gt4
	short M
	sta	tabs,Y
	long	M
	iny
	cpy	#256
	blt	gt3
gt4	cpy	#256
	bge	rt1
	short M
	lda	#2
	sta	tabs,Y
	long	M
rt1	lda	#7	purge the file
	sta	ffAction
	OSFastFile ffRec
rts	rts
;
;  Locatate the correct tab line
;
FindLn	stz	num
fn1	jsr	GetC	Get a decimal number
	bcs	err
	jsl	IsDigit
	bcc	fn2
	and	#$000F
	pha
	lda	num
	ldx	#10
	jsl	~mul2
	clc
	adc	1,S
	plx
	sta	num
	bra	fn1

fn2	cmp	#RETURN	make sure this is the end of line
	beq	fn3
err	clc
	rts

fn3	lda	num	see if we found the tab line
	cmp	r0
	beq	fn4
	jsr	Skip	skip past settings
	jsr	Skip	skip past tab line
	bra	FindLn	next tab
fn4	jsr	Skip	skip the settings line
	sec
	rts
;
;  Skip to next line
;
Skip	jsr	GetC
	bcs	srts
	cmp	#RETURN
	bne	Skip
srts	rts
;
;  GetC - get a character; return C=1 if at end of file
;
GetC	lda	ffFileLength
	beq	yes
	dec	ffFileLength
	lda	[r4]
	inc4	r4
	and	#$00FF
	clc
	rts

yes	sec
	rts

num	ds	2
;
;  Local Data
;
ffRec		dc    i'14'	Fast File record
ffAction		ds    2
ffIndex		ds    2
ffFlags		dc    i'$C000'
ffFilehandle	ds    4
ffPathname	dc    a4'systabs'
ffAccess		ds    2
ffFileType	ds    2
ffAuxType 	ds    4
ffStorageType	ds    2
ffCreateDate	ds    8
ffModDate 	ds    8
ffOption		ds    4
ffFileLength	ds    4
ffBlocksUsed	ds    4
	end

****************************************************************
*
*  GetType - Read a file type from a token
*
*  Inputs:
*	r0 - pointer to the token
*
*  Outputs:
*	A - file type ID number
*	C - clear is successful else set
*
****************************************************************
*
GetType	private
	debug GetType
	using Common
;
;  Read the file type string
;
	lda	[r0]	make sure string is not bigger than
	cmp	#4	 three characters
	blt	gt1
	sec		file type is too long
	rts
;
;  Find a file number to go with the string
;
gt1	tax		X is the length of the string
	ldy	#2	split based on first character
	lda	[r0],Y
	and	#$00FF
	jsl	IsDigit
	bcs	gt4
	jsl	IsAlpha
	jcs	nm1
	cmp	#'$'
	beq	hx1
err	long	M	flag an error and return
	sec
	rts
;
;  Read a hex number
;
hx1	short M
	stz	r4	number = 0
hx2	iny		next character
	dex
	beq	hx3
	lda	[r0],Y
	jsl	IsXDigit	error if not a hex digit
	bcc	err
	jsr	ToInt	roll in the value
	asl	r4
	asl	r4
	asl	r4
	asl	r4
	ora	r4
	sta	r4
	bra	hx2

hx3	lda	r4	return the value
rts	long	M
	and	#$00FF
	clc
	rts
;
;  Read a decimal number
;
gt4	short M
	stz	r4	number = 0
gt5	lda	[r0],Y	get a character
	jsl	IsDigit	error if not a digit
	bcc	err
	pha		add in the value
	asl	r4
	lda	r4
	asl	r4
	asl	r4
	clc
	adc	r4
	sta	r4
	pla
	and	#$0F
	clc
	adc	r4
	sta	r4
	iny		next character
	dex
	bne	gt5
	bra	hx3	return the value
;
;  Read a named entry
;
	longa on
nm1	lla	r4,typecode	get a pointer to the type code table
	cpx	#3	make sure there are exactly 3 characters
	bne	err
	inc4	r0	r0 points to the chars -1

nm2	ldy	#0	error if no matches
	lda	[r4],Y
	and	#$00FF
	bne	nm3
	sec
	rts

nm3	ldy	#3	check name for match
nm4	short M
	lda	[r0],Y
	jsl	ToUpper
	cmp	[r4],Y
	long	M
	bne	nm5
	dbne	Y,nm4
	lda	[r4]	match found - set type
	and	#$00FF
	clc
	rts

nm5	add4	r4,#4	next entry
	bra	nm2
	end

****************************************************************
*
*  InitParms - Set language dependent parameter list
*
*  Inputs:
*	A - language number
*	ldpList - pointer to language list
*
*  Outputs:
*	li_laddr - pointer to parameters for that language
*
****************************************************************
*
InitParms private
	debug InitParms
	using Common

	and	#$7FFF
	sta	lnum	save language number
	stz	li_laddr	initialize STRING
	stz	li_laddr+2
	move4 ldpList,r4	get list pointer

fs1	lda	r4	are we at end of list
	ora	r4+2
	bne	fs2
	rts

fs2	ldy	#4	get a pointer to the string
	lda	[r4],Y
	sta	r8
	iny
	iny
	lda	[r4],Y
	sta	r10
	lda	[r8]	is this the one
	cmp	lnum
	bne	fs3
	add4	r8,#2,li_laddr	yes, set the pointer
	rts

fs3	ldy	#2	no, get next entry
	lda	[r4]
	tax
	lda	[r4],Y
	sta	r6
	stx	r4
	brl	fs1

lnum	ds	2
	end

****************************************************************
*
*  IsUtility - See if a program is a utility
*
*  Inputs:
*	r0 - pointer to file name
*
*  Outputs:
*	C - set if name is a utility
*	ctlast - pointer to command table entry
*
****************************************************************
*
IsUtility private
	debug IsUtility
	using Common

	move4 ctpointer,r4	scan the command table
st1	ldy	#disp_cnum+1	quit if at end of table
	lda	[r4],Y
	beq	no
	ldy	#disp_cnum	branch if not a utility
	lda	[r4],Y
	bne	st2
	add4	r4,#disp_name-1,r8	continue search if not one we want
	lda	[r8]
	xba
	and	#$00FF
	cmp	[r0]
	bne	st2
	tax
	ldy	#2
	short M
st1a	lda	[r0],Y
	jsl	ToUpper
	cmp	[r8],Y
	bne	st1b
	iny
	dex
	bne	st1a
	long	M
	bra	st3
st1b	long	M

st2	ldy	#disp_name	get next command
	lda	[r4],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r4
	sta	r4
	bcc	st1
	inc	r6
	bra	st1

st3	move4 r4,ctlast
	sec		found it
	rts

no	anop
	clc		not a utility
	rts
	end

****************************************************************
*
*  IsWildcard - see if the file contains wildcards
*
*  Inputs:
*	ptr - pointer to the path name
*
****************************************************************
*
IsWildcard private
isIt	equ	1	is it a wildcard?

	sub	(4:ptr),2

	stz	isIt	assume it is not
	lda	[ptr]	get the start position
	inc	A
	tay
	short M	for each character do
lb1	lda	[ptr],Y	  if it is ':' then
	cmp	#':'
	beq	lb3	    quit
	cmp	#'?'	  if it is '?' or '=' then
	beq	lb2
	cmp	#'='
	beq	lb2	    goto set/quit
	dey		next char
	cpy	#1
	bne	lb1
lb2	inc	isIt	set the flag
lb3	long	M	exit

	ret	2:isIt
	end

****************************************************************
*
*  KeepNameParm - Parse the line for the KEEP operand
*
*  Inputs:
*	LINE - input line
*
*  Outputs:
*	li_oaddr - keep name
*	C - clc if no error and keep found, else set
*
****************************************************************
*
KeepNameParm private
	debug KeepNameParm
	using Common

	jsr	SkipBlanks	skip any leading blanks
	short M
	ldx	#0	see if names match
ke1	jsr	NextChar
	jsl	ToUpper
	cmp	_keep-1,X
	bne	ke2
	cpx	#l:_keep
	bne	ke1
	bra	ke3

ke2	long	M
	sec		no this is not a keep
	rts

ke3	long	M
	jsr	GetChar	yes - remove KEEP=
	jsr	GetChar
	jsr	GetChar
	jsr	GetChar
	jsr	GetChar

	lda	li_keep	error if keep has been found
	and	#$00FF
	beq	ke4
	puts	#'Multiple keep parameters',cr=t,errout=t
	pla		return to caller
	sec
	rts

ke4	inc	li_keep	mark keep as found
	jsr	GetToken	get the keep name
	sta	exIn
	sta	r0
	stx	exIn+2
	stx	r2
	ora	r2
	bne	ke5
oom	lda	#outOfMem	out of memory
erts	jsl	SysError
	pla
	sec
	rts

ke5	lda	[r0]	branch if there is no name
	beq	ke7
	ph4	#exRec	expand devices and prefixes
	jsr	ExpandDevice
	pha
	free	exIn
	pla
	bne	erts
	lda	exPathname
	ora	exPathname+2
	beq	oom
	add4	exPathname,#2,li_oaddr
	clc
	rts

ke7	long	M
	puts	#'No keep name found',cr=t,errout=t
	pla		return to caller
	sec
	rts

_keep	dc	c'KEEP='

exRec	dc	i'2'	OSExpandDevices record for expanding 0/
exIn	ds	4
exPathname ds	4
	end

****************************************************************
*
*  LanguageParms - Parse the line for language dependent parameters
*
*  Inputs:
*	LINE - input line
*
*  Outputs:
*	ldpList - language dependent parameters
*	C - clc if no error and parm found, else set
*
****************************************************************
*
LanguageParms private
	debug LanguageParms
	using Common

	short M
	jsr	SkipBlanks
	ldx	#0
ld1	jsr	NextChar	check for an =
	cmp	#RETURN
	beq	ld2
	cmp	#'='
	beq	ld3
	jsl	IsWhite
	bcc	ld1

ld2	long	M
	sec		no this is not a language parameter
	rts

	longa off
ld3	lda	#' '	replace the =
	dex
	jsr	ChangeChar
	inx
	long	M
	phx		save the length of name
	jsr	FindCommand	see if this is a language
	bcs	ld4
ld3a	puts	#'Invalid language parameter',cr=t,errout=t
	plx		no flag error and return
	pla		return to caller
	sec
	rts

ld4	cmp	#0	make sure it is a language
	bne	ld3a
	sty	lnum
	pla		remove the language=
	sta	count
	beq	ld4b
ld4a	jsr	GetChar
	dbne	count,ld4a
;
;  Remove the parameter list
;
ld4b	stz	r2	length of name
	jsr	SkipBlanks	skip to list
	jsr	GetChar	make sure there is a '('
	cmp	#'('
	bne	ld9

	ldx	#0	count the number of characters
ld5	jsr	NextChar
	cmp	#RETURN
	beq	ld9
	cmp	#')'
	bne	ld5
	dex
	txa
	beq	ld7
	phx		reserve memory for the string
	clc
	adc	#4
	jsl	Malloc
	sta	r0
	stx	r2
	plx
	ora	r2
	beq	ld9
	lda	lnum	set the language number
	sta	[r0]
	txa		set the length of the string
	ldy	#2
	sta	[r0],Y
	ldy	#4	get the characters
	short M
ld6	jsr	GetChar
	sta	[r0],Y
	iny
	dex
	bne	ld6
	long	M	place the parms in the table
	ph4	r0
	ph4	#ldpList
	jsr	PushName
ld7	jsr	GetChar	make sure there is a closing ')'
	cmp	#')'
	bne	ld9
	clc
	rts

ld9	puts	#'Names syntax error',cr=t,errout=t
	pla		return to caller
	sec
	rts
;
;  Local data
;
count	ds	2	temp counter
lnum	ds	2	temp storage for language number
	longa on
	end

****************************************************************
*
*  ListAlias - Print Alias List
*
****************************************************************
*
ListAlias private
	debug ListAlias
	using Common

	jsr	FirstAlias	get the first entry

la1	lda	r0	quit if last one
	ora	r2
	beq	rts
	puts	#'Alias '	print name
	ph4	r4
	ph2	#0
	jsl	PrintOSName
	puts	#'  '
	ph4	r8	print value
	ph2	#0
	jsl	PrintOSName
	putcr
	jsr	NextAlias
	jsr	Pause	next variable
	bcc	la1
rts	rts
	end

****************************************************************
*
*  ListExport - List Exportable Variables
*
*  Inputs:
*	V_POINTER - pointer to variable table
*
****************************************************************
*
ListExport private
	debug ListExport
	using Common

	jsr	FirstVariable	get the first entry

ls1	lda	r0	quit if last one
	ora	r2
	beq	rts
	ldy	#12	if the variable is exportable then
	lda	[r0],Y
	beq	ls2
	puts	#'Export '	print name
	ph4	r4
	ph2	#0
	jsl	PrintOSName
	putcr
ls2	jsr	NextVariable
	jsr	Pause	next variable
	bcc	ls1
rts	rts
	end

****************************************************************
*
*  ListVariables - List Variables
*
*  Inputs:
*	V_POINTER - pointer to variable table
*
****************************************************************
*
ListVariables private
	debug ListVariables
	using Common

	jsr	FirstVariable	get the first entry

ls1	lda	r0	quit if last one
	ora	r2
	beq	rts
	puts	#'Set '	print name
	ph4	r4
	ph2	#0
	jsl	PrintOSName
	puts	#'  '
	ph4	r8	print value
	ph2	#0
	jsl	PrintOSName
	putcr
	jsr	NextVariable
	jsr	Pause	next variable
	bcc	ls1
rts	rts
	end

****************************************************************
*
*  MultiNameInit - Initialize for multi-name compiles
*
*  Inputs:
*	nameListPtr - list of source names
*
*  Outputs:
*	firstTime - set to true (1)
*	multiple - true (1) if there is more than one name
*		in the name list
*	objPtr - set to nil, old contents disposed
*
****************************************************************
*
MultiNameInit private
	debug MultiNameInit
	using Common

	stz	multiple	assume multiple is false
	move4 nameListPtr,r0	if nameListPtr <> nil then
	lda	r0
	ora	r2
	beq	lb1
	ldy	#2	  if nameListPtr^.next <> nil then
	lda	[r0]
	ora	[r0],Y
	beq	lb1
	inc	multiple	    multiple = 1
lb1	anop

	la	firstTime,1	firstTime := true

	free	objPtr	free(objPtr)
	stz	objPtr	objPtr = nil
	stz	objPtr+2
	rts
	end

****************************************************************
*
*  NameList - Parse the line for the NAMES operand
*
*  Inputs:
*	LINE - input line
*
*  Outputs:
*	li_paddr - partial compile name list
*	C - clc if no error and NAMES= found, else set
*
****************************************************************
*
NameList private
	debug NameList
	using Common

	short M
	jsr	SkipBlanks
	ldx	#0	see if names match
ns1	jsr	NextChar
	jsl	ToUpper
	cmp	_names-1,X
	bne	ns2
	cpx	#l:_names
	bne	ns1
	bra	ns3

ns2	long	M
	sec		no this is not a names
	rts

ns3	long	M
	jsr	GetChar	yes - remove NAMES=
	jsr	GetChar
	jsr	GetChar
	jsr	GetChar
	jsr	GetChar
	jsr	GetChar

	lda	li_paddr	error if names has been found
	ora	li_paddr+2
	beq	ns4
	puts	#'Multiple name lists',cr=t,errout=t
	pla		return to caller
	sec
	rts

ns4	lda	#2	length of name + 2
	sta	r4
	jsr	LineLength	get space for li_paddr
	jsl	Malloc
	sta	r0
	sta	li_paddr
	stx	r2
	stx	li_paddr+2
	ora	r2
	jeq	oom
	jsr	SkipBlanks	skip to list
	jsr	GetChar	make sure there is a '('
	cmp	#'('
	bne	ns9
ns5	jsr	SkipBlanks	skip to name
	jsr	GetChar	see if there is a name
	cmp	#')'
	beq	ns7
	cmp	#RETURN
	beq	ns9
ns6	ldy	r4	yes - save it
	short M
	sta	[r0],Y
	long	M
	inc	r4
	jsr	GetChar
	cmp	#')'
	beq	ns7
	cmp	#RETURN
	beq	ns9
	jsl	IsWhite
	bcc	ns6
	short M	place a space at the end
	ldy	r4
	lda	#' '
	sta	[r0],Y
	long	M
	inc	r4
	bra	ns5

ns7	ldy	r4	remove last char if its a space
	dey
	lda	[r0],Y
	and	#$00FF
	cmp	#' '
	bne	ns8
	dey
ns8	tya		save the length
	dec	A
	sta	[r0]
	clc
	rts

ns9	long	M
	puts	#'Names syntax error',cr=t,errout=t
	pla		return to caller
	sec
	rts

oom	long	M
	lda	#outOfMem
	jsl	SysError
	pla		return to caller
	sec
	rts

_names	dc	c'NAMES='
	end

****************************************************************
*
*  NextName - Fetch the next name to compile
*
*  Inputs:
*	nameListPtr - list of names to compile
*	firstTime - first time called?
*	multiple - multiple names in list?
*
*  Outputs:
*	li_saddr - file name to compile, or object files to
*		link
*	li_oaddr - if all names have compiled, keep name for
*		link
*	li_merrf - set to merr+1 if an error occurred
*
****************************************************************
*
NextName private
	debug NextName
	using Common
;
;  If not the first time through on a multiple compile, keep the
;  old object module name
;
	lda	multiple	if multiple then
	jeq	lb9
;
;  Place any names that are simply object modules or libraries in objPtr
;
lb3	stz	r0	  r0 = 0
	stz	r2
	lda	nameListPtr	  if nameListPtr <> nil then
	ora	nameListPtr+2
	beq	lb5
	ph4	#nameListPtr	    remove the top name from the list
	jsr	PopName
	sta	r0
	stx	r2
	phx		    while the name is an obj file do
	pha
	jsr	GetFileType
	cmp	#OBJ
	beq	lb4
	cmp	#LIB
	bne	lb5
lb4	jsr	AppendObj	      AppendObj(objPtr, r0)
	bra	lb3	      goto top of test
lb5	anop		  endif
;
;  If there is a file to compile, do so.  If not, set up for link.
;
	lda	r0	  if length(r0) <> 0 then
	ora	r2
	beq	lb7
	lsr	li_ops	    LI_OPS |= 1
	sec
	rol	li_ops
	lda	li_keep	    if LI_KEEP > 1 then
	and	#$00FF
	cmp	#2
	blt	kp1
	short M	      LI_KEEP := 0
	stz	li_keep
	long	M	    endif
kp1	lda	li_keep	    if li_keep = 0 then
	and	#$00FF
	bne	kp2
	free	li_oaddr	      free(li_oaddr)
	stz	li_oaddr	      li_oaddr = nil
	stz	li_oaddr+2
kp2	anop		      endif
!			    endif
	move4 r0,li_saddr	    li_saddr = name
	stz	firstTime	    firstTime := false
lb7	rts		  endif
;
;  For non-muliple compiles, simply return the original parameters.
;
lb9	anop		else
	lda	firstTime	  if firstTime then
	beq	lb10
	stz	firstTime	    firstTime := false
	ph4	#nameListPtr	    li_saddr = PopName(nameListPtr)
	jsr	PopName
	sta	li_saddr
	stx	li_saddr+2
lb10	rts		  endif
!			endif
	end

****************************************************************
*
*  Operands - Parse the operand for compiles and links
*
*  Outputs:
*	li_dcb - set language info
*
****************************************************************
*
Operands private
	debug Operands
	using Common
plus_flags equ r4	plus flags mask
minus_flags equ r0	minus flags mask
;
;  Read the flags
;
fo1	lda	ldpList	dispose of the current language
	ora	ldpList+2	 dependent parameter list
	beq	fo2
	ph4	#ldpList
	jsr	PopName
	jsl	Free
	bra	fo1

fo2	lda	nameListPtr	dispose of the current list of names
	ora	nameListPtr+2	 to compile
	beq	fo3
	ph4	#nameListPtr
	jsr	PopName
	jsl	Free
	bra	fo2

fo3	lda	objPtr	dispose of the current obj list
	ora	objPtr+2
	beq	fo3a
	free	#objPtr
	stz	objPtr
	stz	objPtr+2

fo3a	free	li_oaddr	keep not found yet
	stz	li_oaddr
	stz	li_oaddr+2
	short M
	stz	li_keep
	long	M
	free	li_paddr	partial compile names not found yet
	stz	li_paddr
	stz	li_paddr+2

	jsr	RemoveCommand	remove command name
	lda	#$FFFF	read the flags
	sta	r0
	sta	r2
	sta	r4
	sta	r6
	jsr	Flags
	jcs	rts
	lda	plus_flags+2	see if +E or -E specified
	and	#$0800
	bne	fo5
	lda	minus_flags+2
	and	#$0800
	bne	fo5
	lda	#$0800
	ldx	exec_level	if not specified then
	beq	fo4
	ora	minus_flags+2	  if in exec file then set -E
	sta	minus_flags+2
	bra	fo5
fo4	ora	plus_flags+2	  else set +E
	sta	plus_flags+2

fo5	move4 plus_flags,li_plus
	move4 minus_flags,li_minus
;
;  Insure there is a command line
;
	jsr	SkipBlanks	make sure a command name is givin
	jsr	LineLength
	tax
	bne	fn1
	puts	#'Input file: '
	jsr	GetLine
	jsr	SkipBlanks
	jsr	LineLength
	tax
	bne	fn1
	sec
rts	rts
;
;  Read the list of names and process special parameters
;
fn1	jsr	GetToken	read a path name
	sta	epIn
	sta	r0
	stx	epIn+2
	stx	r2
	ora	r2	branch if there are no more
	jeq	oom
	lda	[r0]
	beq	dp1

	ph4	#epRec	expand devices, etc
	jsr	ExpandPath
	sta	error
	free	epIn
	lda	error
	jne	erts
	add4	epPathname,#2
	ph4	epPathname	place the name in the name list
	ph4	#nameListPtr
	jsr	PushName
	sta	error
	tax
	jne	erts

fn2	jsr	SkipBlanks	skip blanks
	jsr	LineLength	quit if RETURN found
	tax
	beq	dp1
	jsr	NameList	check and get NAMES= parameters
	bcc	fn2
	jsr	KeepNameParm	check and get KEEP= parameter
	bcc	fn2
	jsr	LanguageParms	check and get language dependent parms
	bcc	fn2
	brl	fn1	next path name
;
;  Set up default parameters
;
dp1	lda	nameListPtr	make sure we got at least one name
	ora	nameListPtr+2
	bne	dp2
	puts	#'Missing file name',cr=t,errout=t
	sec
	rts

dp2	jsr	EOLCheck	make sure there is no garbage
	jcs	sec
	short M
	stz	li_merr	max error allowed = 0
	stz	li_merrf	max error found = 0
	stz	li_keep	no keep file, yet
	long	M
	stz	li_org	org = nil
	stz	li_org+2
	clc
	rts
;
;  Error handlers
;
err	puts	#'Expanding file names made the line too long',cr=t,errout=t
	sec
	rts

err2	puts	#'Unknown parameters',cr=t,errout=t
	sec
	rts

oom	lda	#outOfMem	out of memory
	sta	error
erts	lda	error	error return
	jsl	SysError
sec	sec
	rts
;
;  Local data
;
error	ds	2	error number
list_len ds	2	length of names list

epRec	dc	i'3'	ExpandPath record
epIn	ds	4
epPathname ds	4
	dc	i'0'
	end

****************************************************************
*
*  PopName - pop a name from an OS string list
*
*  Inputs:
*	list - pointer to the pointer to the first name in the list
*
*  Outputs:
*	X-A - pointer to the OS string
*
*  Notes:
*	The string returned is dynamically allocated.	It is
*	up to the caller to dispose of the string.
*
****************************************************************
*
PopName	private
	debug PopName

str	equ	1	pointer to the string
rec	equ	5	list element

	sub	(4:list),8

	stz	str	str = nil
	stz	str+2
	lda	[list]	get a pointer to the 1st record
	sta	rec
	ldy	#2
	lda	[list],Y
	sta	rec+2
	ora	rec	quit now if there is nothing in the list
	beq	lb1
	lda	[rec]	remove the record from the list
	sta	[list]
	lda	[rec],Y
	sta	[list],Y
	iny		get the string pointer
	iny
	lda	[rec],Y
	sta	str
	iny
	iny
	lda	[rec],Y
	sta	str+2
	free	rec	dispose of the record

lb1	ret	4:str
	end

****************************************************************
*
*  PrintCommand - Print the command table entry
*
*  Inputs:
*	R0 - long pointer to begining of command entry
*
****************************************************************
*
PrintCommand private
	debug PrintCommand
	using Common

	move	#' ',buff1+1,#12	initialize the string
	short I,M
	lda	#12
	sta	buff1
	ldy	#disp_name	copy over the language name
	lda	[r0],Y
	tax
pt2	iny
	lda	[r0],Y
	sta	buff1-disp_name,Y
	dex
	bne	pt2
	long	I,M
	puts	buff1-1
	rts
	end

****************************************************************
*
*  PrintCommandTable - Print the command table
*
*  Inputs:
*	CTHANDLE - command table handle
*
*  Outputs:
*	Prints all commands in the command table
*
****************************************************************
*
PrintCommandTable private
	debug PrintCommandTable
	using Common

	puts	#'Available Commands:',cr=t
	putcr

	lda	#6
	sta	count
	move4 ctpointer,r0
lt1	ldy	#disp_cnum+1	quit if at end of table
	lda	[r0],Y
	beq	rts
	putc	#' '
	jsr	PrintCommand	write the entry
	dec	count
	bne	lt2
	putcr
	lda	#6
	sta	count
lt2	ldy	#disp_name	get next command
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	lt1
	inc	r2
	bra	lt1

rts	putcr
	rts

count	ds	2	loop counter
	end

****************************************************************
*
*  PrintLanguage - Print the current language
*
*  Inputs:
*	GET_LANG - gets current language number
*
****************************************************************
*
PrintLanguage private
	debug PrintLanguage
	using Common

	puts	#'Current language: '
	Get_Lang glDCB
	lda	glNum
	ora	#$8000
	jsr	FindCommand2
	bcc	err
	sub4	r0,#disp_name	set pointer to begining of entry
	jsr	PrintCommand	write the language name
	putcr
	rts

err	puts	#'not in command table',cr=t
	rts	                        in command table

glDCB	anop		get/set language DCB
glNum	ds	2	language number
	end

****************************************************************
*
*  PrintLanguages - Print the available languages
*
*  Inputs:
*	CTHANDLE - command table handle
*
****************************************************************
*
PrintLanguages private
	debug PrintLanguages
	using Common

	puts	#'Available Languages:',cr=t
	putcr
	puts	#'    Language    ID Number',cr=t
	putcr

	move4 ctpointer,r0
pl1	ldy	#disp_cnum+1	quit if at end of table
	lda	[r0],Y
	beq	rts
	ldy	#disp_cnum	branch if not a language
	lda	[r0],Y
	bpl	pl2
	and	#$7FFF	save the language number
	sta	r4
	puts	#'    '
	jsr	PrintCommand	write the command name
	put2	r4,cr=t

pl2	ldy	#disp_name	get next command
	lda	[r0],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	r0
	sta	r0
	bcc	pl1
	inc	r2
	bra	pl1

rts	rts
	end

****************************************************************
*
*  PrintPrefix - Print the current prefixes
*
****************************************************************
*
PrintPrefix private
	debug PrintPrefix
	using Common

	puts	#'System Prefix:',cr=t	write the header
	putcr
	puts	#'    Number    Name',cr=t
	putcr
;
;  Put the boot prefix
;
	ph4	#gb_dcb	get the boot volume name
	jsr	GetBootVol
	bcs	ap1
	puts	#'    *         '	print the prefix name
	ph4	gb_dcb+2	print the prefix value
	ph2	#$8000
	jsl	PrintOSName
	putcr
	free	gb_dcb+2	dispose of the prefix name buffer
	jsr	Pause	allow the user to pause the listing
	jcs	pr3
;
;  Put the boot prefix
;
ap1	ph4	#ep_dcb	get the name of the @ prefix
	jsr	ExpandPath
	tax
	bne	pr0
	puts	#'    @         '	print the prefix name
	ph4	ep_dcb+6	print the prefix value
	ph2	#$8000
	jsl	PrintOSName
	putc	#':',cr=t
	free	ep_dcb+6	dispose of the prefix name buffer
	jsr	Pause	allow the user to pause the listing
	jcs	pr3
;
;  Put the system prefixes
;
pr0	stz	pr_num	initialize the prefix dcb
pr1	ph4	#pr_dcb	get the prefix
	jsr	GetPrefix
	bcs	pr2	ignore errors
	move4 pr_name,r0	ignore prefixes that are not set
	ldy	#2
	lda	[r0],Y
	clc
	beq	pr1a
	put2	pr_num,#5	print the prefix number
	puts	#'         '	skip some space
	ph4	pr_name	print the prefix name
	ph2	#$8000
	jsl	PrintOSName
	putcr
	jsr	Pause	allow the user to pause the listing
pr1a	php		save the "stop" code from Pause
	free	pr_name	dispose of the prefix name buffer
	plp		quit if Pause indicated we should
	bcs	pr3

pr2	inc	pr_num	next prefix
	lda	pr_num
	cmp	#32
	jlt	pr1
pr3	rts

gb_dcb	dc	i'1',a4'0'	GET_BOOT_VOL DCB

ep_dcb	dc	i'3',a4'atPrefix,0',i'0' EXPAND_PATH DCB
atPrefix dc	i'2',c'@:'

pr_dcb	dc	i'2'	GET_PREFIX DCB
pr_num	ds	2
pr_name	ds	4
	end

****************************************************************
*
*  PrintTime - Print the time
*
****************************************************************
*
PrintTime private
	debug PrintTime
	using Common

	puts	#'Time: '	write the header string

	sec		push space for result
	tsc
	sbc	#8
	tcs
	_ReadTimeHex	call read time
	pl2	second	get the time
	pl2	hour
	pl2	day
!	pla

!	pha		get the date format
	ph2	#$34
	_ReadBParam
	pla
	dec	a	split based on format
	bpl	lb1

	jsr	PrintMonth	month day year format
	jsr	PrintDay
	jsr	PrintYear
	bra	lb3

lb1	bne	lb2	day month year format
	jsr	PrintDay
	jsr	PrintMonth
	jsr	PrintYear
	bra	lb3

lb2	jsr	PrintYear	year month day format
	jsr	PrintMonth
	jsr	PrintDay

lb3	putc	#' '	padd with an extra space
	pha		get the time format
	ph2	#$35
	_ReadBParam
	pla
	bne	lb7

lb4	short M	AM-PM format
	stz	PM	assume AM format
	lda	hour	decide if it is AM or PM
	cmp	#13
	blt	lb5
	inc	PM	it's PM
	sec		convert the hour
	sbc	#12
	sta	hour
lb5	long	M
	jsr	PrintHour	print the time
	lda	PM	print AM or PM
	bne	lb6
	puts	#' AM',cr=t
	rts
lb6	puts	#' PM',cr=t
	rts

lb7	jsr	PrintHour	print military time
	putcr
	rts
;
;  PrintDay - print the numerical date
;
PrintDay lda	day
	and	#$00FF
	inc	A
	bra	yr1
;	sta	num
;	put2	num
;	putc	#' '
;	rts
;
;  PrintMonth - print the month characters
;
PrintMonth lda month
	and	#$00FF
	asl	a
	asl	a
	tax
	lda	monthCh,x
	sta	str+1
	lda	monthCh+2,x
	sta	str+3
	puts	str-1
	rts

str	dc	i1'4',c'    '
;
;  PrintYear - print the year
;
PrintYear lda	year
	and	#$00FF
	clc
	adc	#1900
yr1	sta	num
	put2	num
	putc	#' '
	rts
;
;  PrintHour - print the time string
;
PrintHour lda	hour
	jsr	PrintNum
	putc	#':'
	lda	minute
	jsr	PrintNum
	putc	#':'
	lda	second
!	jsr	PrintNum
!	rts
;
;  PrintNum - print a two-digit number
;
PrintNum short I,M
	jsr	dec
	jsr	xzero
	long	I,M
	phy
	stx	num
	putc	num
	pl2	num
	putc	num
	rts
;
;  Local data area
;
monthCh	dc	c'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec '

day	ds	1	time/date info
month	ds	1
hour	ds	1
year	ds	1
second	ds	1
minute	ds	1

num	ds	2	work number
PM	ds	2	PM? (or AM)
	longa on
	longi on
	end

****************************************************************
*
*  PrintUnits - Print the units
*
*  Outputs:
*	Writes device table
*
****************************************************************
*
PrintUnits private
	debug PrintUnits
	using Common

	puts	#'Units Currently On Line:',cr=t
	putcr
	puts	#'    Number  Device              Name',cr=t
	putcr
;
;  Make each volume call
;
	lda	#1	init the device number
	sta	dev_num
un1	OSD_Info block	get device name
	jcs	un6	done if error
	OSVolume vlBlock	get the volume name
	bcc	un1a
	cmp	#$58
	jne	un5
	move	chdev,vlName+2,#20
un1a	puts	#'    .D'	write the device number
	put2	dev_num
	lda	dev_num
	cmp	#10
	bge	un2
	putc	#' '
un2	puts	#'    '
	ph4	#name+2	write the device name
	ph2	#0
	jsl	PrintOSName
	sec
	lda	#20
	sbc	name+2
	beq	un3
	bpl	un3a
un3	lda	#1
un3a	tax
un4	phx
	putc	#' '
	plx
	dex
	bne	un4
	ph4	#vlName+2	print the volume name
	ph2	#0
	jsl	PrintOSName
	putcr
un5	inc	dev_num	next device
	jsr	Pause
	jcc	un1
un6	rts
;
;  Local data
;
block	anop		control block for OSD_Info call
	dc	i'2'
dev_num	ds	2
	dc	a4'name'

vlBlock	anop		control block for OSVolume call
	dc	i'2'
	dc	a4'name+2'
	dc	a4'vlName'

chdev	dc	i'18',c'<Character Device>'
name	dc	i'35,0',31c' '
vlName	dc	i'35,0',31c' '
	end

****************************************************************
*
*  PushName - push an OS string onto a list
*
*  Inputs:
*	str - address of the string to push
*	list - address of the head of the list
*
*  Outputs:
*	A - error code; 0 if none
*
*  Notes:
*	The string passed should be dynamically allocated.  If
*	an error occurs, the string is disposed of.
*
****************************************************************
*
PushName private
	using Common
error	equ	1	error number
rec	equ	3	list record

	sub	(4:str,4:list),6

	stz	error	no error, yet
	malloc #8	get memory for the record
	sta	rec
	stx	rec+2
	ora	rec+2
	bne	lb1
	free	str	handle the error
	lda	#outOfMem
	sta	error
	bra	lb4

lb1	lda	#0	rec^.next = nil
	sta	[rec]
	ldy	#2
	sta	[rec],Y
	lda	str	rec^.str = str
	iny
	iny
	sta	[rec],Y
	lda	str+2
	iny
	iny
	sta	[rec],Y
lb2	lda	[list]	while list^ <> nil do
	ldy	#2
	ora	[list],Y
	beq	lb3
	lda	[list],Y	  list = list^
	tax
	lda	[list]
	sta	list
	stx	list+2
	bra	lb2	endwhile
lb3	lda	rec	list^ = rec
	sta	[list]
	lda	rec+2
	sta	[list],Y

lb4	ret	2:error
	end

****************************************************************
*
*  ReadAlias - Read an alias
*
*  Inputs:
*	R12 - pointer to alias string (os string)
*
*  Outputs:
*	X-A - pointer to alias value (os string)
*
****************************************************************
*
ReadAlias private
	using Common
	using RecCom

	jsr	FindAlias	find the alias
	lda	r0
	ora	r0+2
	bne	rd1
	lda	#null	not found so return null string
	ldx	#^null
	rts

rd1	lda	r8	return the alias value
	ldx	r10
	rts

null	dc	i'0'
	end

****************************************************************
*
*  ReadNumber - Read a number
*
*  Inputs:
*	r0 - pointer to an OS string
*
*  Outputs:
*	A - decimal value of the number
*	C - set if error (non numeric value)
*
****************************************************************
*
ReadNumber private
	debug ReadNumber
	using Common

	lda	[r0]	make sure all characters are digits
	tax
	beq	lb2
	ldy	#2
lb1	lda	[r0],Y
	and	#$00FF
	jsl	IsDigit
	bcc	lb2
	iny
	dex
	bne	lb1
	ph4	r0	convert to value
	jsl	OSStringToLong
	clc
	rts

lb2	sec
	rts
	end

****************************************************************
*
*  RemoveCommand - Remove command name from input line
*
*  Inputs:
*	LINE - input line
*
*  Outputs:
*	LINE - input line with command and following spaces
*		removed
*
****************************************************************
*
RemoveCommand private
	debug RemoveCommand
	using Common

rm1	ldx	#0	remove command name
	jsr	NextChar
	cmp	#RETURN
	beq	rm2
	jsl	IsWhite
	bcs	rm2
	jsr	GetChar
	bra	rm1
rm2	jmp	SkipBlanks	remove following blanks
	end

****************************************************************
*
*  SetNewPrefixes - change to the new prefixes
*
*  Outputs:
*	A - zero if no error; error number if there was one
*
****************************************************************
*
SetNewPrefixes private
	debug SetNewPrefixes
	using Common

	stz	opNum	0 -> 8
	lda	#8
	sta	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	1 -> 9
	inc	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	2 -> 13
	lda	#13
	sta	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	3 -> 14
	inc	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	4 -> 15
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	5 -> 16
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	6 -> 17
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	7 -> 18
	inc	npNum
	jsr	DoIt
	bcs	rts

	lda	#7	set prefixes 0 to 7 to null
	sta	nlNum
sp1	OSSet_Prefix nlRec
	dec	nlNum
	bpl	sp1
rts	rts

DoIt	ph4	#opRec	get the old prefix
	jsr	GetPrefix
	lda	opName
	ora	opName+2
	bne	lb1
	lda	#outOfMem
	sec
	bra	rts
lb1	move4 opName,r0	if the old prefix is not set then
	ldy	#2
	lda	[r0],Y
	bne	lb2
	free	opName	  free the buffer
	lda	#0	  return
	clc
	rts

lb2	add4	opName,#2,npName	set the old prefix
	OSSet_Prefix npRec
	pha
	php
	free	opName	free the buffer
	plp
	pla
	rts

opRec	dc	i'2'	old style prefix
opNum	ds	2
opName	ds	4

npRec	dc	i'2'	new style prefix
npNum	ds	2
npName	ds	4

nlRec	dc	i'2'	used to set prefixes to null
nlNum	ds	2
	dc	a4'null'

null	ds	2	null OS name
	end

****************************************************************
*
*  SetOldPrefixes - change to the old prefixes
*
*  Outputs:
*	A - zero if no error; error number if there was one
*
****************************************************************
*
SetOldPrefixes private
	debug SetOldPrefixes
	using Common

	stz	opNum	8 -> 0
	lda	#8
	sta	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	9 -> 1
	inc	npNum
	jsr	DoIt
	jcs	rts
	inc	opNum	13 -> 2
	lda	#13
	sta	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	14 -> 3
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	15 -> 4
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	16 -> 5
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	17 -> 6
	inc	npNum
	jsr	DoIt
	bcs	rts
	inc	opNum	18 -> 7
	inc	npNum

DoIt	ph4	#npRec	get the new prefix
	jsr	GetPrefix
	lda	npName
	ora	npName+2
	bne	lb1
	lda	#outOfMem
	sec
	bra	rts
lb1	add4	npName,#2,opName	set the old prefix
	OSSet_Prefix opRec
	pha
	php
	free	npName
	plp
	pla
rts	rts

opRec	dc	i'2'	old style prefix
opNum	ds	2
opName	ds	4

npRec	dc	i'2'	new style prefix
npNum	ds	2
npName	ds	4
	end

****************************************************************
*
*  SetStatus - Set {Status}
*
*  Inputs:
*	A - value to set it to
*
*  Outputs:
*	C - set if error
*	A - error number
*
****************************************************************
*
SetStatus private
	debug SetStatus
	using Common

	cmp	lastStatus	if status has changed
	bne	lb1
	ldx	setUsed	  or an OSSet call has occurred then
	beq	lb2
lb1	sta	lastStatus	  save the status value
	pha		  set the variable
	ph4	#string
	jsl	IntToOSString
	OSSet svRec
	stz	setUsed	  clear the OSSet flag
lb2	rts

svRec	dc	i'3'	OSSet record
svName	dc	a4'status'
svValue	dc	a4'string'
svExport dc	i'0'

string	ds	8
	end

****************************************************************
*
*  SetVariable - Set a variable
*
*  Inputs:
*	X-A - pointer to the variable name to set
*	line - value to set the variable to (possibly quoted)
*
*  Outputs:
*	A - error code
*
****************************************************************
*
SetVariable private
	debug SetVariable
	using Common

	sta	svName	set the variable name
	stx	svName+2
	stz	svExport	get the export flag (if any)
	sta	r12
	stx	r14
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb1
	ldy	#12
	lda	[r0],Y
	sta	svExport
lb1	stz	svValue	svValue = nil
	stz	svValue+2
	jsr	GetString	get the variable value
	bcs	err
	sta	svValue
	stx	svValue+2
	ora	svValue+2
	beq	err
	OSSet svRec	set the variable
	bcs	err
	free	svValue	dispose of the string buffer
	lda	#0
	rts

err	puts	#'Could not set variable',cr=t,errout=t
	free	svValue
	lda	#$FFFF
	rts

svRec	dc	i'3'	OSSet record
svName	ds	4
svValue	ds	4
svExport	ds	2
	end

****************************************************************
*
*  StrToNum - convert a string to a number
*
*  Inputs:
*	ptr - pointer to a GS/OS string to convert
*
*  Outputs:
*	A - value
*	X - 0 if the string contains non-digits, else 1
*
*  Notes:
*	If the string contains non-numeric digits, all digits
*	up to the first non-digit are still processed and
*	returned as a valid decimal number.
*
****************************************************************
*
StrToNum private
val	equ	1	value
isNumber equ	3	is the string all digits?

	sub	(4:ptr),4

	stz	val	val = 0
	lda	#1	isNumber = true
	sta	isNumber
	lda	[ptr]	X = # of characters
	beq	lb4	(done if there are no characters)
	tax
	ldy	#2	for each character do
lb1	lda	[ptr],Y
	and	#$00FF	  if the character is not a digit then
	cmp	#'0'
	blt	lb2
	cmp	#'9'+1
	blt	lb3
lb2	dec	isNumber	    isNumber = false
	bra	lb4	    return
lb3	and	#$000F	  val = val*10 + A
	pha
	asl	val
	lda	val
	asl	A
	asl	A
	adc	val
	adc	1,S
	sta	val
	pla
	iny		next character
	dex
	bne	lb1

lb4	ret	4:val
	end

****************************************************************
*
*  Time - Format the Date and Time
*
*  Inputs:
*	R0-R7 - date/time field
*
*  Outputs:
*	buff1 - string representing the time
*
****************************************************************
*
Time	start
	debug Time
	using Common

	lda	r3	check for no time field
	bne	tm1
	move	noTime,buff1+1,#16	move the no time field into
	rts		 the string

tm1	move	#' ',buff1+2,#16	initialize the string
	short I,M
	lda	#16
	sta	buff1+1
	lda	r4	set the date
	inc	A
	jsr	dec
	stx	buff1+2
	sty	buff1+3
	lda	r5	set the month
	cmp	#12	month has to be 1..12
	blt	tm3
	lda	#1
tm3	sta	r8
	asl	A
	adc	r8
	tax
	lda	month,X
	sta	buff1+5
	lda	month+1,X
	sta	buff1+6
	lda	month+2,X
	sta	buff1+7
	lda	r3	set the year
	cmp	#100	year has to be 0..99
	blt	tm4
	lda	#0
tm4	jsr	dec
	stx	buff1+9
	sty	buff1+10
	lda	r1	branch if the time is 0
	ora	r2
	beq	tm5
	lda	r2	set the hour
	jsr	dec
	stx	buff1+12
	sty	buff1+13
	lda	#':'
	sta	buff1+14
	lda	r1	set the minute
	jsr	dec
	jsr	xzero	if X is blank then set X to '0'
	stx	buff1+15
	sty	buff1+16
tm5	long	I,M
	rts
;
;  DEC: convert to decimal digits.
;
	longa off
	longi off
dec	entry
	ldx	#$FF
	sec
dc1	sbc	#10
	inx
	bcs	dc1
	adc	#10
	ora	#'0'
	tay
	txa
	beq	dc2
	ora	#'0'
	tax
	rts

dc2	ldx	#' '
	rts
;
;  If X is a blank then X = '0'
;
xzero	entry
	cpx	#' '
	bne	xzrts
	ldx	#'0'
xzrts	rts
	longa on
	longi on
;
;  Local data area
;
noTime	dw	'   <No Date>    '
month	dc	c'JanFebMarAprMayJunJulAugSepOctNovDec'
	end

****************************************************************
*
*  TypeBuffer - Print a buffer to standard output
*
*  Inputs:
*	R0 - starting line number
*	R4 - ending line number (0 if entire file)
*	R8 - flag to put line numbers (0 no, 1 yes)
*	R10 - expand tabs flag (0 no, 1 yes)
*	R12 - length of buffer
*	R16 - pointer to begining of file
*	NON_ASCII - non ascii allowed as output?
*
*  Outputs:
*	Prints the file begining at line R0 and ending at
*	line R4.  Prints line numbers if R8 is true.
*
****************************************************************
*
TypeBuffer private
	debug TypeBuffer
	using Common
formfeed equ	12
linefeed equ	10
maxlines equ	$1000000
pSize	equ	80	chars before allowing a pause

	stz	col	start a new line
	lda	r4	if no end line then print all of file
	ora	r4+2
	bne	fp
	lla	r4,maxlines
fp	sub4	r4,r0,r4	get number of lines to print
	inc4	r4
	lda	r4+2	make sure start line is < end line
	bpl	fp2
	puts	#'Start line is greater than end line',cr=t,errout=t
	lda	#-1
	sta	r0
	rts

fp2	lda	r4	make sure there are lines to print
	ora	r4+2
	jeq	rts
	add4	r12,r16,fl_end	compute end of file
	lda	r0	see if starting is not zero
	ora	r0+2
	jeq	fp8
;
;  Compute starting line address
;
fp3	dec4	r0	next line
	lda	r0
	ora	r0+2
	jeq	fp8
	inc4	line_num	update the line number
fp4	lda	[r16]	skip over a line
	and	#$00FF
	cmp	#RETURN
	beq	fp5
	inc4	r16
	lda	r16
	ldx	r18
	cmp	fl_end
	bne	fp4
	cpx	fl_end+2
	bne	fp4
	brl	rts	past eof - return

fp5	inc4	r16	skip RETURN
	lda	r16	if not past eof then
	ldx	r18	  next line
	cmp	fl_end
	bne	fp3
	cpx	fl_end+2
	bne	fp3
	brl	rts	else quit
;
;  Print out the lines
;
fp8	lda	r8	see if line number flag is set
	beq	fp9
	put4	line_num,#6
	putc	#' '
	inc4	line_num	update the line number
fp9	lda	r18	quit if at EOF
	cmp	fl_end+2
	bne	fp9a
	lda	r16
	cmp	fl_end
	jeq	rts
fp9a	lda	[r16]	write the line
	and	#$00FF
	ldx	non_ascii	  fetch a char
	bne	fp10	  if not non_ascii and
	cmp	#$7F	    char is control then
	bge	fp10b	    skip it
	cmp	#$20
	bge	fp10
	cmp	#RETURN
	beq	fp10
	cmp	#FormFeed
	beq	fp10
	cmp	#LineFeed
	bne	fp10b
fp10	cmp	#TAB	if the char is a tab
	bne	tb3
	ldx	r10	  and the tab flag is set then
	beq	tb3
	lda	#' '	  expand the tab
	jsl	~stdout
	ldx	col
	inx
	short M
tb1	lda	tabs,X
	bne	tb2
	lda	#' '
	jsl	~stdout
	inx
	cpx	#256
	blt	tb1
tb2	stx	col
	long	M
	bra	fp10b
tb3	jsl	~stdout	print the character
	inc	col
	cmp	#RETURN	if RETURN the break
	beq	fp11
fp10b	inc4	r16	update char pointer
	dec	pCount	if --pCount >= 0 then
	bpl	fp9	  next char
	lda	#pSize	else pCount = pSize
	sta	pCount
	jsr	Pause	  if stop seen then stop
	jcc	fp9
	inc	abortType
	bra	rts

fp11	inc4	r16	update the file pointer
	stz	col	start a new line
	lda	r18	make sure we're not past end of file
	cmp	fl_end+2
	bne	fp13
	lda	r16
	cmp	fl_end
fp13	bge	rts
	dec4	r4	next line
	jsr	Pause	if stop seen then stop
	bcc	fp14
	inc	abortType
	bra	rts
fp14	lda	#pSize	reset pause char counter
	sta	pCount
	lda	r4	next line
	ora	r6
	jne	fp8
rts	stz	r0
	rts

col	ds	2	column number
fl_end	ds	4	end of file pointer
pCount	ds	2	pause counter
	end

****************************************************************
*
*  TypeFile - Print a file to standard output
*
*  Inputs:
*	nxPath - file name to type
*	R0 - starting line number
*	R4 - ending line number (0 if entire file)
*	R8 - flag to put line numbers (0 no, 1 yes)
*	R10 - expand tabs flag (0 no, 1 yes)
*	NON_ASCII - non ascii allowed as output?
*
*  Outputs:
*	C - clear if everything ok else set
*
*	Prints the file begining at line R0 and ending at
*	line R4.  Prints line numbers if R8 is true.
*
****************************************************************
*
TypeFile private
	debug TypeFile
	using Common
	using RecCom

	add4	nxPath,#2,ffPathname	load the file
	stz	ffAction
	ph4	r0
	ph4	r4
	ph4	r8
	OSFastFile ffRec
	tax
	pl4	r8
	pl4	r4
	pl4	r0
	txa
	jcs	err1
	lda	ffFileType	make sure file is SRC or TXT
	cmp	#TXT
	beq	ty1
	cmp	#SRC
	beq	ty1
	cmp	#$50
	bne	er1
	lda	ffAuxType
	cmp	#$5445
	beq	ty1
er1	ph4	nxPath
	ph2	#$C000
	jsl	PrintOSName
	puts	#' must be SRC, TXT, or Teach',cr=t,errout=t
	brl	err2

ty1	ph4	r0	get the tab line
	ph4	r4
	ph4	r8
	stz	r0
	lda	ffFileType
	cmp	#SRC
	bne	gt1
	lda	ffAuxType
	sta	r0
gt1	jsr	GetTabLine
	pl4	r8
	pl4	r4
	pl4	r0
	lda	ffFileLength	quit if file is empty
	ora	ffFileLength+2
	jeq	ty2
	move4 ffFileLength,r12	set the length for TypeBuffer
	move4 ffFileHandle,r16	dereference the handle
	ldy	#2
	lda	[r16]
	tax
	lda	[r16],Y
	sta	r18
	stx	r16
	jsr	TypeBuffer	write out the file
ty2	lda	#7	make the file purgable
	sta	ffAction
	OSFastFile ffRec
	clc
	stz	r0
	rts

err1	sta	r0	save error number
	jsl	SysError	write error message
err2	lda	#7	make the file purgable
	sta	ffAction
	OSFastFile ffRec
	sec
	rts

ffRec		dc    i'14'	Fast File record
ffAction		ds    2
ffIndex		ds    2
ffFlags		dc    i'$C000'
ffFilehandle	ds    4
ffPathname	ds    4
ffAccess		ds    2
ffFileType	ds    2
ffAuxType 	ds    4
ffStorageType	ds    2
ffCreateDate	ds    8
ffModDate 	ds    8
ffOption		ds    4
ffFileLength	ds    4
ffBlocksUsed	ds    4
	end

****************************************************************
*
*  UnsetAlias - Unset an alias
*
*  Inputs:
*	R12 - pointer to alias name (OS input string)
*
*  Outputs:
*	C - clear if successful else set
*
****************************************************************
*
UnsetAlias private
	using Common
	using RecCom

	jsr	FindAlias	find the alias
	lda	r0
	ora	r0+2
	beq	err
	lda	p1	remove node from the list
	ora	p1+2	if this is the only node then
	bne	us1
	ldy	#2	  A_POINTER = R0^next
	lda	[r0]
	sta	a_pointer
	lda	[r0],Y
	sta	a_pointer+2
	bra	us5
us1	ldy	#2	else
	lda	[r0]	  P1^next = R0^next
	sta	[p1]
	lda	[r0],Y
	sta	[p1],Y
us5	free	r0	dispose of the node
	free	r4	dispose of the name
	free	r8	dispose of the string
	clc
	rts

err	sec		did not find alias
	rts
	end

****************************************************************
*
*  WriteFileInfo - Write a catalog entry
*
*  Inputs:
*	nxRec - file info
*	catChar - separator character for full path names
*	catCol - column number (for -n option only)
*	catFlags - mFlags field
*	catPathLength - length of the base path name
*
*  Outputs:
*	Writes catalog entry for file name to standard output
*
****************************************************************
*
WriteFileInfo private
	debug WriteFileInfo
	using Common
	using Command
;...............................................................
;
;  Write a list of names with no other info
;
;...............................................................
;
	lda	catFlags+2	branch if not a names only write
	and	#^set_n
	beq	lb1

	add4	nxPath,#2,r0	find the length of the file name
	sec
	lda	[r0]
	sbc	catPathLength
	tax
	clc		if length + catCol >= 80 then
	adc	catCol
	cmp	#80
	blt	no1
	lda	catCol	  if catCol <> 0 then
	beq	no1
	stz	catCol	    start a new line
	phx
	putcr
	plx
no1	anop

	clc		find the disp to the first char
	lda	catPathLength
	adc	#1
	tay
	short M
	stx	r4	save the length of the file name
no2	iny		write the file name
	lda	[r0],Y
	cmp	#':'
	bne	no2a
	lda	catChar
no2a	jsl	~stdout
	inc	catCol
	bne	no3
	inc	catCol+1
no3	dex
	bne	no2
	long	M
	lda	catCol	if catCol < 80-16 then
	cmp	#80-16
	bge	no5
no4	lda	#' '	  tab to next 16 column boundary
	jsl	~stdout
	inc	catCol
	lda	catCol
	and	#$000F
	bne	no4
	rts		  return

no5	stz	catCol	start a new line
	putcr
	rts		return
;...............................................................
;
;  Write the 'standard' catalog format
;
;...............................................................
;
;  Write the file name
;
lb1	lda	catFlags+2	set the allInfo flag
	and	#^set_l
	sta	allInfo
	bne	fc1	branch if doing a full listing

	jsr	PrintName	Name
	jsr	PrintFileType	FileType
!			BlocksUsed
	add4	nxBlocksUsed,nxResourceBlocks
	put4	nxBlocksUsed,#8
	putc	#' '
	jsr	PrintModDate	ModDateTime
	jsr	PrintCreateDate	CreateDateTime
	putc	#' '
	jsr	PrintAccess	Access
	jsr	PrintAuxType	AuxType
	putcr
	rts
;...............................................................
;
;  Write a full catalog listing
;
;...............................................................
;
fc1	puts	#'Name         : '	Name
	jsr	PrintName
	puts	#'Storage Type : '	StorageType
	put2	nxStorageType,cr=t
	puts	#'File Type    : '	FileType
	jsr	PrintFileType
	putcr
	puts	#'Aux Type     : '	AuxType
	jsr	PrintAuxType
	putcr
	puts	#'Access       : '	Access
	jsr	PrintAccess
	puts	#'  $'
	lda	nxAccess
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout
	putcr
	puts	#'Mod Date     : '	ModDateTime
	jsr	PrintModDate
	putcr
	puts	#'Create Date  : '	CreateDateTime
	jsr	PrintCreateDate
	putcr
	puts	#'Blocks Used  : '	BlocksUsed
	put4	nxBlocksUsed,cr=t
	puts	#'Data EOF     : '	EOF
	move4 nxEOF,r0
	jsr	WriteHex4
	putcr
	puts	#'Res. Blocks  : '	ResourceBlocks
	put4	nxResourceBlocks,cr=t
	puts	#'Res. EOF     : '	ResourceEOF
	move4 nxResourceEOF,r0
	jsr	WriteHex4
	putcr
	putcr
	rts
;...............................................................
;
;  Subroutines to print specific fields
;
;...............................................................
;
;  PrintAccess
;
PrintAccess anop

	short I,M
	ldy	#8	initialize the string
	lda	#' '
ac1	sta	buff1+1,Y
	dey
	bpl	ac1
	lda	#8
	sta	buff1+1

	lda	nxAccess	set the access flags
	bpl	ac2
	lda	#'D'
	sta	buff1+2
ac2	bit	nxAccess
	bvc	ac3
	lda	#'N'
	sta	buff1+3
ac3	lda	nxAccess
	and	#%00100000
	beq	ac4
	lda	#'B'
	sta	buff1+4
ac4	lda	nxAccess
	and	#%00000010
	beq	ac5
	lda	#'W'
	sta	buff1+5
ac5	lda	nxAccess
	lsr	a
	bcc	ac6
	lda	#'R'
	sta	buff1+6
ac6	lda	nxAccess
	and	#%00000100
	beq	ac7
	lda	#'I'
	sta	buff1+7
ac7	long	I,M
	puts	buff1	write the flag field
	rts
;
;  PrintAuxType
;
PrintAuxType anop

	lda	#9	initialize the string
	sta	buff1+1
	move	#' ',buff1+2,#9

	lda	nxFileType	write the language name
	cmp	#$B0	if this is a source file then
	bne	ax4
	lda	nxAuxType
	ora	#$8000
	jsr	FindCommand2	  if the langauge does not exist then
	bcc	ax4	    go write hex value
	short I,M	  write the language name
	lda	[r0]
	tay
ax1	lda	[r0],Y
	sta	buff1+1,Y
	dbne	Y,ax1
	ldy	#9	remove trailing blanks
	lda	#' '
ax2	cmp	buff1+1,Y
	bne	ax3
	dec	buff1+1
	dey
	bne	ax2
ax3	long	I,M
	puts	buff1	write the aux field
	lda	allInfo	  if not allInfo then
	beq	ax5	    skip the hex type
	puts	#'  '

ax4	lda	nxAuxType	write aux field as a hex value
	sta	r0
	jsr	WriteHex2
ax5	rts
;
;  PrintCreateDate - print the create date
;
PrintCreateDate anop

	ldx	#6	write the mod date
cd1	lda	nxCreateDateTime,X
	sta	r0,X
	dex
	dex
	bpl	cd1
	jsr	Time
	puts	buff1
	rts
;
;  PrintFileType - print the file type
;
PrintFileType anop

	lda	catFlags	if -t flag is set then
	and	#set_t
	bne	ft3	  print as hex

	lla	r4,typecode-4	scan the file type list for a letter
	ldy	#0	 file type
ft1	add4	r4,#4
	lda	[r4],Y
	and	#$00FF
	beq	ft3
	cmp	nxFileType
	bne	ft1
	ldy	#1	letter designator found - write it
ft2	lda	[r4],Y
	jsl	~stdout
	iny
	cpy	#4
	bne	ft2
	lda	allInfo	if allInfo then
	beq	ft4
	puts	#'       '	  write the hex filetype, too

ft3	lda	#'$'	no designator - print as hex
	jsl	~stdout
	lda	nxFileType
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout

ft4	lda	allInfo	if not allInfo then
	bne	ft7
	lda	nxStorageType	  if the file is an extended file then
	cmp	#5
	bne	ft5
	lda	#'+'	    write a +
	bra	ft6	  else
ft5	lda	#' '	    write a ' '
ft6	jsl	~stdout
ft7	rts
;
;  PrintModDate - print the mod date
;
PrintModDate anop

	ldx	#6	write the mod date
md1	lda	nxModDateTime,X
	sta	r0,X
	dex
	dex
	bpl	md1
	jsr	Time
	puts	buff1
	rts
;
;  PrintName - print the name field
;
PrintName add4 nxPath,#2,r0	find the start of the file name in the
	sec		 path name
	lda	[r0]
	sbc	catPathLength
	tax
	clc
	lda	catPathLength
	adc	#1
	tay
	short M
	stx	r4	save the length of the file name
pn1	iny		write the file name
	lda	[r0],Y
	cmp	#':'
	bne	pn1a
	lda	catChar
pn1a	jsl	~stdout
	dex
	bne	pn1
	long	M
	lda	allInfo	if allInfo then
	beq	pn2
	putcr		  writeln
	bra	pn5
pn2	ldx	r4	else if the file is < 16 chars then
	cpx	#16
	bge	pn4
	lda	#' '	  padd with blanks
pn3	jsl	~stdout
	inx
	cpx	#16
	blt	pn3
	bra	pn5	else
pn4	putcr		  skip to a new line
	prbl	#16	  skip past the name field
pn5	anop		endif
	rts
;...............................................................
;
;  Utility subroutines
;
;...............................................................
;
;  WriteHex2 - write a 2-byte hex number
;
WriteHex2 lda	#'$'
	jsl	~stdout
	bra	wx1
;
;  WriteHex4 - write a 4-byte hex number
;
WriteHex4 lda	#'$'
	jsl	~stdout
	lda	r3
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout
	lda	r2
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout
wx1	lda	r1
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout
	lda	r0
	short I,M
	jsr	hex
	long	I,M
	pha
	txa
	jsl	~stdout
	pla
	jsl	~stdout
	rts
;
;  Convert a hex value into a character
;
	longa off
	longi off
Hex	pha
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	jsr	HexChar
	tax
	pla
	and	#$F
;
;  HexChar - convert a value to a hex character
;
HexChar	ora	#'0'
	cmp	#'9'+1
	blt	hx1
	adc	#'A'-'9'-2
hx1	rts
;
;  Local data
;
allInfo	ds	2	print all available info?

	longa on
	longi on
	end

****************************************************************
*
*  YesOrNo - Get a 'y'es or 'n'o answer
*
*  Outputs:
*	A - Y, N or Q
*
****************************************************************
*
YesOrNo	private
	debug YesOrNo
	using Common

	gets	line,cr=t	get the line
	lda	line+1	skip to first non-white char
	and	#$00FF
	beq	fy1
	tax
	ldy	#2
lb1	lda	line,Y
	and	#$00FF
	jsl	IsWhite
	bcc	lb2
	iny
	dex
	bne	lb1
	bra	fy1

lb2	jsl	ToUpper	convert to uppercase
	cmp	#'Y'
	beq	rts
	cmp	#'N'
	beq	rts
	cmp	#'Q'
	bne	fy1
rts	rts

fy1	puts	#'Please answer yes, no or quit (Y, N or Q): '
	bra	YesOrNo

line	dc	i1'5,0',5c' '
	end
