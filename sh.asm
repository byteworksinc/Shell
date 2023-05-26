	keep	obj/sh
	mcopy sh.macros
	title 'ORCA/M Shell 2.0 Source ** CONFIDENTIAL **'
****************************************************************
*
*  ORCA/M SHELL 2.0
*  APW SHELL 2.0
*  November 1988
*
*  By Phil Montoya and Mike Westerfield
*
*  Copyright 1986-1988
*  By the Byte Works, Inc.
*
*  The source code for APW is protected by trade secret.  It
*  cannot be released or used in any way except for building
*  APW and archiving the source without the written permission
*  of the Byte Works, Inc.
*
****************************************************************
*
main	start
	copy	sh.dp
	using common

	phb		save old data bank reg
	phk		set data bank register
	plb
	sta	user_id	save my user ID
	sta	~user_id
	tsc		save stack pointer
	sta	sh_my_stack
	sub2	sh_my_stack,#$2FE,sh_low_stack compute stack boundries
	add2	sh_low_stack,#$100,sh_min_stack
	tdc		save location of my DP
	sta	sh_my_dp
	jsr	Initialize	initialize the shell
	brl	Monitor	enter command processor

~user_id entry
	ds	2
traceMode entry
	dc	i'0'
	end

****************************************************************
*
*  StackFrame - this segment is the stack frame for the shell
*
****************************************************************
*
StackFrame start Stack

	kind	$0012

	ds	1024
	end

****************************************************************
*
*  Variables
*
*  Variables are nested, with each EXEC file level having its
*  own set of variables.  For that reason, the variables are
*  implemented as a linked list of variable files.
*
*  The global variable V_POINTER is the pointer to the active
*  variable file.
*
*  Each variable file is a series of variable records. One for
*  each variable.  The structure of the record is as follows:
*
*	Byte	Length  Use
*	----	------  ---
*	0	4       pointer to next variable
*	4	4       pointer to null terminated name of variable
*	8	4       pointer to null terminated value of variable
*	12	2       export flag
*
*  Since each call has it's own variable table the V_NEXT is the
*  pointer to the next variable pointer record.  This is important
*  inorder to clean up and restore level 0 variable table after a
*  restet.
*
****************************************************************
	eject
****************************************************************
*
*  COMMON - Common Data Area
*
****************************************************************
*
Common	data
;
; Constants
;
buffSize equ	$2004	max size of a GS/OS path name
optionBuffSize equ $404	max size of a GS/OS option list

max_dcb	equ	56	Maximum length of DCB (must be even)

outofmem equ	$0201	out of memory error

RETURN	equ	$0D	RETURN key code
TAB	equ	$09	TAB key code

ins_stack_size equ 256	size of instruction stack
eline_len equ	2048	expanded line length
max_lines equ	20	# lines in line buffer (must be even)
pro_entry equ	$E100A8	ProDOS entry point
pro_stack_entry equ $E100B0	ProDOS stack entry point

!			file type names and numbers
TXT	equ	$04
SRC	equ	$B0
OBJ	equ	$B1
LIB	gequ	$B2
 
!			define set constants
	lclc	&c
	lclc	&a
	lcla	&i
	lcla	&n
&a	setc	abcdefghijklmnopqrstuvwxyz
&i	seta	1
.common1
&c	amid	&a,&i,1
&n	seta	+($40000000|-(&i-1))|1
set_&c	equ	&n
&i	seta	&i+1
	aif	&i<=26,^common1

!			Statement control loop
endsy	equ	0	  end of stack pointer
loopsy	equ	1	  loop node
ifsy	equ	2	  if node
forsy	equ	3	  for node

!			Displacements into command table entry
disp_restart gequ 0	  reastartable flag
disp_user_id gequ 2	  last known user ID
disp_cnum gequ 4	  command number
disp_name gequ 6	  name (length byte followed by ascii)
;
;  File types and thier associated identifiers
;
typecode dt	01,BAD
	dt	02,PCD
	dt	03,PTX
	dt	04,TXT
	dt	05,PDA
	dt	06,BIN
	dt	07,FNT
	dt	08,FOT
	dt	09,BA3
	dt	0A,DA3
	dt	0B,WPF
	dt	0C,SOS
	dt	0F,DIR
	dt	10,RPD
	dt	11,RPI
	dt	12,AFD
	dt	13,AFM
	dt	14,AFR
	dt	15,SCL
	dt	16,PFS
	dt	19,ADB
	dt	1A,AWP
	dt	1B,ASP
	dt	20,TDM
	dt	2A,8SC
	dt	2B,8OB
	dt	2C,8IC
	dt	2D,8LD
	dt	2E,P8C
	dt	42,FTD
	dt	50,GWP
	dt	51,GSS
	dt	52,GDB
	dt	53,DRW
	dt	54,GDP
	dt	55,HMD
	dt	56,EDU
	dt	57,STN
	dt	58,HLP
	dt	59,COM
	dt	5A,CFG
	dt	5B,ANM
	dt	5C,MUM
	dt	5D,ENT
	dt	5E,DVU
	dt	6B,BIO
	dt	6D,TDR
	dt	6E,PRE
	dt	6F,HDV
	dt	A0,WPC
	dt	AB,GSB
	dt	AC,TDF
	dt	AD,BDF
	dt	AF,TOK
	dt	B0,SRC
	dt	B1,OBJ
	dt	B2,LIB
	dt	B3,S16
	dt	B4,RTL
	dt	B5,EXE
	dt	B6,PIF
	dt	B7,TIF
	dt	B8,NDA
	dt	B9,CDA
	dt	BA,TOL
	dt	BB,DRV
	dt	BC,LDF
	dt	BD,FST
	dt	BF,DOC
	dt	C0,PNT
	dt	C1,PIC
	dt	C2,ANI
	dt	C3,PAL
	dt	C5,OOG
	dt	C6,SCR
	dt	C7,CDV
	dt	C8,FON
	dt	C9,FND
	dt	CA,ICN
	dt	D5,MUS
	dt	D6,INS
	dt	D7,MDI
	dt	D8,SND
	dt	DB,DBM
	dt	E0,LBR
	dt	E2,ATK
	dt	EE,R16
	dt	EF,PAS
	dt	F0,CMD
	dt	F5,DMP
	dt	F9,OS6
	dt	FA,INT
	dt	FB,IVR
	dt	FC,BAS
	dt	FD,VAR
	dt	FE,REL
	dt	FF,SYS
	dc	i1'0'
;
;  Local Storage
;
application_id ds 2	application's ID
appName	ds	17	application name buffer (15 char max)
caseSensitive ds 2	is CompareOSStrings case sensitive?
reset_app ds	2	reset application?
clang	dc	i'3'	current language number
cline	ds	2	disp into 20 line line buffer
cthandle ds	4	command table handle
ctlast	ds	4	pointer to last command table
!			 entry found by FindCommand
ctpointer ds	4	pointer to command table
exec_exit ds	2	exit EXEC early?
exec_level ds	2	exec file level
exec_return ds 2	EXEC return code for early exit
executeCmd ds	2	EXECUTE command?
stop_flag ds	2	stop (abort) exec flag
filelist ds	4	head of list of fastfile files
o_error	ds	2	error number for redirected output
lastStatus ds	2	last status value set
line_num ds	4	line counter used by type
line_hand ds	4	handle of 20 line line buffer
non_ascii ds	2	non_ascii allowed from type?
one	dc	i'1'	for exit compares by RETURN
osVersion ds	2	GS/OS version
regs	ds	28	DP storage for shell routines
setUsed	ds	2	has a shell variable been set?
sh_apstk ds	2	application stack space
sh_my_dp ds	2	my DP storage location
sh_my_stack ds 2	my STACK location
sh_low_stack ds 2	lowest STACK location
sh_min_stack ds 2	minimum threshold for shell call
skipping ds	2	print lines during echo?
systabl	dw	SYSTABL	command table name
tabLanguage ds 2	tab language
tabs	ds	256	current tab line
user_call_stack ds 2	S at call to user program (for QUIT)
user_id	ds	2	shell's user id #
volumeName dc	i'35,0',31c' '	general use volume name area
waitFlag	ds	2	wait on keypress in Pause?
;
;  Large buffers reserved for general use
;
buff1	ds	line_len	GP work buffer 1
	ds	1
;
;  wildcard list work areas
;
wcfileList ds	4	ptr to 1st file list entry

wcLength equ	54	length of a record

wcNext	ds	4	next entry
wcPath	ds	4	ptr to full path name
wcAccess ds	2	access attribute
wcFileType ds	2	file type
wcAuxType ds	4	aux type
wcStorageType ds 2	storage type
wcCreateDateTime ds 8	create date
wcModDateTime ds 8	mod date
wcOptionList ds 4	option list pointer
wcEOF	ds	4	EOF
wcBlocksUsed ds 4	blocks used
wcResourceEOF ds 4	resource fork EOF
wcResourceBlocks ds 4	resource fork block count

nxRec	dc	i'12'	12 entries total
nxPath	ds	4	ptr to full path name
nxAccess ds	2	access attribute
nxFileType ds	2	file type
nxAuxType ds	4	aux type
nxStorageType ds 2	storage type
nxCreateDateTime ds 8	create date
nxModDateTime ds 8	mod date
nxOptionList ds 4	option list pointer
nxEOF	ds	4	EOF
nxBlocksUsed ds 4	blocks used
nxResourceEOF ds 4	resource fork EOF
nxResourceBlocks ds 4	resource fork block count
;
;  Language info interface section
;
ldpList	ds	4	langauge dependent parameter list

li_dcb	dc	i'11'	Linfo DCB
li_saddr ds	4	addr of source file name
li_oaddr ds	4	addr of output file name
li_paddr ds	4	addr of parameter list
li_laddr ds	4	addr of language specific input string
li_merr	dc	i1'0'	max error level allowed
li_merrf dc	i1'0'	max error level found
li_ops	dc	b'0000 0100'	operations flag
li_keep	dc	i1'0'	keep flag
li_minus ds	4	set of letters selected with -
li_plus	ds	4	set of letters selected with +
li_org	dc	i4'0'	origin
;
;  User modifyable vectors
;
edit_vector dc i1'$5C',a3'EDIT_ENTRY'	edit command processor
cout_vector dc i1'$5C',a3'ChOut'	console out vector
mntr_vector dc i1'$5C',a3'MNTR_ENTRY'	error entry for monitor
;
;  Multiple compile names variables
;
firsttime ds	2	first time through flag
expkeep	ds	2	explicit keep defined
multiple ds	2	multple names flag
nameListPtr ds 4	list of names to compile
objPtr	ds	4	list of object file names
;
;  File names
;
bs		dosw  ':BLANK'	default name for new disk
console		dosw  .CONSOLE	console device name
editor		dosw  '15:EDITOR'	system editor
helpDir		dosw  '17:HELP:'	help file directory
linker		dosw  '16:LINKER'	command line linker
linked		dosw  '16:LINKED'	scriptable linker
prefix0		dosw  '8:'	current prefix
systabs		dosw  '15:SYSTABS'	system tab file
util		dosw  '17:'	utility prefix
;
;  Shell variable names
;
command		dosw  Command	name of last command
echoStr		dosw  ECHO	echo command lines?
exitStr		dosw  Exit	exit on non-zero {Status}?
In		dosw  IN	IN string; used in for loops
insert		dosw  Insert	Insert mode flag
linkName		dosw  LinkName	default linker keep name
keepName		dosw  KeepName	default compiler keep name
parameters	dosw  Parameters	name of parameters variable
pound		dosw  '#'	#; set to # of exec paramaters
prompt		dosw  Prompt	command line prompt
scroll		dosw  Scroll	scroll lines variable
separator	dosw  Separator	path name separator
status		dosw  Status	status of last command
trueStr		dosw  True	boolean true
;
;  Multi-use constant DCB's
;
svRec	dc	i'3',a4'exitStr,trueStr',i'0' "set exit true"
	end

****************************************************************
*
*  RECCOM - Common Data Area for Reset Records
*
****************************************************************
*
RecCom	data

type_ptr		ds    4	type buffer pointer
;
;  DCBs
;
exec_next 	ds    4	pointer to next exec record
exec_ptr		ds    4	pointer to exec string
exec_disp 	ds    2	displacement into exec string

ins_next		ds    4	pointer to next instruction stack record
ins_ptr		ds    4	pointer to instruction stack
ins_disp		ds    2	disp in instruction stack
ins_size		ds    2	current size of instruction stack

ex_next		ds    4	pointer to next exec file record
ex_ptr		ds    4	pointer to exec file

v_next		ds    4	pointer to next variable record
v_pointer 	ds    4	pointer to variable list

a_next		ds    4	pointer to next alias record
a_pointer 	ds    4	pointer to alias list
	end

****************************************************************
*
*  DoLogin - Read and execute the LOGIN command file
*
****************************************************************
*
DoLogin	private
	debug DoLogin
	using Common
	using RecCom
;
;  Read the LOGIN file.
;
	OSSet svRec	set EXIT to true
	lla	op_path,login	open the command table file
	open	op_dcb
	jcs	done
	lda	op_ref	save ref #
	sta	cl_ref
	sta	rd_ref
	sta	gt_ref
	get_eof gt_dcb	find spot for file
	jcs	err1
	lda	gt_mark
	ldx	gt_mark+2
	clc
	adc	#2
	bcc	lb1
	inx
lb1	jsl	MLalloc
	sta	fhandle
	stx	fhandle+2
	ora	fhandle+2
	jeq	err1
	move4 fhandle,rd_data
	move4 gt_mark,rd_req	read the file
	read	rd_dcb
	jcs	err
	close cl_dcb
	add4	rd_data,gt_mark,r0	set eof mark
	lda	#0
	sta	[r0]
;
;  Execute the commands
;
	move4 rd_data,exec_cmn	set addr of buffer
	stz	exec_flag
	execute exec_dcb	do it
	free	fhandle	get rid of file
	PushVariables 0	export LOGIN variables
	stz	v_next	set level to zero
	stz	v_next+2
	jsr	PushAlias	push the alias's
	stz	a_next	set level to zero
	stz	a_next+2
done	rts
;
;  Handle errors.
;
err	free	fhandle
err1	close cl_dcb
err2	puts	#'Error processing LOGIN file:',cr=t,errout=t
	rts
;
;  Local data
;
fhandle	ds	4	file handle
login	dw	'15:LOGIN'

exec_dcb anop		execute DCB
exec_flag ds	2	  flags
exec_cmn ds	4	  address of commands

cl_dcb	anop		close DCB
cl_ref	ds	2

gt_dcb	anop		get EOF DCB
gt_ref	ds	2
gt_mark	ds	4

rd_dcb	anop		read DCB
rd_ref	ds	2
rd_data	ds	4
rd_req	ds	4
rd_tran	ds	4

op_dcb	anop		open DCB
op_ref	ds	2
op_path	ds	4
op_buf	ds	4
	end

****************************************************************
*
*  Initialize - Initialize The ORCA Shell
*
****************************************************************
*
Initialize private
	debug Initialize
	using Common
	using RecCom

	OSGet_Version vrRec	get the version number
	jcs	error
	lda	vrVersion	check the version number
	and	#$7FFF
	sta	osVersion
	cmp	#$0300
	bge	lb1
	puts	#'Requires version 3.0 or later of GS/OS.',cr=t
	jmp	TermError

lb1	jsr	InitTools	initialize the tools
	jsr	InitMM	initialize our memory manager
	lda	#-1	no tab line, yet
	sta	tabLanguage
	stz	reset_app	not in an application
	stz	caseSensitive	string compares are not case sensitive
	stz	skipping	not skipping to an end
	stz	exec_level	no exec files, yet
	stz	ex_next	ex_next = nil
	stz	ex_next+2
	stz	v_next	v_next = nil
	stz	v_next+2
	stz	a_next	a_next = nil
	stz	a_next+2
	stz	exec_next	exec_next = nil
	stz	exec_next+2
	stz	ins_next	ins_next = nil
	stz	ins_next+2
	stz	type_ptr	type_ptr = nil
	stz	type_ptr+2
	stz	fileList	fileList = nil
	stz	fileList+2
	stz	ldpList	ldpList = nil
	stz	ldpList+2
	stz	li_saddr	li_saddr = nil
	stz	li_saddr+2
	stz	li_oaddr	li_oaddr = nil
	stz	li_oaddr+2
	stz	li_paddr	li_paddr = nil
	stz	li_paddr+2
	stz	li_laddr	li_laddr = nil
	stz	li_laddr+2
	stz	nameListPtr	nameListPtr = nil
	stz	nameListPtr+2
	lda	#1	be sure to set {status}
	sta	setUsed
	stz	lastStatus
	sta	waitFlag	wait on keypress in Pause

	stz	lp	initialize the line handler
	stz	lp+2
	stz	ep
	stz	ep+2

	lda	#9	get prefix 9
	sta	pr1Num
	ph4	#pr1
	jsr	GetPrefix
	tax
	jne	error
	move4 pr1Name,r0	if it is not set then
	lda	[r0]
	bne	lb2
	free	pr1Name	  free the name buffer
	lda	#1	  get prefix 1
	sta	pr1Num
	ph4	#pr1
	jsr	GetPrefix
	tax
	jne	error
	lda	#9	  set prefix 9 to the value of prefix 1
	sta	pr1Num
	OSSet_Prefix pr1
	jcs	error
lb2	free	pr1Name	free the name buffer
	lda	#7	set prefixes 0 to 7 to null
	sta	prNull+2
lb3	OSSet_Prefix prNull
	jcs	error
	dec	prNull+2
	bpl	lb3

	OSSet_Prefix pr0	set the system prefixes
	jcs	error
	OSSet_Prefix pr2
	jcs	error
	OSSet_Prefix pr3
	jcs	error
	OSSet_Prefix pr4
	jcs	error
	OSSet_Prefix pr5
	bcs	error
	OSSet_Prefix pr6
	bcs	error
	OSSet_Prefix pr7
	bcs	error

	jsr	TrapProDOS	trap the ProDOS vector

	jsr	IOInit	initialize the I/O handler

	malloc #max_lines*(line_len+1) reserve a line buffer
	sta	line_hand
	sta	r0
	stx	line_hand+2
	stx	r2
	ora	r2
	bne	in0
	lda	#outofmem
	brl	error
in0	move4 line_hand,r0	set it to 0
	ldy	#max_lines*(line_len+1)-2
	lda	#0
in1	sta	[r0],Y
	dey
	dey
	bpl	in1
	jsr	InitConsole	initialize console drivers
	bcs	error
	stz	a_pointer	initialize alias list
	stz	a_pointer+2
	jsr	SplashScreen	bring up the screen
	lla	r0,cName	read in the command table
	jsr	ReadSYSCMND
	jsr	DoLogin	execute the LOGIN file
	OSSet svRec	set EXIT to true
	rts

error	jsl	SysError	load error
	jmp	TermError
;
;  Local data area
;
pr1	dc	i'2'	for setting prefix 9
pr1Num	ds	2
pr1Name	ds	4

prNull	dc	i'2,0',a4'null'	for zeroing a prefix
null	dc	i'0'

pr0	dc	i'2,8',a4'work'	standard prefixes
pr2	dc	i'2,13',a4'libraries'
pr3	dc	i'2,14',a4'work'
pr4	dc	i'2,15',a4'system'
pr5	dc	i'2,16',a4'languages'
pr6	dc	i'2,17',a4'utilities'
pr7	dc	i'2,18',a4'work'

system	dosw	'9:SHELL'	default prefix values
utilities dosw '9:UTILITIES'
languages dosw '9:LANGUAGES'
libraries dosw '9:LIBRARIES'
work	dosw	'9:'

vrRec	dc	i'1'	OSGet_Version record
vrVersion ds	2

cName	dosw	'15:SYSCMND'	name of the command table
	end

****************************************************************
*
*  InitTools - Initialize toolkits
*
*  Outputs:
*	C - set if error
*
****************************************************************
*
InitTools private
	debug InitTools
	using Common

	_TLStartup	start up tool locator
	ph2	#0	set up memory manager
	_MMStartup
	pla
	ora	#$100	make sure we use aux ID
	sta	User_id
	sta	~User_id
	rts
	end

****************************************************************
*
*  MakeRoom - make room at the bottom of the screen
*
*  Notes: This is a work-around to a bug in 5.0.4.  It can be
*	removed once 5.0.4 is no longer in use.
*
****************************************************************
*
MakeRoom	private
	debug MakeRoom
	using Common
	using IOCom

	ldx	#14	save those critical values
lb0	lda	r0,X
	sta	tr0,X
	dex
	dex
	bpl	lb0
	move4	p1,tp1
	lda	#4	default count is 4
	sta	count
	lla	r12,scroll	if {Scroll} <> nil then
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb1
	lda	[r8]                       if length({Prompt}) = "1" then
	cmp	#1
	bne	lb1
	ldy	#2	    if isDigit({Prompt}) then
	lda	[r8],Y
	and	#$00FF
	cmp	#'0'
	blt	lb1
	cmp	#'9'+1
	bge	lb1
	and	#$000F	      set count
	sta	count
lb1	lda	consoleRefnum	print count down arrows
	sta	dnRefnum
	sta	upRefnum
	lda	count
	beq	lb4
	sta	loop
lb2	~OSWrite dnRec
	dec	loop
	bne	lb2
lb3	~OSWrite upRec	print count up arrows
	dec	count
	bne	lb3
lb4      move4	tp1,p1	restore DP values
	ldx	#14
lb5	lda	tr0,X
	sta	r0,X
	dex
	dex
	bpl	lb5
	rts
;
;  Local data
;
dnRec	dc	i'4'	write DCB for down arrow
dnRefnum ds	2
	dc	a4'down'
	dc	i4'1'
	ds	4

down	dc	i1'$0A'

upRec	dc	i'4'	write DCB for up arrow
upRefnum ds	2
	dc	a4'up'
	dc	i4'1'
	ds	4

up	dc	i1'$1F'

count	ds	2	line counter
loop	ds	2	loop counter
tr0	ds	16	temp dp area
tp1	ds	4	temp p1 area
	end

****************************************************************
*
*  Monitor - Line editor and command processor
*
****************************************************************
*
Monitor	start
	debug Monitor
	using Common

mn2	stz	stop_flag	initialize the stop flag
	jsr	StopSpin	stop the spinner
	putcr		start with a fresh line

	lla	r12,prompt	if {prompt} <> nil then
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	lb2
	lda	[r8]                       if {prompt} <> "#" then
	cmp	#1
	bne	lb1
	ldy	#2
	lda	[r8],Y
	and	#$00FF
	cmp	#'#'
	beq	lb3                        
lb1	ph4	r8                           print({prompt})
	ph2	#$2000
	jsl	PrintOSName
	bra	lb3                      else
lb2	cout	#'#'	  write out the prompt character

lb3	la	r0,line+1                read the line
	jsr	ReadLine
	putcr
	lda	line+1	nul terminate the line
	and	#$00FF
	tay
	lda	#0
	sta	line+2,y
	lla	exec_cmn,line+2	set up the execute DCB
	stz	exec_flag
	stz	exec_level
	Execute exec_dcb	execute the line
	brl	mn2

line	dc	i1'line_len,0'
	ds	line_len+2

exec_dcb anop		execute DCB
exec_flag ds	2	  flags
exec_cmn ds	4	  address of commands
	end

****************************************************************
*
*  ReadLine - Get A Line
*
*  Inputs:
*	outLine - addr to place line read
*
*  Note:	 This routine makes shell calls which disrupt the
*  direct page.  Be sure and save/restore critical values
*  before calling the shell!
*
****************************************************************
*
ReadLine private
	debug ReadLine
	using Common
	using IOCom

up_arrow equ	$0B	key codes
down_arrow equ $0A
right_arrow equ $15

outLine	equ	r0	addr of line
wp1	equ	r2	work pointer
;
;  Initialization
;
	lda	cline	eline := cline
	sta	eline
	lda	osVersion	if osVersion <= 3.03 then
	cmp	#$0304
	bge	in1
	jsr	MakeRoom
in1	anop		endif
;
;  Read a line
;
	lda	tlNum	include up/down arrows in terminator list
	sta	ntNum
	~OSD_Control ntDCB
	lda	#$8001	get the current input port values
	sta	code
	~OSD_Status dvDCB
	short M	use a blank fill
	lda	#' '
	sta	list
	stz	list+3	don't beep me in! (God, how I hate beeps!)
	stz	entry_type	initial entry
	lda	list+2	use overstrike, disable control charactes
	ora	#$01
	and	#$BF
	sta	list+1
	sta	list+2
	long	M
	ldx	#14	if insert <> nil then
fv1	lda	r0,X
	sta	tr0,X
	dex
	dex
	bpl	fv1
	move4	p1,tp1
	lla	r12,insert
	jsr	FindVariable
	lda	r0
	ora	r2
	beq	fv2
	lda	list+1                     use insert mode
	and	#$FEFE
	sta	list+1
fv2      move4	tp1,p1
	ldx	#14
fv3	lda	tr0,X
	sta	r0,X
	dex
	dex
	bpl	fv3
	lda	#$8000	set the parameters
	sta	code
	~OSD_Control dvDCB
	~OSD_Control frDCB	do a formatted read
lb0	lda	outLine	read a line
	inc	A
	sta	rdLine
	lda	#^ReadLine
	sta	rdLine+2
	lla	rdLen,254

	~OSD_Read rdDCB
	short I,M	set the length of the line
	lda	rdTrans
	sta	(outLine)
	long	I,M
;
;  Handle the various terminators
;
	lda	#$8001	get the input port values
	sta	code
	~OSD_Status dvDCB
	lda	last_Term	if last_Term = RETURN then
	and	#$007F
	cmp	#RETURN
	bne	tr2
	lda	(outline)	  if length(outLine) > 0 then
	and	#$00FF
	cmp	#1
	blt	rt0
	jsr	SaveLine	    save the line
	lda	cline	    ++cline
	jsr	IncLines
	sta	cline
rt0	lda	cursor_pos	  move to the end of the line
	and	#$00FF
	sta	cnt
	lda	(outLine)
	and	#$00FF
	sec
	sbc	cnt
	beq	rt2
	bmi	rt2
	sta	cnt
	lda	consoleRefnum
	sta	mrRefnum
rt1	~OSWrite mrRec
	dec	cnt
	bne	rt1
rt2	anop
	bra	fn1	  exit

tr2	cmp	#down_arrow	else if last_Term = down_arrow then
	bne	tr3
	jsr	Clear	  clear the characters typed
	lda	cline	  if cline = eline then
	cmp	eline
	bne	tr2a
	jsr	SaveLine	    save the current line
tr2a	lda	eline	  ++eline
	jsr	IncLines
	sta	eline
	jsr	SetLine	  set the default line
	brl	lb0	  go get the new input

tr3	cmp	#up_arrow	else if last_Term = up_arrow then
	bne	tr5
	jsr	Clear	  clear the characters typed
	lda	cline	  if cline = eline then
	cmp	eline
	bne	tr3a
	jsr	SaveLine	    save the current line
tr3a	sec		  --eline
	lda	eline
	sbc	#line_len+1
	bpl	tr4
	lda	#(line_len+1)*(max_lines-1)
tr4	sta	eline
	jsr	SetLine	  set the default line
	brl	lb0	  go get the new input

tr5	cmp	#right_arrow	else if last_Term = right_arrow then
	bne	tr6
	jsr	RightArrow	  expand the line
	brl	lb0	  go get the new input

tr6	anop		else
	jsr	Clear	  clear the characters typed
	jsr	fn2	  clear the default line
	ph2	outLine	  save critical variables
	SetStopFlag flDCB	  clear the stop flag
	pl2	outLine	  restore critical variables
	brl	lb0	  re-enter the string
;
;  Termination
;
fn1	~OSD_Control tlDCB	reset the terminator list
fn2	lda	tlNum	clear the default line
	sta	slNum
	stz	slLen
	~OSD_Control slDCB
	rts
;
;  Clear - clear the characters typed
;
Clear	ph2	outLine	save critical variables
	lda	#$15	make sure <- can wrap
	jsl	~stdout
	lda	#$1F
	jsl	~stdout
	lda	cursor_pos	set the loop counter
	and	#$00FF
	beq	cl2
	sta	wp1

cl1	lda	#8	back up one character
	jsl	~stdout
	dec	wp1	loop
	bne	cl1

cl2	lda	#$0B	clear to the end of the screen
	jsl	~stdout
	pl2	outLine	restore critical variables
	rts
;
;  IncLines - increment cline to point to the next line
;
IncLines clc
	adc	#line_len+1
	cmp	#(line_len+1)*max_lines
	bne	il1
	lda	#0
il1	rts
;
;  RightArrow - Handle right-arrow expansion
;
RightArrow anop
	lda	(outLine)	no match is possible if the length is 0
	and	#$00FF
	beq	mr2
	short M
	cmp	cursor_pos	don't check for match if not at EOL
	long	M
	bne	mr1
	cmp	#16	no match is possible if the length
	blt	ra0	 is >16

mr1	short M	if not at the end of the line then
	lda	(outLine)
	cmp	cursor_pos
	long	M
	beq	mr2
	inc	cursor_pos	  move right one character
	lda	#$8000	  set the parameters
	sta	code
	~OSD_Control dvDCB
mr2	rts

ra0	ldy	#1	set the case flag
	lda	(outline),Y
	and	#$0020
	sta	caseFlag
	short I,M	move the current name into a buffer
	lda	(outLine)
	tay
ra1	lda	(outLine),Y
	jsl	ToUpper
	sta	cmdLine,Y
	dey
	bpl	ra1
	long	I,M
	move4 ctPointer,wp1	init command table address
ra2	ldy	#disp_cnum+1	quit if we're at the end of the list
	lda	[wp1],Y
	beq	mr2
	short I,M	quit if we've got a match
	ldy	#disp_name
	lda	[wp1],Y
	cmp	cmdLine
	blt	ra5
	iny
	ldx	#0
ra3	lda	[wp1],Y
	jsl	ToUpper
	cmp	cmdLine+1,X
	bne	ra5
	iny
	inx
	cpx	cmdLine
	bne	ra3
	long	I,M
	bra	ra6
ra5	long	I,M
	ldy	#disp_name	next entry
	lda	[wp1],Y
	and	#$00FF
	clc
	adc	#disp_name+1
	adc	wp1
	sta	wp1
	bcc	ra2
	inc	wp1+2
	bra	ra2

ra6	anop		entry found - fill in cmdLine:
	ph4	wp1	clear the old characters
	jsr	Clear
	pl4	wp1
	short I,M
	ldy	#disp_name	get the # of chars
	lda	[wp1],Y
	sta	cmdLine
	ldx	#1	move the chars into cmdLine
ra7	iny
	lda	[wp1],Y
	ora	caseFlag
	sta	cmdLine,X
	inx
	dec	cmdLine
	bne	ra7
	lda	#' '	set the trailing blank
	sta	cmdLine,X
	short M	note that we have a new string
	stz	entry_type
	long	M
	lda	#$8000
	sta	code
	~OSD_Control dvDCB
	stx	cmdLine	set the line length
	stx	cursor_pos	set the cursor position
	long	I,M
	lda	tlNum	set the device number
	sta	slNum
	lla	slStr,cmdLine+1	set the string location
	lda	cmdLine	get the length of the string
	and	#$00FF
	sta	slLen
	~OSD_Control slDCB	set the default string
	rts

caseFlag ds	2	for setting the case of an output string
cmdLine	ds	17	for expanding the command line
;
;  SaveLine - save the current line at [wbuff+cline]
;
SaveLine clc		set wp1 to point to the line storage area
	lda	line_hand
	adc	cline
	sta	wp1
	lda	line_hand+2
	adc	#0
	sta	wp1+2
	short M	save the line
	ldy	#0
sv1	lda	(outLine),Y
	sta	[wp1],Y
	iny
	cpy	#line_len+1
	bne	sv1
	long	M
	rts
;
;  SetLine - set the default string to the one pointed to by eline
;
SetLine	lda	tlNum	set the device number
	sta	slNum
	clc		set the string location
	lda	line_hand
	adc	eline
	sta	wp1
	sta	slStr
	lda	line_hand+2
	adc	#0
	sta	wp1+2
	sta	slStr+2
	inc4	slStr
	lda	[wp1]	get the length of the string
	and	#$00FF
	sta	slLen
	~OSD_Control slDCB	set the default string
	rts
;
;  Local variables
;
ntDCB	anop		DCB for D_Control to set our terminator list
	dc	i'5'	parameter count
ntNum	ds	2	device number
	dc	i'$8001'	control code
	dc	a4'terms'	control list ptr
	dc	i4'l:tList+4'	request count
	dc	i4'0'	transfer count

terms	dc	i'$A07F,l:tList/2'
tList	dc	i'$001B,$000D,$000A,$000B,$2015'

flDCB	dc	i'0'	for SetStopFlag call

slDCB	anop		DCB for D_Control to set default string
	dc	i'5'	parameter count
slNum	ds	2	device number
	dc	i'$8004'	control code
slStr	ds	4	control list ptr
slLen	ds	4	request count
	dc	i4'0'	transfer count

eline	ds	2	disp into history buffer
tr0	ds	16	temp dp area
tp1	ds	4	temp p1 area

mrRec	dc	i'4'	write DCB for -> character
mrRefnum ds	2
	dc	a4'ch'
	dc	i4'1'
	ds	4

ch	dc	i1'$1C'

cnt	ds	2	loop counter
	end

****************************************************************
*
*  SplashScreen - Clears Screen And Writes Copyright Notice
*
****************************************************************
*
SplashScreen private
	debug SplashScreen

	home
	aif	apw,.a
	prbl	#30
	puts	#'ORCA/Shell 2.0.5 B3',cr=t
	prbl	#22
	puts	#'Copyright Byte Works, Inc. 1980-1999',cr=t
	ago	.b
.a
	prbl	#20
	puts	#'Apple IIGS Programmer''''s Workshop 2.0 D11',cr=t
	prbl	#22
	puts	#'Copyright Byte Works, Inc. 1980-1992',cr=t
	prbl	#20
	puts	#'Copyright Apple Computer, Inc. 1986-1992',cr=t
.b
	prbl	#30
	puts	          #'All Rights Reserved',cr=t
	putcr
	rts
	end

****************************************************************
*
*  Trace - Debug Trace Facility
*
*  If the global cons ant DEBUG is true this subroutine will
*  be placed in the code.  If the variable TRACE is true, it
*  will print the characters appearing o  the DEBUG macros
*  used in the program.
*
*  Inputs:
*	TRACE - trace on flag
*
****************************************************************
*
Trace	start
	using Common

	aif	debug=0,.a
	php		save variables
	long	i,m
	sta	la
	sty	ly
	stx	lx
	short m
	pla
	sta	lp
	long	m
	lda	r0
	sta	tr0
	pla		get the address of the characters
	sta	r0
	inc	r0
	lda	traceMode	quit if trace is off
	and	#$00ff
	jeq	lb2

* INSTALL CUSTOM DEBUG CODE HERE

	ldx	#10	write the characters
	ldy	#0
lb1	lda	(r0),y
	and	#$00ff
	jsl	~stdout
	iny
	dbne	x,lb1
	putcr

* RETURN TO CALLER

lb2	clc		get the return address
	lda	r0
	adc	#9
	pha
	lda	tr0	restore variables
	sta	r0
	ldy	ly
	ldx	lx
	short m
	lda	lp
	pha
	long	m
	lda	la
	plp
	rts

la	ds	2
lx	ds	2
ly	ds	2
lp	ds	1
tr0	ds	2
local	ds	2
.a
	end

****************************************************************
*
*  TrapProDOS - Traps The ProDOS Vector
*
****************************************************************
*
TrapProDOS private
	debug TrapProDOS
	using Common

	php		disable interrupts
	sei
	lda	>pro_entry	get and save ProDOS 16 vector
	sta	ProDOS
	lda	>pro_entry+2
	sta	ProDOS+2
	lda	jmp	set ProDOS vector pointing to shell
	sta	>pro_entry
	lda	jmp+2
	sta	>pro_entry+2

	lda	>pro_stack_entry	get and save ProDOS 16 stack vector
	sta	StackProDOS
	lda	>pro_stack_entry+2
	sta	StackProDOS+2
	lda	sjmp	set ProDOS stack vector pointing to shell
	sta	>pro_stack_entry
	lda	sjmp+2
	sta	>pro_stack_entry+2
	plp		enable interrupts
	rts

jmp	jml	ShellCall
sjmp	jml	StackShellCall
	end
