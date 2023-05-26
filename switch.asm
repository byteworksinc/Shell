	keep	obj/switch
	mcopy switch.macros
	copy	sh.dp
****************************************************************
*
*  Switch
*
*  This module contains the subroutines used by the COMPRESS
*  and SWITCH commands.
*
****************************************************************
*
*  SWCommon - common variables for this module
*
****************************************************************
*
SWCommon privdata

tpointer ds	4	directory pointer
filent	ds	2	file entry number
blkcnt	ds	2	block counter
ent1	ds	2	entry 1
ent2	ds	2	entry 2
bcnt1	ds	2	block counter 1
bcnt2	ds	2	block counter 2

clRec	dc	i'1'	OSClose record
clRefnum ds	2	directory file reference number

gdRec	dc	i'2'	OSGetDevNumber record
gdDevName ds	4
gdDevnum ds	2

rdRec	dc	i'4'	OSRead record
rdRefnum ds	2
rdDataBuffer ds 4
rdRequestCount ds 4
rdTransferCount ds 4

wbBlockDevnum ds 2	Read/Write Block record
wbBlockDataBuffer ds 4
wbBlockNum ds	4
;
;  Local variables for Alphabetize
;
al_index ds	2	index counter
al_curfcnt ds	2	entry counter
;
;  Local variables for Compare
;
cm_ch	ds	1	uppercase character buffer
cm_flag	ds	1	string size flag
;
;  Local data for Crunch
;
cr_filent1 ds	2	file counter for first entry
cr_filent2 ds	2	file counter for second entry
cr_blkcnt1 ds	2	block counter for first entry
cr_blkcnt2 ds	2	block counter for second entry
cr_curfcnt ds	2	entry counter
;
;  Local data for Find
;
fn_ch	ds	2	uppercase character buffer
fn_curfcnt ds	2
;
;  Local data for GetEntry
;
ge_fcount   ds	   2	active entry counter
;
;  Local data for ParentPointers
;
pp_temp	ds	4	temp storage for R0
pp_block ds	2
;
;  Local data for WriteDirectory
;
wd_blknum ds	2	current block
wd_nxtblk ds	2	next block
	end

****************************************************************
*
*  Local data for ReadDirectory
*
****************************************************************
*
ReadDirCommon privdata

error	ds	2	error number

inRec	dc	i'2'	OSInitWildcard record
inPathname ds	4
inFlags	dc	i'0'

prRec	dc	i'2'	OSGet_Prefix record
prPrefixnum dc i'0'
prPathname ds	4

opRec	dc	i'3'	OSOpen record
opRefnum ds	2
opPathname ds	4
	dc	i'$0001'

gtRec	dc	i'2'	OSGet_EOF record
gtRefnum ds	2
gtEOF	ds	4
	end

	datachk off
****************************************************************
*
*  Alphabetize - Alphabetize directory for compress
*
*  Inputs:
*	rdDataBuffer - address of dictionary file
*	rdTransferCount - length of file
*
*  Outputs:
*	Directory if alphabetized
*
****************************************************************
*
Alphabetize start seg2
	debug Alphabetize
	using Common
	using SWCommon
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24
mark1	equ	r12
mark2	equ	r16
entry	equ	r10

	move4 rdDataBuffer,r0	get the directory address
	ldy	#$23	get the directory information
	lda	[r0],Y
	and	#$00FF
	sta	elength
	iny
	lda	[r0],Y
	and	#$00FF
	sta	eperblk
	iny
	lda	[r0],Y
	sta	filecnt
	stz	al_curfcnt	init the file counter

cp1	lda	al_curfcnt	while not all first entries done
	cmp	filecnt
	bne	cp1a
rts	rtl

cp1a	sta	entry
	jsr	GetEntry	get the first entry
	move4 r0,mark1	save entry information
	lda	filent
	sta	ent1
	lda	blkcnt
	sta	bcnt1
	lda	al_curfcnt	get the next entry
	inc	a
	sta	al_index

cp2	lda	al_index	while not all second entries
	cmp	filecnt
	beq	cp7
	sta	entry
	jsr	GetEntry	get the second entry
	move4 r0,mark2	save entry information
	lda	filent
	sta	ent2
	lda	blkcnt
	sta	bcnt2
	jsr	Compare	compare the entries
	bcs	cp6
	jsr	ParentPointers	handle any possible back pointers
	short I,M
	ldy	#38	switch the entries
cp3	lda	[mark1],Y
	sta	buff1,Y
	dbpl	Y,cp3
	ldy	#38
cp4	lda	[mark2],Y
	sta	[mark1],Y
	dbpl	Y,cp4
	ldy	#38
cp5	lda	buff1,Y
	sta	[mark2],Y
	dbpl	Y,cp5
	long	I,M
cp6	inc	al_index	next entry
	brl	cp2

cp7	inc	al_curfcnt	next entry
	brl	cp1
	end

****************************************************************
*
*  Compare - Compare entries alphabetically
*
*  Inputs:
*	MARK1 - pointer to first entry
*	MARK2 - pointer to second entry
*
*  Outputs:
*	C - set if entry 1 less than or equal to entry 2
*
****************************************************************
*
Compare	private seg2
	debug Compare
	using common
	using SWCommon
mark1	equ	r12
mark2	equ	r16

	short I,M
	stz	cm_flag	see if entries are the same size
	lda	[mark1]
	and	#$0F
	sta	r4
	lda	[mark2]
	and	#$0F
	cmp	r4
	bgt	cm1	set physical length flag
	sta	r4
	inc	cm_flag
cm1	ldy	#0	compare strings
cm2	iny
	lda	[mark2],Y
	jsl	ToUpper
	sta	cm_ch
	lda	[mark1],Y
	jsl	ToUpper
	cmp	cm_ch
	bgt	cm3
	blt	cm4
	cpy	r4
	bne	cm2
	lda	cm_flag
	beq	cm4
cm3	long	I,M
	clc
	rts

cm4	long	I,M
	sec
	rts
	end

****************************************************************
*
*  Crunch - Crunch the directory for compress
*
*  Inputs:
*	rdRec - information about directory
*	rdRef - reference number for directory file
*	rdDate - address of dictionary file
*	rdTran - length of file
*
*  Outputs:
*	Directory if crunched.  All entries a move up to occupy
*	unused entry space.
*
****************************************************************
*
Crunch	start seg2
	debug Crunch
	using Common
	using SWCommon
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24
mark1	equ	r12
mark2	equ	r16

	move4 rdDataBuffer,r0
	ldy	#$23	get the directory information
	lda	[r0],Y
	and	#$00FF
	sta	elength
	iny
	lda	[r0],Y
	and	#$00FF
	sta	eperblk
	iny
	lda	[r0],Y
	sta	filecnt

	add4	r0,#4	get to the first entry
	jsr	nexte
	stz	cr_curfcnt	init the file counter
	stz	cr_blkcnt1
	lda	#2
	sta	cr_filent1

cpc1	lda	filecnt	while not all first entries done
	cmp	cr_curfcnt
	bne	cpc1a
	rtl

cpc1a	lda	[r0]	get the first blank entry
	and	#$00FF
	beq	cpc3
	inc	cr_curfcnt
	lda	cr_filent1
	cmp	eperblk
	bne	cpc2
	stz	cr_filent1
	inc	cr_blkcnt1
	add4	r0,#5
cpc2	jsr	nexte
	inc	cr_filent1
	bra	cpc1

cpc3	move4 r0,mark1	save first entry information
	lda	cr_blkcnt1
	sta	bcnt1
	sta	cr_blkcnt2
	lda	cr_filent1
	sta	ent1
	sta	cr_filent2

cpc4	lda	cr_filent2	get second valid entry
	cmp	eperblk
	bne	cpc5
	stz	cr_filent2
	inc	cr_blkcnt2
	add4	r0,#5
cpc5	jsr	nexte
	inc	cr_filent2

	lda	[r0]	see if entry is active
	and	#$00FF
	beq	cpc4
	move4 r0,mark2	save second entry information
	lda	cr_filent2
	sta	ent2
	lda	cr_blkcnt2
	sta	bcnt2

	jsr	ParentPointers	handle any possible back pointers
	short I,M
	ldy	#38	move the active entry
cpc6	lda	[mark2],Y
	sta	[mark1],Y
	dbpl	Y,cpc6
	lda	#0	deactivate this entry
	sta	[mark2]
	long	I,M
	move4 mark1,r0
	brl	cpc1
;
;  Advance to next entry
;
nexte	clc
	lda	r0
	adc	elength
	sta	r0
	bcc	nx1
	inc	r0+2
nx1	rts
	end

****************************************************************
*
*  Find - Find a file entry in the directory buffer
*
*  Inputs:
*	RD_DATA - address of directory buffer
*	R4 - pointer to entry name
*	ELENGTH - entry length
*	EPERBLK - entries per block
*	FILECNT - file count
*
*  Outputs:
*	R8 - pointer to entry
*
****************************************************************
*
Find	private seg2
	debug Find
	using Common
	using SWCommon
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24

	lda	[r4]	switch to a p-string
	xba
	sta	[r4]
	inc4	r4
	add4	rdDataBuffer,#4,r8	get the address of the first active entry
	jsr	nexte
	stz	fn_curfcnt	init current file counter
	stz	blkcnt	init block counter
	lda	#2	init # of entries
	sta	filent

fn1	lda	filecnt	while there are entries
	cmp	fn_curfcnt
	beq	rts	should never happen
	lda	[r8]	see if we have a file
	and	#$000F
	beq	fn4
	short I,M	see if we have a match
	cmp	[r4]
	bne	fn3
	tay
fn2	lda	[r8],Y
	jsl	ToUpper
	sta	fn_ch
	lda	[r4],Y
	jsl	ToUpper
	cmp	fn_ch
	bne	fn3
	dbne	Y,fn2
	long	I,M
rts	rts

fn3	long	I,M
	inc	fn_curfcnt
fn4	lda	filent
	cmp	eperblk
	bne	fn5
	stz	filent
	inc	blkcnt
	add4	r8,#5
fn5	jsr	nexte
	inc	filent
	brl	fn1
;
;  Get next entry
;
nexte	clc
	lda	r8
	adc	elength
	sta	r8
	bcc	nx1
	inc	r8+2
nx1	rts
	end

****************************************************************
*
*  GetEntry - Get a directory entry
*
*  Inputs:
*	rdDataBuffer - address of directory buffer
*	entry - entry number
*	eLength - entry length
*	ePerBlk - entries per block
*	fileCnt - file count
*
*  Outputs:
*	r0 - pointer to entry
*	filent - entry number
*	blkCnt - block number
*
****************************************************************
*
GetEntry private seg2
	debug GetEntry
	using Common
	using SWCommon
entry	equ	r10
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24

	move4 rdDataBuffer,r0	get the file address
	add4	r0,#4	get to first active entry
	jsr	NextE
	stz	ge_fcount	init current file counter
	stz	blkcnt	init block counter
	lda	#2	init # of entries
	sta	filent

fn1	lda	[r0]	check for a valid entry
	and	#$00FF
	beq	fn3
	lda	ge_fcount	is this the entry we want
	cmp	entry
	bne	fn2
	rts

fn2	inc	ge_fcount	update active entry counter
fn3	lda	filent
	cmp	eperblk
	bne	fn4
	stz	filent
	inc	blkcnt
	add4	r0,#5
fn4	jsr	NextE
	inc	filent
	brl	fn1
;
;  Get next entry
;
NextE	clc
	lda	r0
	adc	eLength
	sta	r0
	bcc	nx1
	inc	r0+2
nx1	rts
	end

****************************************************************
*
*  GetNum - get the current device number
*
*  Inputs:
*	nxPath - volume name of the device
*
*  Outputs:
*	gdDevnum - device number
*
****************************************************************
*
GetNum	private seg2
	debug GetNum
	using Common
	using SWCommon

	ph4	r4	get a volume number
	move4 nxPath,r4
	lda	[r4]
	pha
	ldy	#3
	tax
	dex
	short M
	lda	#':'
vn1	cmp	[r4],Y
	beq	vn2
	iny
	dex
	bne	vn1
vn2	long	M
	dey
	dey
	tya
	sta	[r4]
	move4 r4,gdDevname
	OSGet_Dev_Number gdRec
	pla
	sta	[r4]
	pl4	r4
	rts
	end

****************************************************************
*
*  ParentPointers - Handle parent pointers for SWITCH and COMPRESS
*
*  Inputs:
*	MARK1 - pointer to first file
*	MARK2 - pointer to second file
*	ENT1 - entry # of first file
*	ENT2 - entry # of second file
*	BCNT1 - block counter for the first file
*	BCNT2 - block counter for the second file
*
*  Outputs:
*	Adjusts backpointers
*
****************************************************************
*
ParentPointers private seg2
	debug ParentPointers
	using Common
	using SWCommon
mark1	equ	r12
mark2	equ	r16

	move4 r0,pp_temp	save R0
;
;  Handle first entry
;
	lda	[mark1]	check for active entry
	and	#$00FF
	beq	bk1
	ldy	#$10	see if it is a directory
	lda	[mark1],Y
	and	#$00FF
	cmp	#$0F
	bne	bk1

	iny		get the block number
	lda	[mark1],Y
	sta	wbBlockNum
	stz	wbBlockNum+2
	ldy	#$25	calculate block number
	lda	[mark1],Y
	sta	pp_block
	lda	bcnt2	if this is in the first block skip
	beq	bk1a
	dec	a
	xba		multiply by 512
	asl	a
	tay		save the index
	iny
	iny
	move4 tpointer,r0	get the directory pointer
	lda	[r0],Y
	sta	pp_block
bk1a	move4 mark1,r0
	jsr	Read	read in the block
	move4 wbBlockDatabuffer,r0	set the block pointer
	ldy	#$27
	lda	pp_block
	sta	[r0],Y
	iny
	iny
	short M
	lda	ent2
	sta	[r0],y
	long	M
	jsr	Write
;
;  Handle second entry
;
bk1	lda	[mark2]	check for active entry
	and	#$00FF
	beq	bk2
	ldy	#$10	see if it is a directory
	lda	[mark2],Y
	and	#$00FF
	cmp	#$0F
	bne	bk2

	iny		get the block number
	lda	[mark2],Y
	sta	wbBlockNum
	stz	wbBlockNum+2
	ldy	#$25	calculate block number
	lda	[mark1],Y
	sta	pp_block
	lda	bcnt1	if this is in the first block skip
	beq	bk2a
	dec	a
	xba		multiply by 512
	asl	a
	tay
	iny
	iny
	move4 tpointer,r0	get the directory pointer
	lda	[r0],Y
	sta	pp_block
bk2a	move4 mark2,r0
	jsr	Read	read in the block
	move4 wbBlockDatabuffer,r0	set the block pointer
	ldy	#$27
	lda	pp_block
	sta	[r0],Y
	iny
	iny
	short M
	lda	ent1	set the entry number
	sta	[r0],Y
	long	M
	jsr	Write
bk2	move4 pp_temp,r0	restore R0
	rts
;
;  Read in the block
;
Read	malloc #512
	sta	wbBlockDataBuffer
	stx	wbBlockDataBuffer+2
	Read_Block wbBlockDevnum
	rts
;
;  Write out the block
;
Write	Write_Block wbBlockDevnum
	free	wbBlockDatabuffer
	rts
	end

****************************************************************
*
*  ReadDirectory - Read in the directory
*
*  Inputs:
*	r0 - pointer to directory name (if null use current prefix)
*
*  Outputs:
*	A - error number
*	C - set for error, else clear
*	tpointer - pointer to directory file
*	NOTE : file remains open until WriteDirectory is called
*	clRefnum - directory file reference number
*	wbBlockDevnum - block number of the file
*
****************************************************************
*
ReadDirectory start seg2
	debug ReadDirectory
	using Common
	using SWCommon
	using ReadDirCommon
DIR	equ	$0F

	stz	nxPath	nxPath = nil
	stz	nxPath+2
	stz	tpointer	tpointer = nil
	stz	tpointer+2
	stz	error	error number

	move4 r0,nxPath	make sure file is a directory
	stz	nxOptionList
	stz	nxOptionList+2
	OSGet_File_Info nxRec
	sta	error
	jcs	rts
	lda	nxFileType
	cmp	#DIR
	beq	rd7
	ph4	nxPath
	ph2	#$4000
	jsl	PrintOSName
	puts	#' must be a directory',cr=t,errout=t
	dec	error
	brl	rts

rd7	jsr	GetNum	get the volume number
	lda	gdDevnum
	sta	wbBlockDevnum
	move4 r0,opPathname	open the file
	OSOpen opRec
	sta	error
	jcs	rts
	lda	opRefnum
	sta	clRefnum
	sta	gtRefnum
	sta	rdRefnum
	OSGet_EOF gtRec	get the length of the file
	sta	error
	jcs	rts

	mlalloc gtEOF	find a spot for the file
	sta	rdDataBuffer
	sta	tpointer
	stx	rdDataBuffer+2
	stx	tpointer+2
	ora	tpointer+2
	bne	rd8
	lda	#outOfMem
	sta	error
	bra	rts
rd8	move4 gtEOF,rdRequestCount	get the table's pointer
	OSRead rdRec	read in the file
	sta	error

rts	free	nxPath	free the path name buffer
	OSClose clRec	close the directory file
	lda	error	return the error code
	beq	rts1	if there was an error then
	free	tpointer	  free(tpointer)
rts1	lda	error
	cmp	#1
	rtl
	end

****************************************************************
*
*  SwitchFiles - Switch file entries
*
*  Inputs:
*	rdDataBuffer - address of directory buffer
*	r0 - pointer to name of first entry
*	r4 - pointer to name of second entry
*
*  Outputs:
*	Entries are switched in buffer
*
*  NOTE: By this time we know that the entries are valid and
*	exist.  There cannot be an error.
*
****************************************************************
*
SwitchFiles start seg2
	debug SwitchFiles
	using Common
	using SWCommon
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24
mark1	equ	r12
mark2	equ	r16

	lda	r0	save first file pointer
	sta	>first
	lda	r2
	sta	>first+2
	move4 rdDataBuffer,r0
	ldy	#$23	get the directory information
	lda	[r0],Y
	and	#$00FF
	sta	elength
	iny
	lda	[r0],Y
	and	#$00FF
	sta	eperblk
	iny
	lda	[r0],Y
	sta	filecnt
	jeq	rts

	jsr	Find	find the location of the first file
	move4 r8,mark1
	lda	filent
	sta	ent1
	lda	blkcnt
	sta	bcnt1
	ldy	#38	save the entry (ok to use long reg)
sw1	lda	[r8],Y
	sta	buff1,Y
	dbpl	Y,sw1
	lda	>first	find the location of the second file
	sta	r4
	lda	>first+2
	sta	r6
	jsr	Find
	move4 r8,mark2
	lda	filent
	sta	ent2
	lda	blkcnt
	sta	bcnt2
	jsr	ParentPointers	handle any possible back pointers
	short I,M	switch the entries
	ldy	#38
sw2	lda	[mark2],Y
	sta	[mark1],Y
	dbpl	Y,sw2
	ldy	#38
sw3	lda	buff1,Y
	sta	[mark2],Y
	dbpl	Y,sw3
	long	I,M
rts	rtl

first	ds	4	first file pointer
	end

****************************************************************
*
*  WriteDirectory - Write out the directory for Compress or Switch
*
*  Inputs:
*	r0 - pointer path name of directory or volume
*	rdDataBuffer - address of file
*
*  Outputs:
*	Directory file is written back to disk.
*	Directory file buffer is released.
*
*  Note: Prodos does not allow a directory file to be written to. So
*	we must do block writes.
*
****************************************************************
*
WriteDirectory start seg2
	debug WriteDirectory
	using Common
	using SWCommon
elength	equ	r20
eperblk	equ	r22
filecnt	equ	r24

	move4 rdDataBuffer,r0	get the directory address
	ldy	#2
	lda	[r0],Y
	sta	wd_nxtblk
	ldy	#$23	get the directory information
	lda	[r0],Y
	and	#$00FF
	sta	elength
	iny
	lda	[r0],Y
	and	#$00FF
	sta	eperblk
	iny
	lda	[r0],Y
	sta	filecnt
	jeq	rts	quit if there are no entries
;
;  Find first active entry to get starting block
;
	add4	r0,#4	get to the first entry
	jsr	nexte
	lda	#2
	sta	filent

wd4	lda	[r0]	find an active entry
	and	#$0F
	bne	wd6
	lda	filent
	cmp	eperblk
	bne	wd5
	stz	filent
	add4	r0,#5
wd5	jsr	nexte
	inc	filent
	bra	wd4

wd6	ldy	#37
	lda	[r0],Y
	sta	wd_blknum
	move4 rdDataBuffer,r0
;
;  Write out blocks until there are no more
;
wd7	lda	wd_blknum
	beq	rts
	sta	wbBlocknum	set the block number
	stz	wbBlocknum+2
	move4 r0,wbBlockDataBuffer	set the data buffer
	Write_Block wbBlockDevnum	write the block
	bcs	err
	lda	wd_nxtblk	get next block
	sta	wd_blknum
	add4	r0,#512
	ldy	#2
	lda	[r0],Y
	sta	wd_nxtblk
	bra	wd7

err	sta	>errNum	error return
	OSError errRec
	free	tpointer
	sec
	rtl

rts	free	tpointer
	clc
	rtl
;
;  Get next entry
;
nexte	clc
	lda	r0
	adc	elength
	sta	r0
	bcc	nx1
	inc	r0+2
nx1	rts
;
;  Local data
;
errRec	dc	i'1'	OSError call
errNum	ds	2
	end
