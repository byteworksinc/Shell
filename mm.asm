	keep	obj/mm
	mcopy mm.macros
****************************************************************
*
*  Memory Manager
*
*  This module contains the memory manager.
*
****************************************************************
	copy	sh.dp
	eject
****************************************************************
*
*  MMCommon - common data for the memory manager
*
****************************************************************
*
MMCommon privdata
;
;  The memory manager requires the following areas from direct page.
;  All areas are four bytes long.
;
!memory	 gequ	 $xx	 memory list
!freeList gequ	 $xx	 free memory list
!lastmem  gequ  $xx                      last entry in the used memory list
!mp1	 gequ	 $xx	 memory manager work pointers
!mp2	 gequ	 $xx
!mp3	 gequ	 $xx
!mp4	 gequ	 $xx
;
;  Memory manager record structure
;
mmEnd	equ	0	disp to the end of a used memory area +1
mmLength equ	0	size of a free memory block
mmLast	equ	4	pointer to the previous memory area
mmNext	equ	8	pointer to the next memory area
mmFlags	equ	12	flags word
mmMem	equ	14	first byte of usable memory
mmLastFree equ 14	(free only) ptr to previous free area
mmNextFree equ 18	(free only) ptr to next free area
;
;  Sizes
;
blockSize equ	8*1024-mmMem	size of a memory block
maxSize	equ	blockSize/2	max memory request for Malloc
minSize	equ	8	min memory request
splitSize equ	22	min extra memory to split a block
;
;  Flag bits
;
isFree	equ	$0001	memory area is free
firstBlock equ $0002	first block in a memory manager block
lastBlock equ	$0004	last block in a memory manager block
	end

	datachk off
****************************************************************
*
*  Free - free memory allocated by Malloc
*
*  Inputs:
*	X-A - address of a byte in the parameter block
*
*  Notes:
*	No action is taken if a nil pointer is passed.
*
****************************************************************
*
Free	start seg2
	using Common
	using MMCommon

	tay		set up our DP
	lda	sh_my_dp
	phd
	tcd
	tya
	sta	mp1	save the pointer
	stx	mp1+2
	ora	mp1+2	quit now if it is nil
	bne	lb1
rts	pld
!	jsl	Integrety
	rtl

lb1	anop
;
;  Find the proper memory area
;
	move4 lastmem,mp2	mp2 = lastmem
ma1	lda	mp2	branch if the memory does not exist
	ora	mp2+2
	beq	rts
	lda	mp1+2	if mp1 >= mp2 then
	cmp	mp2+2
	bne	ma2
	lda	mp1
	cmp	mp2
ma2	blt	ma4
	ldy	#2	  if mp1 < mp2^.mmEnd then
	lda	mp1+2
	cmp	[mp2],Y
	bne	ma3
	lda	mp1
	cmp	[mp2]
ma3	blt	ma5	    exit the loop
ma4	ldy	#mmLast+2	mp2 = mp2^.mmLast
	lda	[mp2],Y
	tax
	dey
	dey
	lda	[mp2],Y
	sta	mp2
	stx	mp2+2
	bra	ma1	loop

ma5	anop
;
;  Place the area in the free memory pool
;
	ldy	#2	change the end pointer to a length
	sec
	lda	[mp2]
	sbc	mp2
	sta	mp1
	lda	[mp2],Y
	sbc	mp2+2
	sta	mp1+2
	sec
	lda	mp1
	sbc	#mmMem
	sta	[mp2]
	lda	mp1+2
	sbc	#^mmMem
	sta	[mp2],Y
	ldy	#mmFlags	set the free flag
	lda	#isFree
	ora	[mp2],Y
	sta	[mp2],Y
!			{make sure lastmem is set correctly}
	lda	lastmem	if lastmem = mp2 then
	cmp	mp2
	bne	lm3
         lda	lastmem+2
	cmp	mp2+2
	bne	lm3
	ldy	#mmLast	  mp3 = mp2^.mmLast
	lda	[mp2],Y
	sta	mp3
	iny
	iny
	lda	[mp2],Y
	sta	mp3+2
lm1	lda	mp3                        done if no previous memory exists
	ora	mp3+2
	beq	lm2
	ldy	#mmFlags                   done if the area is reserved
	lda	[mp3],Y
	and	#isFree
	beq	lm2
	ldy	#mmLast                    mp3 := mp3^.mmLast
	lda	[mp3],Y
	tax
	iny
	iny
	lda	[mp3],Y
	sta	mp3+2
	stx	mp3
	bra	lm1                        try again
lm2	move4	mp3,lastmem                set lastmem
lm3	anop

!			{find any previous free memory block}
	ldy	#mmLast	mp1 = mp2^.mmLast
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
fp1	lda	mp1	quit if there is no previous block
	ora	mp1+2
	beq	fp4
	ldy	#mmFlags	found one if the free flag is set
	lda	[mp1],Y
	and	#isFree
	bne	fp2
	ldy	#mmLast+2	move to the previous entry
	lda	[mp1],Y
	tax
	dey
	dey
	lda	[mp1],Y
	sta	mp1
	stx	mp1+2
	bra	fp1	loop
!			{previous block found: insert}
fp2	ldy	#mmLastFree	mp2^.mmLastFree = mp1
	lda	mp1
	sta	[mp2],Y
	iny
	iny
	lda	mp1+2
	sta	[mp2],Y
	ldy	#mmNextFree	mp2^.mmNextFree = mp1^.mmNextFree
	lda	[mp1],Y	mp1^.mmNextFree = mp2
	sta	[mp2],Y	mp3 = mp2^.mmNextFree
	sta	mp3
	lda	mp2
	sta	[mp1],Y
	iny
	iny
	lda	[mp1],Y
	sta	[mp2],Y
	sta	mp3+2
	lda	mp2+2
	sta	[mp1],Y
	lda	mp3	if mp3 <> nil then
	ora	mp3+2
	beq	fp3
	ldy	#mmLastFree	  mp3^.mmLastFree = mp2
	lda	mp2
	sta	[mp3],Y
	iny
	iny
	lda	mp2+2
	sta	[mp3],Y
fp3	anop		endif
	bra	fp6	done with insert: exit
!			{no last block: insert at head of list}
fp4	lda	freeList	if freeList <> nil then
	ora	freeList+2
	beq	fp4a
	ldy	#mmNextFree	  mp2^.mmNextFree := freeList
	lda	freeList
	sta	[mp2],Y
	iny
	iny
	lda	freeList+2
	sta	[mp2],Y
	ldy	#mmLastFree	  freeList^.mmLastFree = mp2
	lda	mp2
	sta	[freeList],Y
	iny
	iny
	lda	mp2+2
	sta	[freeList],Y
	bra	fp5	else
fp4a	ldy	#mmNextFree	  mp2^.mmNextFree := nil
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y
fp5	anop		endif
	ldy	#mmLastFree	mp2^.lastFree = nil
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y
	move4 mp2,freeList	freeList = mp2

fp6	anop
;
;  Combine adjacent free areas
;
	ldy	#mmFlags	if not the first area in a block then
	lda	[mp2],Y
	and	#firstBlock
	jne	ca1
	ldy	#mmLastFree	  if mp2^.mmLastFree = mp2^.mmLast then
	lda	[mp2],Y
	ldy	#mmLast
	cmp	[mp2],Y
	jne	ca1
	ldy	#mmLastFree+2
	lda	[mp2],Y
	ldy	#mmLast+2
	cmp	[mp2],Y
	jne	ca1
	sta	mp1+2	    mp1 = mp2^.mmLastFree
	ldy	#mmLastFree
	lda	[mp2],Y
	sta	mp1
	ora	mp1+2	    if mp1 <> nil then
	beq	ca1
	ldy	#mmNextFree	      mp1^.mmNextFree = mp2^.mmNextFree
	lda	[mp2],Y
	sta	[mp1],Y
	iny
	iny
	lda	[mp2],Y
	sta	[mp1],Y
	clc		      mp1^.mmLength = mp2^.mmLength+mmMem
	ldy	#2
	lda	[mp2]
	adc	#mmMem
	adc	[mp1]
	sta	[mp1]
	lda	[mp2],Y
	adc	[mp1],Y
	sta	[mp1],Y
	ldy	#mmFlags	      mp1^.mmFlags = mp1^.mmFlags |
	lda	[mp2],Y		mp2^.mmFlags & lastBlock
	and	#lastBlock
	ora	[mp1],Y
	sta	[mp1],Y
	ldy	#mmNext	      mp1^.mmNext = mp2^.mmNext
	lda	[mp2],Y
	sta	[mp1],Y
	iny
	iny
	lda	[mp2],Y
	sta	[mp1],Y
	move4 mp1,mp2	      mp2 = mp1
	ldy	#mmNextFree	      mp1 = mp2^.NextFree
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ora	mp1	      if mp1 <> nil then
	beq	ca0
	ldy	#mmLastFree		mp1^.mmLastFree = mp2
	lda	mp2
	sta	[mp1],Y
	iny
	iny
	lda	mp2+2
	sta	[mp1],Y
ca0	anop		      endif
	ldy	#mmNext	      mp1 = mp2^.mmNext
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ora	mp1	      if mp1 <> nil then
	beq	ca0a
	ldy	#mmLast		mp1^.mmLast = mp2
	lda	mp2
	sta	[mp1],Y
	iny
	iny
	lda	mp2+2
	sta	[mp1],Y
ca0a	anop		      endif

ca1	ldy	#mmFlags	if not the last area in a block then
	lda	[mp2],Y
	and	#lastBlock
	jne	ca2
	ldy	#mmNextFree	  if mp2^.mmNextFree = mp2^.mmNext then
	lda	[mp2],Y
	ldy	#mmNext
	cmp	[mp2],Y
	jne	ca2
	ldy	#mmNextFree+2
	lda	[mp2],Y
	ldy	#mmNext+2
	cmp	[mp2],Y
	jne	ca2
	sta	mp1+2	    mp1 = mp2^.mmNextFree
	ldy	#mmNextFree
	lda	[mp2],Y
	sta	mp1
	ora	mp1+2	    if mp1 <> nil then
	jeq	ca2
	lda	[mp1],Y	      mp2^.mmNextFree = mp1^.mmNextFree
	sta	[mp2],Y
	iny
	iny
	lda	[mp1],Y
	sta	[mp2],Y
	ldy	#mmNext	      mp2^.mmNext = mp1^.mmNext
	lda	[mp1],Y
	sta	[mp2],Y
	iny
	iny
	lda	[mp1],Y
	sta	[mp2],Y
	clc		      mp2^.mmLength = mp1^.mmLength+mmMem
	ldy	#2
	lda	[mp1]
	adc	#mmMem
	adc	[mp2]
	sta	[mp2]
	lda	[mp1],Y
	adc	[mp2],Y
	sta	[mp2],Y
	ldy	#mmFlags	      mp2^.mmFlags = mp2^.mmFlags |
	lda	[mp1],Y		mp1^.mmFlags & lastBlock
	and	#lastBlock
	ora	[mp2],Y
	sta	[mp2],Y
	ldy	#mmNextFree	      mp1 = mp2^.mmNextFree
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ora	mp1	      if mp1 <> nil then begin
	beq	ca1a
	ldy	#mmLastFree		mp1^.mmLastFree = mp2
	lda	mp2
	sta	[mp1],Y
	iny
	iny
	lda	mp2+2
	sta	[mp1],Y
ca1a	anop		      endif
	ldy	#mmNext	      mp1 = mp2^.mmNext
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ora	mp1	      if mp1 <> nil then begin
	beq	ca1b
	ldy	#mmLast		mp1^.mmLast = mp2
	lda	mp2
	sta	[mp1],Y
	iny
	iny
	lda	mp2+2
	sta	[mp1],Y
ca1b	anop		      endif

ca2	anop
;
;  If this area encompases an entire block, release the block
;
	ldy	#mmFlags	if both last and first block flags are
	lda	[mp2],Y	  not set then return
	cmp	#firstBlock+lastBlock+isFree
	beq	eb0
	pld
!	jsl	Integrety
	rtl

!			{break the back memory link}
eb0	ldy	#mmLast	mp1 = mp2^.mmLast
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ldy	#mmNext	mp3 = mp2^.mmNext
	lda	[mp2],Y
	sta	mp3
	iny
	iny
	lda	[mp2],Y
	sta	mp3+2
	lda	mp1	if mp1 = nil then
	ora	mp1+2
	bne	eb1
	move4 mp3,memory	  memory = mp3
	bra	eb2	else
eb1	ldy	#mmNext	  mp1^.mmNext = mp3
	lda	mp3
	sta	[mp1],Y
	iny
	iny
	lda	mp3+2
	sta	[mp1],Y
eb2	anop		endif
!			{break the forward memory link}
	lda	mp3	if mp3 <> nil then
	ora	mp3+2
	beq	eb3
	ldy	#mmLast	  mp3^.mmLast = mp1
	lda	mp1
	sta	[mp3],Y
	iny
	iny
	lda	mp1+2
	sta	[mp3],Y
eb3	anop		endif

!			{break the back free link}
	ldy	#mmLastFree	mp1 = mp2^.mmLastFree
	lda	[mp2],Y
	sta	mp1
	iny
	iny
	lda	[mp2],Y
	sta	mp1+2
	ldy	#mmNextFree	mp3 = mp2^.mmNextFree
	lda	[mp2],Y
	sta	mp3
	iny
	iny
	lda	[mp2],Y
	sta	mp3+2
	lda	mp1	if mp1 = nil then
	ora	mp1+2
	bne	eb4
	move4 mp3,freeList	  freeList = mp3
	bra	eb5	else
eb4	ldy	#mmNextFree	  mp1^.mmNextFree = mp3
	lda	mp3
	sta	[mp1],Y
	iny
	iny
	lda	mp3+2
	sta	[mp1],Y
eb5	anop		endif
!			{break the forward free link}
	lda	mp3	if mp3 <> nil then
	ora	mp3+2
	beq	eb7
	ldy	#mmLastFree	  mp3^.mmLastFree = mp1
	lda	mp1
	sta	[mp3],Y
	iny
	iny
	lda	mp1+2
	sta	[mp3],Y
eb7	anop		endif

	pha		free the block of memory
	pha
	ph4	mp2
	_FindHandle
	_DisposeHandle
	pld
!	jsl	Integrety
	rtl   
	end

****************************************************************
*
*  InitMM - initialize the memory manager
*
****************************************************************
*
InitMM	start

	stz	memory
	stz	memory+2
	stz	freeList
	stz	freeList+2
	stz	lastmem
	stz	lastmem+2
	rts
	end

****************************************************************
*
*  Integrety - check it
*
****************************************************************
*
Integrety private seg2
	ago	.end
	using Common
	using MMCommon
w1	equ	mp4+4
w2	equ	w1+4
w3	equ	w2+4

	php		save regs
	phx
	phy
	pha
	phd		set up our DP
	lda	sh_my_dp
	tcd
;
;  Check the memory links
;
	move4 memory,w2	w2 = memory
	stz	w1	w1 = nil
	stz	w1+2
	stz	w3	w3 = nil
	stz	w3+2
cm1	lda	w2	while w2 <> nil do
	ora	w2+2
	beq	cm2
	ldy	#mmLast	  if w2^.mmLast <> w1 then
	lda	[w2],Y	    goto err
	cmp	w1
	bne	err
	iny
	iny
	lda	[w2],Y
	cmp	w1+2
	bne	err
	move4 w2,w1	  w1 = w2
	ldy	#mmNext	  w2 = w1^.mmNext
	lda	[w1],Y
	sta	w2
	iny
	iny
	lda	[w1],Y
	sta	w2+2
	ldy	#mmFlags                   if not (w1^.flags & isFree) then
	lda	[w1],Y
	and	#isFree
	bne	cm1
	move4	w1,w3                        w3 := w1
	bra	cm1	endwhile
cm2	anop

	lda	w3	if w3 <> lastmem then goto err
	cmp	lastmem
	bne	err
	lda	w3+2
	cmp	lastmem+2
	bne	err
;
;  Check the free memory links
;
	move4 freeList,w2	w2 = freeList
	stz	w1	w1 = nil
	stz	w1+2
fm1	lda	w2	while w2 <> nil do
	ora	w2+2
	beq	fm2
	ldy	#mmLastFree	  if w2^.mmLastFree <> w1 then
	lda	[w2],Y	    goto err
	cmp	w1
	bne	err
	iny
	iny
	lda	[w2],Y
	cmp	w1+2
	bne	err
	move4 w2,w1	  w1 = w2
	ldy	#mmNextFree	  w2 = w1^.mmNextFree
	lda	[w1],Y
	sta	w2
	iny
	iny
	lda	[w1],Y
	sta	w2+2
	bra	fm1	endwhile
fm2	anop
;
;  Return
;
	pld
	pla
	ply
	plx
	plp
	rtl

err	pld
	pla
	ply
	plx
	plp
	brk	$A0
.end
	end

****************************************************************
*
*  Malloc - allocate memory
*
*  Inputs:
*	A - # of bytes to allocate
*
*  Outputs:
*	X-A - pointer to allocated memory; null if allocation failed
*
****************************************************************
*
Malloc	start seg2
	using Common
	using MMCommon
;
;  Scan the free memory pool for an adequate area
;
;	jsl	Report	print a memory report
	tay		set up our DP
	lda	sh_my_dp
	phd
	tcd
	tya
	cmp	#minSize	make sure the min amount is requested
	bge	sf1
	lda	#minSize
	bra	sf2
sf1	cmp	#maxSize	use MLalloc if the request is too big
	blt	sf2
	ldx	#0
	pld
	brl	MLalloc
sf2	sta	mp1	save the size of the area to allocate
	stz	mp1+2

	move4 freeList,mp2	mp2 = freeList
sf3	lda	mp2	while mp2 <> nil do
	ora	mp2+2
	beq	nm1
	lda	[mp2]	  if the area is big enough then
	cmp	mp1
	bge	ar1	    allocate it
	ldy	#mmNextFree+2	  mp2 = mp2^.mmNextFree
	lda	[mp2],Y
	tax
	dey
	dey
	lda	[mp2],Y
	sta	mp2
	stx	mp2+2
	bra	sf3	endwhile
;
;  Allocate a new memory block
;
nm1	lla	mp4,blockSize	allocate the block
	jsl	NewBlock
	bcc	ar1
	lda	#0	out of memory
	tax
	pld
!	jsl	Integrety
	rtl   
;
;  Alocate an area from the block
;
ar1	sec		if the area is big enough to split then
	lda	[mp2]
	sbc	mp1
	cmp	#splitSize
	jlt	ar2
	add4	mp2,#mmMem,mp3	  {create a new area}
	add2	mp3,mp1	  mp3 = <start of new area>
	sub2	mp3,mp2,mp4	  mp4 = <size of old area>
	sec		  mp3^.mmLength = mp2^.mmLength-mp4
	lda	[mp2]
	sbc	mp4
	sta	[mp3]
	ldy	#2
	lda	#0
	sta	[mp3],Y
	lda	mp1	  mp2^.mmLength = mp1
	sta	[mp2]
	ldy	#mmLast	  mp3^.mmLast = mp2
	lda	mp2
	sta	[mp3],Y
	iny
	iny
	lda	mp2+2
	sta	[mp3],Y
	ldy	#mmNext	  mp3^.mmNext = mp2^.mmNext
	lda	[mp2],Y	  mp2^.mmNext = mp3
	sta	[mp3],Y
	lda	mp3
	sta	[mp2],Y
	iny
	iny
	lda	[mp2],Y
	sta	[mp3],Y
	lda	mp3+2
	sta	[mp2],Y
	lda	#$FFFF	  mp3^.mmFlags = mp2^.mmFlags & ~firstBlock
	eor	#firstBlock
	ldy	#mmFlags
	and	[mp2],Y
	sta	[mp3],Y
	lda	#$FFFF	  mp2^.mmFlags = mp2^.mmFlags & ~lastBlock
	eor	#lastBlock
	and	[mp2],Y
	sta	[mp2],Y
	ldy	#mmLastFree	  mp3^.mmLastFree = mp2
	lda	mp2
	sta	[mp3],Y
	iny
	iny
	lda	mp2+2
	sta	[mp3],Y
	ldy	#mmNextFree	  mp3^.mmNextFree = mp2^.mmNextFree
	lda	[mp2],Y	  mp2^.mmNextFree = mp3
	sta	[mp3],Y
	lda	mp3
	sta	[mp2],Y
	iny
	iny
	lda	[mp2],Y
	sta	[mp3],Y
	lda	mp3+2
	sta	[mp2],Y
	ldy	#mmNext	  mp4 = mp3^.mmNext
	lda	[mp3],Y
	sta	mp4
	iny
	iny
	lda	[mp3],Y
	sta	mp4+2
	ora	mp4	  if mp4 <> nil then
	beq	ar1a
	ldy	#mmLast	    mp4^.mmLast = mp3
	lda	mp3
	sta	[mp4],Y
	iny
	iny
	lda	mp3+2
	sta	[mp4],Y
	bra	ar1b
ar1a     anop                             else
	move4	mp3,lastmem                  lastmem = mp3
ar1b	anop		  endif
	ldy	#mmNextFree	  mp4 = mp3^.mmNextFree
	lda	[mp3],Y
	sta	mp4
	iny
	iny
	lda	[mp3],Y
	sta	mp4+2
	ora	mp4	  if mp4 <> nil then
	beq	ar1c
	ldy	#mmLastFree	    mp4^.mmLastFree = mp3
	lda	mp3
	sta	[mp4],Y
	iny
	iny
	lda	mp3+2
	sta	[mp4],Y
ar1c	anop		  endif
ar2	anop		endif

	jsl	Reserve
	pld
!	jsl	Integrety
	rtl   
	end

****************************************************************
*
*  MLalloc - allocate long memory
*
*  Inputs:
*	X-A - # of bytes to allocate
*
*  Outputs:
*	X-A - pointer to allocated memory; null if allocation failed
*
****************************************************************
*
MLalloc	start seg2
	using Common
	using MMCommon

	tay		set up our DP
	lda	sh_my_dp
	phd
	tcd
	tya
	sta	mp4	allocate the block
	sta	mp1
	stx	mp4+2
	stx	mp1+2
	jsl	NewBlock
	bcc	lb1
	lda	#0	out of memory
	tax
	pld
!	jsl	Integrety
	rtl   

lb1	jsl	Reserve
	pld
!	jsl	Integrety
	rtl   
	end

****************************************************************
*
*  NewBlock - allocate a new block of memory
*
*  Inputs:
*	mp4 - size of the block to allocate
*
*  Outputs:
*	mp2 - ptr to the first byte of the block
*	C - set for out of memory error, else clear
*
****************************************************************
*
NewBlock private seg2
	Using MMCommon

	pha		reserve the memory
	pha
	clc
	lda	mp4
	ldx	mp4+2
	adc	#mmMem
	bcc	lb1
	inx
lb1	phx
	pha
	ph2	~User_ID
	lda	mp4+2
	beq	lb2
	ph2	#$C008
	bra	lb3
lb2	ph2	#$C018
lb3	ph4	#0
	_NewHandle
	pl4	mp3	pull the handle
	bcc	lb3a
!	jsl	Integrety
	rtl   

lb3a	ldy	#2	dereference the handle
	lda	[mp3]
	sta	mp2
	lda	[mp3],Y
	sta	mp2+2
	ldy	#2	set the length of the free memory
	lda	mp4
	sta	[mp2]
	lda	mp4+2
	sta	[mp2],Y
!			{set the memory area links}
	lda	memory	if memory = nil then
	ora	memory+2
	bne	lb4
	move4 mp2,memory	  memory = mp2
	ldy	#mmLast	  mp2^.mmLast = nil
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y
	bra	lb7	else
lb4	move4 memory,mp3	  mp3 = memory
lb5	ldy	#mmNext	  while mp3^.mmNext <> nil do
	lda	[mp3],Y
	tax
	iny
	iny
	ora	[mp3],Y
	beq	lb6
	lda	[mp3],Y	    mp3 = mp3^.mmNext
	sta	mp3+2
	stx	mp3
	bra	lb5	  endwhile
lb6	ldy	#mmNext	  mp3^.mmNext = mp2
	lda	mp2
	sta	[mp3],Y
	iny
	iny
	lda	mp2+2
	sta	[mp3],Y
	ldy	#mmLast	  mp2^.mmLast = mp3
	lda	mp3
	sta	[mp2],Y
	iny
	iny
	lda	mp3+2
	sta	[mp2],Y
lb7	anop		endif
	ldy	#mmNext	mp2^.mmNext = 0
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y

	ldy	#mmFlags	set the flags word
	lda	#isFree+firstBlock+lastBlock
	sta	[mp2],Y
!			{set the free area links}
	lda	freeList	if freeList = nil then
	ora	freeList+2
	bne	lb8
	move4 mp2,freeList	  freeList = mp2
	ldy	#mmLastFree	  mp2^.mmLastFree = nil
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y
	bra	lb11	else
lb8	move4 freeList,mp3	  mp3 = freeList
lb9	ldy	#mmNextFree	  while mp3^.mmNextFree <> nil do
	lda	[mp3],Y
	tax
	iny
	iny
	ora	[mp3],Y
	beq	lb10
	lda	[mp3],Y	    mp3 = mp3^.mmNextFree
	sta	mp3+2
	stx	mp3
	bra	lb9	  endwhile
lb10	ldy	#mmNextFree	  mp3^.mmNextFree = mp2
	lda	mp2
	sta	[mp3],Y
	iny
	iny
	lda	mp2+2
	sta	[mp3],Y
	ldy	#mmLastFree	  mp2^.mmLastFree = mp3
	lda	mp3
	sta	[mp2],Y
	iny
	iny
	lda	mp3+2
	sta	[mp2],Y
lb11	anop		endif
	ldy	#mmNextFree	mp2^.mmNextFree = 0
	lda	#0
	sta	[mp2],Y
	iny
	iny
	sta	[mp2],Y
	clc
!	jsl	Integrety
	rtl   
	end

****************************************************************
*
*  Report - Print a memory report
*
*  Inputs:
*        memory - ptr to first memory area
*
****************************************************************
*
Report	start
	using	MMCommon
	using	Common
	ago	.report
;
;  See if a report is needed
;
	pha
	lda	>$C025
	and	#$0040
	bne	lb1
	pla
	rtl
;
;  Print the report
;
lb1	phx		save registers (A saved already)
	phy
	lda	sh_my_dp
	phd
	tcd
	stz	count	no entries printed, yet
	jsr	Wait	wait for a keypress
	move4	memory,mp1	mp1 = memory
lb2	lda	mp1	while mp1 <> nil do
	ora	mp1+2
	jeq	lb7
	inc	count	  Print(++count)
	put2	count,#3
	putc	#' '
	move4	mp1,mp2                    print mp1
	jsr	Print
	ldy	#mmFlags                   if free then
	lda	[mp1],Y
	lsr	A
	bcc	lb3
	ldy	#2                           print mp1^.mmLength
	lda	[mp1]
	sta	mp2
	lda	[mp1],Y
	sta	mp2+2
	bra	lb4                        else
lb3	ldy	#2                           print mp1^.mmEnd-mp1
	sec
	lda	[mp1]
	sbc	mp1
	sta	mp2
	lda	[mp1],Y
	sbc	mp1+2
	sta	mp2+2
lb4	jsr	Print                      endif
	ldy	#mmFlags                   print free/used
	lda	[mp1],Y
	lsr	A
	bcc	lb5
	puts	#'free',cr=t
	dec	count
	bra	lb6
lb5	puts	#'used '
	ldy	#mmFlags+2
	lda	[mp1],Y
	sta	mp2
	jsr	Print2
 	ldy	#mmFlags+4
 	lda	[mp1],Y
 	sta	mp2
 	jsr	Print2
	putcr
lb6	jsr	Pause
	ldy	#mmNext                    mp1 = mp1^.mmNext
	lda	[mp1],Y
	tax
	iny
	iny
	lda	[mp1],Y
	sta	mp1+2
	stx	mp1
	brl	lb2	endwhile
lb7	pld		restore regs
	ply
	plx
	pla
	rtl

count	ds	2
;
;  Print - print a hex number
;
Print	lda	mp2+3
	jsr	pbyte
	lda	mp2+2
	jsr	pbyte
Print2	lda	mp2+1
	jsr	pbyte
	lda	mp2
	jsr	pbyte
	putc	#' '
	rts

pbyte	pha
	lsr	A
	lsr	A
	lsr	A
	lsr	A
         jsr	pnibble
	pla
pnibble	and	#$000F
	ora	#'0'
	cmp	#'9'+1
	blt	pb1
	adc	#6
pb1	sta	ch
	putc	ch
	rts

ch	ds	2
;
;  Pause - stop on keypress
;
Pause	short	I,M
	lda	>$C000
	bpl	ps2
Wait	short	I,M
	sta	>$C010
ps1	lda	>$C000
	bpl	ps1
	sta	>$C010
ps2	long	I,M
	rts
.report
	end

****************************************************************
*
*  Reserve - reserve an area of memory
*
*  Inputs:
*	mp1 - size of the memory area
*	mp2 - locatio of the area to allocate
*
*  Outputs:
*	X-A - mp2
*
****************************************************************
*
Reserve	start seg2
	using MMCommon

	clc		change length to last byte+1 pointer
	lda	[mp2]
	adc	#mmMem
	adc	mp2
	sta	[mp2]
	ldy	#2
	lda	mp2+2
	sta	[mp2],Y
	ldy	#mmLastFree	{remove from the free memory pool}
	lda	[mp2],Y	mp3 = mp2^.mmLastFree
	sta	mp3
	iny
	iny
	lda	[mp2],Y
	sta	mp3+2
	ldy	#mmNextFree	mp4 = mp2^.mmNextFree
	lda	[mp2],Y
	sta	mp4
	iny
	iny
	lda	[mp2],Y
	sta	mp4+2
	ora	mp4	if mp4 <> nil then
	beq	lb2
	ldy	#mmLastFree	  mp4^.mmLastFree = mp3
	lda	mp3
	sta	[mp4],Y
	iny
	iny
	lda	mp3+2
	sta	[mp4],Y
lb2	lda	mp3	if mp3 <> nil then
	ora	mp3+2
	beq	lb3
	ldy	#mmNextFree	  mp3^.NextFree = mp4
	lda	mp4
	sta	[mp3],Y
	iny
	iny
	lda	mp4+2
	sta	[mp3],Y
	bra	lb4	else
lb3	move4 mp4,freeList	  freeList = mp4
lb4	anop		endif
	ldy	#mmFlags                 clear free flag
	lda	#$FFFF
	eor	#isFree
	and	[mp2],Y
	sta	[mp2],Y
!			{make sure lastmem is correct}
	ldy	#mmNext	mp3 := mp2^.mmNext
	lda	[mp2],Y
	sta	mp3
	iny
	iny
	lda	[mp2],Y
	sta	mp3+2
lb5	lda	mp3	while mp3 <> nil do
	ora	mp3+2
	beq	lb7
	ldy	#mmFlags                   if not (mp3^.mmFlags & isFree) then
	lda	[mp3],Y
	and	#isFree
	bne	lb6
	bra	lb8                          goto lb8
lb6	ldy	#mmNext+2                  mp3 := mp3^.mmNext
	lda	[mp3],Y
	tax
	dey
	dey
	lda	[mp3],Y
	sta	mp3
	stx	mp3+2
	bra	lb5	endwhile
lb7	move4	mp2,lastmem	lastmem := mp2
lb8	anop

	clc		return mp2+mmMem
	lda	mp2
	adc	#mmMem
	ldx	mp2+2
!	jsl	Integrety
	rtl   
	end
