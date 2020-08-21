C SUBROUTINE GOREAD
C
C V04 13-JUN-2000 OXK LANREAD from LANCOM.DEF to here
C V03 12-DEC-1994 GPR Integrate UK changes into X2X Baseline
C V02 30-MAR-1992 DAS IF NO LAN BUFFERS THEN SET A READ TIMER TRAP
C V01 15-SEP-1990 MRM RELEASED FOR VAX
C
C CALL GOREAD(LAN)
C IN - LAN - ETHERNET ON WHICH READ PROCEED IS TO BE STARTED
C  SET UP OUTSTANDING READ.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GOREAD(LAN,SAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:NMADEF.DEF'
        INCLUDE '($SSDEF)'
        INCLUDE '($IODEF)'
	INCLUDE '($SYSSRVNAM)'
C
        EXTERNAL LTIMTRAP           !AST TO HANDLE TIMER TRAPS
	EXTERNAL LBUFTRAP	    !AST TO HANDLE READ  TRAPS
C
	INTEGER*4 LANREAD                     !LAN BUFS EXHAUSTED,TRY AGAIN
C
        INTEGER*4 INIT/-1/
        INTEGER*4 STATUS
        INTEGER*4 READTIME
        INTEGER*4 DELAY(2)
        CHARACTER CURTIM*30
C
	INTEGER*4 LUN, SAP, LAN, LANS, LUNS, BUF
	INTEGER*4 ST, I, SLOT, IOSTAT, TEMP, MAX_READ			!V03
C
D	WRITE(6,*)'**** GOREAD ****[',LAN,SAP,LUN,']'
C
C CHECK VALIDITY
C
	LUN=LUNLAN(SAP,LAN)
	IF(LAN.LE.0.OR.LAN.GT.MAXLAN.OR.
     *	   LUN.LE.0) THEN
D	      TYPE*,'**** GOREAD: INVALID STATE TO READ ****[',LAN,LUN,']'
D	      TYPE *,'SAP:   ',SAP,'  LAN:  ',LAN
	      CALL OPS('**** GOREAD IO:... LAN STATE ****',LUN,LAN)
	      RETURN
	ENDIF
C
C CHECK TO ENSURE THE ETHERNET DEVICE HAS BEEN STARTED.
C
	IF(LANOPN(SAP,LAN).NE.LSOPN.OR.
     *	   LOCLAN(SAP,LAN).NE.LANUP) THEN
	      CALL OPS('**** GOREAD:... STATUS ****',LUN,LAN)
	      LANS=LOCLAN(SAP,LAN)
	      LUNS=LANOPN(SAP,LAN)
	      CALL OPS('**** GOREAD:... STATUS ****',LUNS,LANS)
	      RETURN
	 ENDIF
C
C START UP TO MAXREAD OUTSTANDING READS.
C
   	TEMP=READCNT(LAN,SAP)
	BUF=0
        MAX_READ=LAN_MAXREAD						  !V03
        IF (MAX_READ.GT.MAXREAD-1) MAX_READ=MAXREAD-1			  !V03
        DO 100 I=TEMP,MAX0(1,MAX_READ)					  !V03
	  CALL LANGETB(BUF,ST)
	  IF(ST.NE.2) THEN
	    READ_SLOT(SAP,LAN)=MOD(READ_SLOT(SAP,LAN),MAXREAD)+1
	    SLOT=READ_SLOT(SAP,LAN)
	    TRAP_DATA(TRPDTA_SLOT,SLOT,SAP,LAN)=READ_SLOT(SAP,LAN)
	    TRAP_DATA(TRPDTA_BUFFER,SLOT,SAP,LAN)=BUF
	    TRAP_INFO(TRPINF_SAP,BUF)=SAP
	    TRAP_INFO(TRPINF_LAN,BUF)=LAN
	    IOSTAT=SYS$QIO(,%VAL(LUN),			        !CHANNEL NUMBER
     *                      %VAL(IO$_READVBLK),	                !FUNCTION CODE
     *                      %REF(READ_IOSB(SLOT,SAP,LAN)),      !STATUS BLOCK
     *                      %REF(LBUFTRAP),	                !AST ROUTINE
     *			    TRAP_DATA_EQV(SLOT,SAP,LAN),        !AST PARAMETER
     *                      BLANBUF(FRTYPBEG,BUF),   	        !BUFFER ADDRESS
     *                      %VAL(ETHLENDT),,,	                !BUFFER LENGTH
     *                      READ_DATA(SLOT,SAP,LAN),,)
	    IF(.NOT.IOSTAT) THEN
	      CALL OPS('ERROR STARTING READ',IOSTAT,SAP)
	      GOTO 8000
	    ENDIF
	    READCNT(LAN,SAP)=READCNT(LAN,SAP)+1
	    READTIM(LAN,SAP)=-1
C
          ELSE
C
            IF(INIT.EQ.-1) THEN
C
C CALCULATE TIME OFFSET FOR THIS TIMER TRAP.
C
              IF(LANREAD.LT.1000) LANREAD = 1000      ! 1 SECOND
C
              WRITE(CURTIM,9000) LANREAD/1000,MOD(LANREAD,1000)
              STATUS=SYS$BINTIM(CURTIM,DELAY)
              IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
              INIT=0
            ENDIF
C
C ENABLE TIMER TRAP
C
            CALL GETTIM(READTIME)                 ! VALUE IS IN SECONDS
            READTIM(LAN,SAP)=READTIME+(LANREAD/1000)
            STATUS=SYS$SETIMR(,DELAY,LTIMTRAP,PREADTIMR,0)    ! 3 = PREADTIMR
            IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	  ENDIF
100	CONTINUE
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
C
C FORMAT STATEMENT
C
9000    FORMAT('0000 00:00:',I2.2,'.',I3.3)
	END
