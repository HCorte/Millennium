
C  GXSRC:CRSTIM.FOR
C  
C  $Log:   GXAFIP:[GOLS]CRSTIM.FOV  $
C  
C     Rev 1.2   28 Mar 1997 12:34:34   HXK
C  Rita's changes for releasing buffer when IPS system times out
C  
C     Rev 1.1   28 Jan 1997 19:33:34   HXK
C  Changes for LOTGEN (IPS=
C  
C     Rev 1.0   17 Apr 1996 12:44:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   01 Sep 1994 17:11:34   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY FOR THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:04:54   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:34:24   SYSTEM
C  Initial revision.
C
C
C
C V06 27-JAN-14 SCML Fix: validation transaction offset changed from 15 to 17
C                    (validation mode type 2 byte-field was added regarding new bank validation mode).
C V05 22-JUL-03 FRP Apply Urmas fix: if TRANSTYP=13, then OFFSET=15 (not 17)
C V04 15-MAY-93	DSL Added financial passthru per (FPT0001)
C V03 27-APR-93 TJR ADDED AGENT UPDATE PER DESIGN DOC CDH0001
C V02 14-OCT-92 GAP ADDED (CHECKWRITER)
C V01 18-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C This program will check for timed out transactions
C which have been sent over the TCP lines.
C
C Calling sequence:
C
C     CALL CRSTIM
C
C Input parameters:
C
C     None
C
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
C Copyright 1994,1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CRSTIM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4   XRFNUM          !Cross reference number
	INTEGER*4   PBUF            !Procom buffer number
	INTEGER*4   XLIN            !Transmit line
	INTEGER*4   TRTYPE          !Procom type
	INTEGER*4   ST              !Status
	INTEGER*4   MESS(EDLEN)	    !ERRLOG Message Buffer
	INTEGER*4   TRANSTYP        !Transaction type
	INTEGER*4   OFFSET          !Offset for result code 
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	LOGICAL*1   TIMEOUT         !Timeout Flag
C
	TIMEOUT=.FALSE.
C
C
C CHECK FOR ANY TIMED OUT TRANSACTIONS.
C
1000	CONTINUE
	CALL CHKTIMER(XRFNUM,PBUF)
	IF(PBUF.EQ.0) GOTO 9000
C
C A TIMED OUT BUFFER HAS BEEN ENCOUNTERED.
C SET STATUS TO TIMED OUT, AND SET THE TRCODE ALLOWING
C DISPAT TO QUEUE TO THE APPROPRIATE PROCESSING TASK.
C
	TIMEOUT=.TRUE.
C
	I4TEMP=0
	I1TEMP(1)=BPRO(BINSTAB+5,PBUF)
	I1TEMP(2)=BPRO(BINSTAB+6,PBUF)
	I1TEMP(3)=BPRO(BINSTAB+7,PBUF)
	I1TEMP(4)=BPRO(BINSTAB+8,PBUF)
	XRFNUM=I4TEMP			!Cross Reference #
C
        I4TEMP=0
        I1TEMP(1)=BPRO(BINSTAB+9,PBUF)
        I1TEMP(2)=BPRO(BINSTAB+10,PBUF)
        TRANSTYP=I4TEMP                 !Transaction type

        IF(TRANSTYP.EQ.15.OR.TRANSTYP.EQ.7.OR.TRANSTYP.EQ.14) THEN
           OFFSET=11           !act.gam.tab or games avail. or pack issue resp. 
        ELSEIF(TRANSTYP.EQ.13) THEN
C           OFFSET=15           !validation (Urmas fix)
           OFFSET=17           !V06
        ELSEIF(TRANSTYP.EQ.8) THEN
           OFFSET=15           !order
        ELSE
           OFFSET=13           !other 
        ENDIF

	I4TEMP=INTIM			    !STATUS 101 = TIMED OUT
        BPRO(BINSTAB+OFFSET+0,PBUF) = I1TEMP(1)  
        BPRO(BINSTAB+OFFSET+1,PBUF) = I1TEMP(2)  
C
	IF((HPRO(TRCODE,PBUF).NE.TYPCWT) .AND. 	!CHECKWRITER		CDH0001
	1  (HPRO(TRCODE,PBUF).NE.TYPSSI) .AND.	!AGENT UPDATE		CDH0001
	1  (HPRO(TRCODE,PBUF).NE.TYPFPT)) THEN  !FINANCIAL PASSTHRU     CDH0001
	   HPRO(TRCODE,PBUF)=TYPCRS	     !Send all others to INSOUT CDH0001
	ENDIF
C
C QUEUE BACK TO DISPAT.
C
        IF(HPRO(TRCODE,PBUF).NE.TYPFPT) THEN
	   CALL ABL(PBUF,QUETAB(1,DIS),ST)
        ELSE
           CALL RELBUF(PBUF)
        ENDIF
	GOTO 1000
C
C ALL BUFFERS HAVE BEEN CHECKED. REMOVE ANY TIMED OUT BUFFERS
C FROM SNDQUE
C
9000	CONTINUE
	IF(TIMEOUT) CALL CRSVER        !CHECK ONLY IF SOMETHING TIMED OUT
	RETURN
	END
