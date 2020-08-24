C SUBROUTINE SPTDSP
C  
C V03 08-FEB-2000 OXK NUMROWS set as SPGNBR (Vakio changes)
C V02 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C     CALL SPTDSP(SYSNR)   ANALYZE SYSTEM BET
C
C     IN: SYSNR     - SYSTEM NUMBER
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
	SUBROUTINE SPTDSP(SYSNR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INTEGER*4 SYSNR, BET, TOTAL, LASTBET, FIRSTBET, TIMES
	INTEGER*4 PTR, NUMROWS
C
CV03	NUMROWS=SPSNUM(4,SYSNR)
	NUMROWS=SPGNBR
	WRITE(6,*) ' '
	WRITE(6,*) 'processing system: ',SYSNR,', nr of rows: ',NUMROWS
	IF (SPSATR(SYSNR).EQ.NOSYS) THEN
	   WRITE(6,*) 'not a valid system bet '
     *	 ,' ',SPSNUM(2,SYSNR)-SPSNUM(1,SYSNR),'-'
     *	 ,SPSNUM(3,SYSNR)-SPSNUM(2,SYSNR),'-'
     *	 ,SPSNUM(5,SYSNR)
	   RETURN
	ELSEIF (SPSATR(SYSNR).EQ.FULSYS) THEN
	   WRITE(6,*) 'full system bet'
     *	 ,' ',SPSNUM(2,SYSNR)-SPSNUM(1,SYSNR),'-'
     *	 ,SPSNUM(3,SYSNR)-SPSNUM(2,SYSNR),'-'
     *	 ,SPSNUM(5,SYSNR)
	   RETURN
	ELSEIF (SPSATR(SYSNR).EQ.REDSYS) THEN
	   WRITE(6,*) 'reduced system bet'
     *	 ,' ',SPSNUM(2,SYSNR)-SPSNUM(1,SYSNR),'-'
     *	 ,SPSNUM(3,SYSNR)-SPSNUM(2,SYSNR),'-'
     *	 ,SPSNUM(5,SYSNR)
	ELSEIF (SPSATR(SYSNR).EQ.USYS) THEN
	   WRITE(6,*) 'u system bet '
     *	 ,' ',SPSNUM(2,SYSNR)-SPSNUM(1,SYSNR),'-'
     *	 ,SPSNUM(3,SYSNR)-SPSNUM(2,SYSNR),'-'
     *	 ,SPSNUM(5,SYSNR)
	ELSE
	   WRITE(6,*) 'not a valid system bet '
	   RETURN
	ENDIF
C
	PTR=SPSPTR(SYSNR)
	IF (PTR.LE.0) THEN
	   WRITE (6,*) 'No further information posted '
	   RETURN
	ENDIF
	TIMES=SPSTAB(PTR)
	FIRSTBET=PTR+1
	LASTBET=FIRSTBET+TIMES*NUMROWS-1
	WRITE(6,*)'This bet corresponds to ',TIMES,' bets'
	TOTAL=0
	DO 60, BET=FIRSTBET,LASTBET,NUMROWS   !DO ANALISIS OF ALL BETS
	   CALL FULLANL(SPSTAB(BET),NUMROWS,TOTAL)
60	CONTINUE
	IF (TOTAL.NE.SPSNUM(5,SYSNR)) THEN
       WRITE(6,*)'Total number of bets (',TOTAL,') does not correspond'
     *	 ,' to ',SPSNUM(2,SYSNR)-SPSNUM(1,SYSNR),'-'
     *	 ,SPSNUM(3,SYSNR)-SPSNUM(2,SYSNR),'-'
     *	 ,SPSNUM(5,SYSNR)
	ENDIF
C
	RETURN
C
	END
