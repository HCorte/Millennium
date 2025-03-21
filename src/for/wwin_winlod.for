C
C SUBROUTINE WWIN_WINLOD
C
C V04 15-JUN-2000 UXN Unused variables removed.
C V03 14-JAN-1999 GPW STOPSYS OPTIMIZATION
C V02 17-DEC-1993 HXK COMMENTED OUT CARRYOVER FILE (NOT USED IN FINLAND)
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C SUBROUTINE TO LOAD VLF/TCF RECORDS TO DRAW FILES
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
	SUBROUTINE WWIN_WINLOD(FUN,RECORD)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
        INTEGER*4 NAMVLW(5),NAMTCW(5)                              !V01
	INTEGER*4 VLFBUF(72,113),TCFBUF(48,170),VFDB(7)
	INTEGER*4 VLFREC(8192),TCFREC(8192),RECORD(*)
	INTEGER*4 FUN, VIND, VBLOCK, TIND, TBLOCK, VLFCNT
	INTEGER*4 ST, K
	EQUIVALENCE(VLFREC(1),VLFBUF(1,1))
	EQUIVALENCE(TCFREC(1),TCFBUF(1,1))
	DATA VLFREC/8192*0/,TCFREC/8192*0/
	DATA VLFCNT/0/
C
C
	IF(FUN.EQ.1) GOTO 100      !OPEN FILES
	IF(FUN.EQ.2) GOTO 200      !WRITE VLF RECORD
C***	IF(FUN.EQ.3) GOTO 300      !WRITE TCF RECORD
	IF(FUN.EQ.4) GOTO 400      !CLOSE FILES
C
C OPEN FILES
C
100	CONTINUE

        CALL WINGETW('WWINTSK ',NAMVLW,NAMTCW)                    !V01
	CALL OPENW(VLW,NAMVLW,4,0,0,ST)
	CALL IOINIT(VFDB,VLW,128*256)
	IF(ST.NE.0) CALL FILERR(NAMVLW,1,ST,0)
C
C
C***	CALL OPENW(TCW,NAMTCW,4,0,0,ST)
C***	CALL IOINIT(TFDB,TCW,128*256)
C***	IF(ST.NE.0) CALL FILERR(NAMTCW,1,ST,0)
C
C
	VIND=1
	TIND=1
	VBLOCK=0
	TBLOCK=0
	RETURN
C
C WRITE VLF RECORD
C
200	CONTINUE
	VLFCNT=VLFCNT+1
	CALL FASTMOV(RECORD,VLFBUF(1,VIND),72)
	VIND=VIND+1
	IF(VIND.GT.113) THEN
	  VBLOCK=VBLOCK+1
	  CALL WRITEW(VFDB,VBLOCK,VLFREC,ST)
	  IF(ST.NE.0) CALL FILERR(NAMVLW,3,ST,VBLOCK)
	  CALL FASTSET(0,VLFREC,8192)
	  VIND=1
	ENDIF
	RETURN
C
C WRITE RECORD TO CARRYOVER FILE
C
C300	CONTINUE
C***	TCFCNT=TCFCNT+1
C***	CALL FASTMOV(RECORD,TCFBUF(1,TIND),48)
C***	TIND=TIND+1
C***	IF(TIND.GT.170) THEN
C***	  TBLOCK=TBLOCK+1
C***	  CALL WRITEW(TFDB,TBLOCK,TCFREC,ST)
C***	  IF(ST.NE.0) CALL FILERR(NAMTCW,3,ST,TBLOCK)
C***	  CALL FASTSET(0,TCFREC,8192)
C***	  TIND=1
C***	ENDIF
C***	RETURN
C
C CLOSE FILES
C
400	CONTINUE
	VBLOCK=VBLOCK+1
	CALL WRITEW(VFDB,VBLOCK,VLFREC,ST)
	IF(ST.NE.0) CALL FILERR(NAMVLW,3,ST,VBLOCK)
	CALL CLOSEFIL(VFDB)
C
C
C***	TBLOCK=TBLOCK+1
C***	CALL WRITEW(TFDB,TBLOCK,TCFREC,ST)
C***	IF(ST.NE.0) CALL FILERR(NAMTCW,3,ST,TBLOCK)
C***	CALL CLOSEFIL(TFDB)
C
C
	WRITE(5,900) IAM(),VLFCNT,(NAMVLW(K),K=1,5)
C***	WRITE(5,900) IAM(),TCFCNT,(NAMTCW(K),K=1,5)
	RETURN
C
C
900	FORMAT(1X,A,1X,I6,' records loaded to ',5A4)
	END
