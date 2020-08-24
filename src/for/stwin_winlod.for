C
C SUBROUTINE STWIN_WINLOD
C 
C V01 21-MAY-1999 UXN INITIAL RELEASE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C SUBROUTINE TO LOAD VLF/TCF RECORDS TO DRAW FILES
C
C
C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE STWIN_WINLOD(FUN,RECORD)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C---- Local variables.

	INTEGER*4 VLFBUF(72,113),TCFBUF(48,170),VFDB(7)
	INTEGER*4 VLFREC(8192),TCFREC(8192),RECORD(*)
	INTEGER*4 FUN, VIND, VBLOCK, TIND, TBLOCK, VLFCNT
	INTEGER*4 ST, K
        INTEGER*4 NAMVLW(5),NAMTCW(5)                             ! V01
C
	EQUIVALENCE(VLFREC(1),VLFBUF(1,1))
	EQUIVALENCE(TCFREC(1),TCFBUF(1,1))

	DATA VLFREC/8192*0/,TCFREC/8192*0/
	DATA VLFCNT/0/


	IF (FUN .EQ. 1) GOTO 100      !OPEN FILES
	IF (FUN .EQ. 2) GOTO 200      !WRITE VLF RECORD
C***	IF (FUN .EQ. 3) GOTO 300      !WRITE TCF RECORD
	IF (FUN .EQ. 4) GOTO 400      !CLOSE FILES

C---- Open files

100	CONTINUE

        CALL WINGETW('STWINTSK',NAMVLW,NAMTCW)                    ! V01
        CALL OPENW(VLW,NAMVLW,4,0,0,ST)
 
	CALL IOINIT(VFDB,VLW,128*256)

	IF (ST .NE. 0) CALL FILERR(NAMVLW,1,ST,0)                 !V01

	VIND = 1
	TIND = 1
	VBLOCK = 0
	TBLOCK = 0

	RETURN

C---- Write vlf record

200	CONTINUE

	VLFCNT = VLFCNT + 1

	CALL FASTMOV(RECORD,VLFBUF(1,VIND),72)

	VIND = VIND + 1

	IF (VIND .GT. 113) THEN
	  VBLOCK = VBLOCK+1
	  CALL WRITEW(VFDB,VBLOCK,VLFREC,ST)
	  IF (ST .NE. 0) CALL FILERR(NAMVLW,3,ST,VBLOCK)           !V01
	  CALL FASTSET(0,VLFREC,8192)
	  VIND = 1
	END IF

	RETURN

C WRITE RECORD TO CARRYOVER FILE

C300	CONTINUE
C***	TCFCNT=TCFCNT+1
C***	CALL FASTMOV(RECORD,TCFBUF(1,TIND),48)
C***	TIND=TIND+1
C***	IF(TIND.GT.170) THEN
C***	  TBLOCK=TBLOCK+1
C***	  CALL WRITEW(TFDB,TBLOCK,TCFREC,ST)
C***	  IF(ST.NE.0) CALL FILERR(NAMTCW,3,ST,TBLOCK)               !V01
C***	  CALL FASTSET(0,TCFREC,8192)
C***	  TIND=1
C***	ENDIF
C***	RETURN
C
C CLOSE FILES
C

400	CONTINUE

	VBLOCK = VBLOCK+1
	CALL WRITEW(VFDB,VBLOCK,VLFREC,ST)
	IF (ST .NE. 0) CALL FILERR(NAMVLW,3,ST,VBLOCK)              !V01
	CALL CLOSEFIL(VFDB)
C
C
C***	TBLOCK=TBLOCK+1
C***	CALL WRITEW(TFDB,TBLOCK,TCFREC,ST)
C***	IF(ST.NE.0) CALL FILERR(NAMTCW,3,ST,TBLOCK)
C***	CALL CLOSEFIL(TFDB)
C
C
	WRITE(5,900) IAM(),VLFCNT,(NAMVLW(K),K=1,5)                !V01
C***	WRITE(5,900) IAM(),TCFCNT,(NAMTCW(K),K=1,5)
	RETURN
C
C
900	FORMAT(1X,A,1X,I6,' records loaded to ',5A4)

	END
