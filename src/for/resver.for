C
C PROGRAM RESVER
C
C V10 06-FEB-2001 ANG Added passive game
C V09 01-DEC-2000 UXN TOTOGOLO ADDED.
C V08 13-OCT-1999 RXK World Tour added.
C V07 14-MAY-1999 UXN Super Triple added.
C V06 23-NOV-1995 PXB Added Double and Couple games
C V05 18-OCT-1994 HXK Added Bingo
C V04 23-JUL-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C LOTTERY RESULTS ENTRY PROGRAM
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM RESVER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
	INTEGER*4  K                   !
	INTEGER*4  DRAW                !
	INTEGER*4  GNUM                !
	INTEGER*4  GIND                !
	INTEGER*4  GTYP                !
	INTEGER*4  ST                  !
C
C
	CALL COPYRITE
C
C
100	CONTINUE
	IF(VERREADY.EQ.0) THEN
	    CALL XWAIT(5,2,ST)
	    GOTO 100
	ENDIF
C
  	GTYP = CUR_GTYP
	GIND = CUR_GIND
	GNUM = CUR_GNUM
	DRAW = CUR_DRAW
C
	WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
C
C WAIT FOR OPERATOR ENTRY TO FINISH
C
10	CONTINUE
	IF(OPDONE.EQ.0) THEN
	    TYPE*,IAM(),' Waiting for operator entry'
	    CALL XWAIT(5,2,ST)
	    GOTO 10
	ENDIF
C
C
	IF(GTYP.EQ.TLTO) CALL LTOVER(GIND,DRAW,ST)
	IF(GTYP.EQ.TSPT) CALL SPTVER(GNUM,GIND,DRAW,ST)
	IF(GTYP.EQ.TTGL) CALL TGLVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TNBR) CALL NBRVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TKIK) CALL KIKVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TSCR) CALL SCRVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TWIT) CALL WITVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TTSL) CALL TSLVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TBNG) CALL BNGVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TDBL) CALL DBLVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TCPL) CALL CPLVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TSSC) CALL SSCVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TTRP) CALL TRPVER(GNUM,GIND,DRAW,ST)
        IF(GTYP.EQ.TSTR) CALL STRVER(GNUM,GIND,DRAW,ST)
	IF(GTYP.EQ.TPAS) CALL PASVER(GNUM,GIND,DRAW,ST)
	IF(ST.NE.0) GOTO 10

	WRITE(5,920) IAM(),GTNAMES(GTYP),GIND
	CALL GSTOP(GEXIT_SUCCESS)

910	FORMAT(1X,A,1X,A8,I1,2X,4A4,'Draw ',I5,/)
920	FORMAT(1X,A,1X,A8,I1,' results entry complete')

	END
