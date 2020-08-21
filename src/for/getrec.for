C
C SUBROUTINE GETREC
C $Log:   GXAFXT:[GOLS]GETREC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:22:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:27:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dllrec.for **
C
C DLLREC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETREC(NEW,FILNAM,LU,LEN,BUF,ST)
	IMPLICIT NONE
C
	INTEGER*4 IND, BLK, LU, ST, OUTIND, LEN
C
	INTEGER*4 FILNAM(2),I4LBUF(128)
	CHARACTER BUF(*),LOCALBUF(512)
	LOGICAL NEW
	EQUIVALENCE (I4LBUF,LOCALBUF)
C
C
	IF(NEW) THEN
	  NEW=.FALSE.
	  IND=999
	  BLK=0
	  CLOSE(UNIT=LU)
	  OPEN(UNIT=LU,NAME=FILNAM,STATUS='OLD',IOSTAT=ST,FORM='UNFORMATTED',
     *         ACCESS='DIRECT')
	  IF(ST.NE.0) THEN
       	    WRITE(5,900) FILNAM,ST
900	    FORMAT(1X,2A4,' open error ',I4)
	    CALL GPAUSE
	  ENDIF
	  RETURN
	ENDIF
C
C
	OUTIND=1
10	CONTINUE
	IF(IND.GT.512) THEN
	  IND=1
	  BLK=BLK+1
	  READ(LU,REC=BLK,IOSTAT=ST) I4LBUF
	  IF(ST.NE.0) THEN
	    WRITE(5,901) FILNAM,ST
901         FORMAT(1X,2A4,' read error ',I4)
	    CALL GPAUSE
	  ENDIF
	ENDIF
C
C
	BUF(OUTIND)=LOCALBUF(IND)
	OUTIND=OUTIND+1
	IND=IND+1
	IF(OUTIND.GT.LEN) RETURN
	GOTO 10
	END
