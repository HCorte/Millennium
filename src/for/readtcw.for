C
C SUBROUTINE READTCW
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE READTCW(LOGREC,FDB,EOT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
C
	INTEGER*4 LOGREC(*)	    !LOG FORMAT OF ONE SINGLE SERIAL #
	INTEGER*4 FDB(7)	    !FILE DESC BLOCK OF SAVDRW FILE
	INTEGER*4 ST		    !STATUS
	INTEGER*4 BLOCK		    !BLOCK # INTO SAVDRW FILE
	INTEGER*4 LENGTH	    !LENGTH OF SINGLE SERIAL #
	INTEGER*4 TMFBUF(8192)	    !BUFFER FOR 1 DRW RECORD
	INTEGER*4 IND		    !WORD INDEX INTO TMFBUF
	LOGICAL   EOT		    !SET TO TRUE WHEN 5000 CONS EMPTY RECS
C
	DATA BLOCK/0/
	DATA IND/8192/
C
C
1000	CONTINUE
	IF(IND.GE.8160) THEN
	  BLOCK=BLOCK+1
	  IND=1
	  CALL READW(FDB,BLOCK,TMFBUF,ST)
	  IF(ST.EQ.144) THEN
	     EOT = .TRUE.
	     GOTO 10000
	  ENDIF
	  IF(ST.NE.0) THEN
	    TYPE *,'FILE READ ERROR =',ST,'  BLOCK=',BLOCK
	    CALL GPAUSE
	    EOT=.TRUE.
	    GOTO 10000
	  ENDIF
	ENDIF
C
	IF(TMFBUF(IND).EQ.0) THEN
	  EOT = .TRUE.
	  GOTO 10000
	ENDIF
C
	LENGTH = LMUREC
	CALL FASTMOV(TMFBUF(IND),LOGREC,LENGTH)
	IND=IND+LENGTH
C
C
10000	CONTINUE
C
	RETURN
	END
