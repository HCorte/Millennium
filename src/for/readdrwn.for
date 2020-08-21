C
C SUBROUTINE READDRWN 
C
C V01 09-NOV-99 RXK Initial version, created from subr. READDRW ro read 
C                   more than 1 draw files 
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
	SUBROUTINE READDRWN(LOGREC,FDB,EOT,NEW)
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
	INTEGER*4 EOTCNT	    !COUNT OF CONSECUTIVE EMPTY RECORDS
	INTEGER*4 BLOCK		    !BLOCK # INTO SAVDRW FILE
	INTEGER*4 LENGTH	    !LENGTH OF SINGLE SERIAL #
	INTEGER*4 TMFBUF(8192)	    !BUFFER FOR 1 DRW RECORD
	INTEGER*4 IND		    !WORD INDEX INTO TMFBUF
	INTEGER*4 RTYPE		    !RTYPE OF SINGLE SERIAL #
	LOGICAL   EOT		    !SET TO TRUE WHEN 5000 CONS EMPTY RECS
	LOGICAL   NEW		    !INPUT PARAM.,VALUE IS .TRUE. IF FIRST READ
                                    !OF NEW FILE AND  
C
	DATA BLOCK/0/
	DATA EOTCNT/0/
	DATA IND/8192/
C
C
        IF(NEW) THEN
           BLOCK=0
           EOTCNT=0
           IND=8192
           NEW=.FALSE.
        ENDIF

1000	CONTINUE
	IF(IND.GE.8157) THEN
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
	IF(EOTCNT.GT.1000) THEN
	  EOT=.TRUE.
	  GOTO 10000
	ENDIF
C
	IF(TMFBUF(IND).EQ.0) THEN
	  EOTCNT=EOTCNT+1
	  IND=IND+LREC
	  GOTO 1000
	ENDIF
C
C
	EOTCNT=0
	CALL ILBYTE(RTYPE,TMFBUF(IND),LREC1-1)
	IF(RTYPE.NE.LONE.AND.RTYPE.NE.LREG) THEN
	  TYPE*,'Bad record type > ',RTYPE,' index > ',IND
	  IND=IND+LREC
	  GOTO 1000
	ENDIF
C
C
	LENGTH=LREC
	IF(RTYPE.EQ.LONE) THEN
	  CALL ILBYTE(RTYPE,TMFBUF(IND),LREC2-1)
	  IF(RTYPE.EQ.LEND) LENGTH=LREC*2
	  IF(RTYPE.EQ.LTWO) LENGTH=LREC*3
	ENDIF
C
	CALL FASTMOV(TMFBUF(IND),LOGREC,LENGTH)
	IND=IND+LENGTH
C
C
10000	CONTINUE
C
	RETURN
	END
