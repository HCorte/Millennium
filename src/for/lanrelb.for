C
C SUBROUTINE LANRELB
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LANRELB.FOV                                  $
C  $Date::   17 Apr 1996 13:47:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lanrelb.for;1 **
C
C LANRELB.FOR
C
C V01 10-SEP-90 MRM RELEASED FOR VAX
C
C CALL LANRELB(BUF)
C
C      BUF   - BUFFER NUMBER (FTN)
C              SEND IT TO FREE EXTRA OR FRAP QUE
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LANRELB(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 BUF,STATUS,QUE,TIMES
C
	IF(BUF.LT.1.OR.BUF.GT.LANBNUM) THEN
	  TYPE*,'**** ILLEGAL REL BUF NUMBER ... ',BUF
	  CALL GPAUSE
	  TYPE*,'**** 1 ILLEGAL REL BUF NUMBER ... ',BUF
	  RETURN
	ENDIF
C
	IF(LANTEST.NE.0) THEN
	  IF(BUF.LT.1.OR.BUF.GT.LANBNUM) THEN
	    TYPE*,'**** ILLEGAL REL BUF NUMBER ...: ',BUF
	    CALL GPAUSE
	    TYPE*,'**** 2 ILLEGAL REL BUF NUMBER...: ',BUF
	    RETURN
  	  ENDIF
	ENDIF
C
	LANBUF(-1,BUF)=0
	HLANBUF(-1,BUF)=ETHLENMX
	HLANBUF(0,BUF)=0
	LANBUF(LANBTYP,BUF)=0
C
	IF(LANTEST.NE.0) THEN
	 CALL CHKQUEUE(BUF,LANFREE,TIMES)
	 IF(TIMES.NE.0) THEN
	  TYPE*,'**** FREE LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  CALL GPAUSE
	  TYPE*,'**** 3 FREE LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  RETURN
	 ENDIF
	 CALL CHKQUEUE(BUF,LANEXTRA,TIMES)
	 IF(TIMES.NE.0) THEN
	  TYPE*,'**** EXTRA LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  CALL GPAUSE
	  TYPE*,'**** 4 EXTRA LIST CORRUPTED (BUF,COUN)...: ',BUF,TIMES
	  RETURN
	 ENDIF
	 DO 100 QUE=1,LANMAXTSK
	 CALL CHKQUEUE(BUF,LANFRAP(1,QUE),TIMES)
	 IF(TIMES.NE.0) THEN
	  TYPE*,'**** FRAP LIST CORRUPTED (Q,B,C)...: ',QUE,BUF,TIMES
	  CALL GPAUSE
	  TYPE*,'**** 5 FRAP LIST CORRUPTED (Q,B,C)...: ',QUE,BUF,TIMES
	  RETURN
	 ENDIF
100	 CONTINUE
	ENDIF
C
	STATUS=0
C
	QUE=LANBUF(LANLIST,BUF)
	IF      (QUE) 200,300,400
200	CONTINUE
C
C     < 0 EXTRA LIST
C
	LANBUF(LANOWN,BUF)=OWNFREE
	CALL ABL(BUF,LANEXTRA,STATUS)
	GOTO 500
300	CONTINUE
C
C     =0 FREE LIST
C
	LANBUF(LANOWN,BUF)=OWNFREE
	CALL ABL(BUF,LANFREE,STATUS)
	GOTO 500
400	CONTINUE
C
C     >0 FRAP LIST
C
	LANBUF(LANOWN,BUF)=OWNFRAP
	CALL ABL(BUF,LANFRAP(1,QUE),STATUS)
C
500	CONTINUE
C
	IF(STATUS.NE.0) THEN
D        TYPE*,'**** BAD FREE LIST (REL) BUF,ADR,QUE..: ',BUF,QUE
         CALL OPS('**** FREE LIST CORRUPTED ****',BUF,QUE)
	ENDIF
C
	BUF=0
	RETURN
	END
