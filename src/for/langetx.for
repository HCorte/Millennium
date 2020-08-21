C
C SUBROUTINE LANGETX
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LANGETX.FOV                                  $
C  $Date::   17 Apr 1996 13:47:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - langetx.for;1 **
C
C LANGETX.FOR
C
C V01 10-SEP-90 MRM RELEASED FOR VAX
C
C CALL LANGETX(BUF,STATUS)
C
C      BUF   - BUFFER NUMBER (FTN)
C      STATUS- AS IN RTL/ABL
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
	SUBROUTINE LANGETX(BUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 STATUS, QUE, BUF
C
	STATUS=0
	BUF=0
C
	CALL RTL(BUF,LANFREE,STATUS)
	IF(STATUS.EQ.2) THEN
          CALL RTL(BUF,LANEXTRA,STATUS)
	ENDIF
C
	IF(STATUS.NE.2) THEN
	 IF(LANTEST.NE.0) THEN
	  IF(BUF.LT.1.OR.BUF.GT.LANBNUM) THEN
	   TYPE*,'ILLEGAL GET BUF NUMBER ...: ',BUF
	   CALL GPAUSE
	   STATUS=2
	  ENDIF
	 ENDIF
C
	 HLANBUF(-1,BUF)=ETHLENMX
	 HLANBUF(0,BUF)=0
	 CALL FASTSET(0,LANBUF(1,BUF),LANDATAF-1)
	 LANBUF(LANBTYP,BUF)=0
	 LANBUF(LANOWN,BUF)=OWNFX
D	 TYPE*,'**** GETX BUF ****[',BUF,']'
	ENDIF
	RETURN
	END
