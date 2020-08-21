C
C PROGRAM X2BLDGRP
C $Log:   GXAFXT:[GOLS]X2BLDGRP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:09:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   30 Sep 1993 20:33:22   LMK
C  Initial revision.
C  
C     Rev 1.1   22 Jan 1993 14:19:04   DAB
C  Comm Update
C
C ** Source - x2blDGRP.for **
C
C X2BLDGRP.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
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
	PROGRAM X2BLDPRT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*2   DATBUF(12)      !Date buffer
	INTEGER*4   SYSDATE(3)      !System date
	INTEGER*4   SREC, ST
	INTEGER*4   GROUP
C
	CHARACTER   X2FILNAM*20     !File name function
C
C
C
	CALL COPYRITE
C
C CLEAR SCREEN AND DISPLAY TITLE.
C
	CALL CLRSCR(5)
	WRITE(5,9050)
C
C PROMPT FOR INPUT DATA.
C
C
C OPEN THE GROUP FILE.
C
	CALL OPENX(2,X2FILNAM(XGRP),4,0,0,ST)
	CALL IOINIT(X2XGRP_FDB,2,X2XGRP_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGRP),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C DETERMINE THE CDC DATE FROM THE SYSTEM DATE.
C
	CALL XDAT(SYSDATE)
	DATBUF(VYEAR)=SYSDATE(1)
	DATBUF(VMON)=SYSDATE(2)
	DATBUF(VDAY)=SYSDATE(3)
	CALL BDATE(DATBUF)
C
C READ THE NEXT RECORD FROM THE BLD FILE.
C
	DO 100 GROUP=1,1024
C
C ================== GROUP FILE UPDATE ==================
C
C
          X2XGRP_GROUP = GROUP
          X2XGRP_STATE = 1
          X2XGRP_DESC  = 'X.21 RELAY  '
          X2XGRP_UPDATE = DATBUF(VCDC)
	  CALL WRITEW(X2XGRP_FDB,GROUP,X2XGRP_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGRP),'WRITEW',ST,SREC)
	    CALL GPAUSE
          ENDIF
100	CONTINUE
C
C PROGRAM EXIT.
C
	CALL CLOSEFIL(X2XGRP_FDB)
	CALL USRCLOS1(6)
C
C     ===================== Format Statements ======================
C
9050	FORMAT(//,T26,'GTECH Distributed Network',/,
     *	          T28,'Build Test Groups',//)
	END
