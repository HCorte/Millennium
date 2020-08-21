C
C SUBROUTINE READX2X
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]READX2X.FOV                                  $
C  $Date::   17 Apr 1996 14:39:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xsubs.for;1 **
C
C
C
C     ========================================================
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
	SUBROUTINE READX2X(LUN,REC,X2XBUF,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XSUBS.DEF'
C
	INTEGER*4 X2XBUF(64*X2XSEC), NDX, BLK, ST, REC
C
	ST=0
	BLK=(REC-1)/RECSPERBKT + 1
	NDX=MOD(REC-1,RECSPERBKT) + 1
C
        IF(NDX .LT. 1)THEN
          ST = 16
          GOTO 8000
        ENDIF
C
	IF(BLK.NE.BKTNUM(LUN))THEN
	  IF(BKTCHG(LUN).NE.0)THEN
	    CALL WRITEW(FDB(1,LUN),BKTNUM(LUN),BUCKET(1,1,LUN),ST)
	    IF(ST.NE.0)THEN
	      CALL OS32ER(5,X2XNAM(LUN),'WRITEW',ST,BKTNUM(LUN))
	      CALL GPAUSE
	      BKTNUM(LUN)=-1
	      BKTCHG(LUN)=0
	      GOTO 8000
	    ENDIF
	  ENDIF
C
	  CALL READW(FDB(1,LUN),BLK,BUCKET(1,1,LUN),ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(ST.NE.0)THEN
	    CALL OS32ER(5,X2XNAM(LUN),'READW',ST,BKTNUM(LUN))
	    CALL GPAUSE
	    BKTNUM(LUN)=-1
	    BKTCHG(LUN)=0
	    GOTO 8000
	  ENDIF
	  BKTNUM(LUN)=BLK
	  BKTCHG(LUN)=0                !NO CHANGE MADE YET
	ENDIF
C
	CALL FASTMOV(BUCKET(1,NDX,LUN),X2XBUF,64*X2XSEC)
C
8000    CONTINUE
	RETURN
	END
