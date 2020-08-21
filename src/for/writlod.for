C
C SUBROUTINE WRITLOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]WRITLOD.FOV                                  $
C  $Date::   17 Apr 1996 16:05:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodsub.for;1 **
C
C
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
	SUBROUTINE WRITLOD(LUN,REC,X2XBUF,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2LODSUB.DEF'
C
	INTEGER*4 X2XBUF(64*X2XSEC), NDX, BLK, ST, REC
C
	ST=0
	BLK=(REC-1)/RECSPERBKT + 1
	NDX=MOD(REC-1,RECSPERBKT) + 1
C
	IF(BLK.NE.BKTNUM(LUN))THEN
	  IF(BKTCHG(LUN).NE.0)THEN
	    CALL WRITEW(FDB(1,LUN),BKTNUM(LUN),BUCKET(1,1,LUN),ST)
	    IF(ST.NE.0)THEN
	      CALL OS32ER(5,X2XNAM(LUN),'WRITEW',ST,BKTNUM(LUN))
	      CALL GPAUSE
	      BKTNUM(LUN)=-1
	      BKTCHG(LUN)=0
	      RETURN
	    ENDIF
	  ENDIF
C
	  CALL READW(FDB(1,LUN),BLK,BUCKET(1,1,LUN),ST)
          IF(ST.NE.0)THEN
            BKTNUM(LUN)=-1
            BKTCHG(LUN)=0
            IF(ST.NE.144) THEN
              CALL OS32ER(5,X2XNAM(LUN),'READW',ST,BLK)
              PAUSE
            ENDIF
            RETURN
          ENDIF
	  BKTNUM(LUN)=BLK
	ENDIF
C
	CALL FASTMOV(X2XBUF,BUCKET(1,NDX,LUN),64*X2XSEC)
	BKTCHG(LUN)=1
C
	RETURN
	END
