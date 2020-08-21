C
C SUBROUTINE TSTCHG2
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]TSTCHG2.FOV                                  $
C  $Date::   17 Apr 1996 15:39:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     TSTCHG2(BUFNR,VALUE,FINAL) - WILL LOCK THE DATA, INCREMENTED
C                                DATA WILL BE PUT IN FINAL
C     IN - DATA - BUFNR TO BE LOCKED [0,7FFFFFFF]
C        - INCREMENT - VALUE TO BE ADDED TO DATA
C     OUT - FINAL - RESULT
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
	SUBROUTINE TSTCHG2(DATA,INCREMENT,FINAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INTEGER*4 FINAL, INCREMENT, DATA
C
10	CONTINUE
C***	DELAY=TESET(NETIOCNT(DATA))
C***	IF (DELAY) THEN
C***	  CALL XWAIT(4,1,ST)
C***	  GOTO 10
C***	ENDIF
	FINAL=NETIOCNT(DATA)
	FINAL=IAND(FINAL,'7FFFFFFF'X)
C
	FINAL=FINAL+INCREMENT
	IF(FINAL.LT.0)FINAL=0
	NETIOCNT(DATA)=FINAL
	RETURN
	END
