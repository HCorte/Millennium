C
C PROGRAM CRTTCC
C
C V02 15-DEC-1999 UXN MULTIWIN CHANGES.
C V01 30-JAN-1994 HXK Initial revision.
C
C CARRYOVER COPY FILE CREATION TASK
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT


	PROGRAM CRTTCC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 FDB(7), K, FLAG, ST
C
C
	CALL COPYRITE
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C CREATE NEW FILES
C
	CALL CRTFIL(SCFSFN(1,TCC),SCFFSZ(TCC),ST)
	IF(ST.NE.0) THEN
  	   WRITE(6,903) IAM(),(SCFSFN(K,TCC),K=1,5)
	   CALL GPAUSE
	ELSE
           WRITE(6,904) IAM(),(SCFSFN(K,TCC),K=1,5)
	   TCCSTS = WCLR
        ENDIF
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C
903	FORMAT(1X,A,' Error while allocating ',5A4)
904     FORMAT(1X,A,1X,5A4,' successfully allocated')
	END
