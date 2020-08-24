C
C PROGRAM CRTTCW
C $Log:   GXAFXT:[GOLS]CRTTCW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:44:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   30 Jan 1994 21:55:36   HXK
C  Initial revision.
C  
C ** Source - CRTTCW.for **
C
C CRTTCW.FOR
C
C
C CARRYOVER MERGE FILE CREATION TASK
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


	PROGRAM CRTTCW
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INTEGER*4 FDB(7), K, FLAG, ST
C
C
	CALL COPYRITE
C
C
C
	CALL OPENX(1,'SCF.FIL',4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),' SCF.FIL open error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),' SCF.FIL read error > ',ST
	  CALL GPAUSE
	ENDIF
	CALL CLOSEFIL(FDB)
C
C CREATE NEW FILES
C
	CALL CRTFIL(SCFSFN(1,TCW),SCFFSZ(TCW),ST)
	IF(ST.NE.0) THEN
  	   WRITE(5,903) IAM(),(SCFSFN(K,TCW),K=1,5)
	   CALL GPAUSE
	ELSE
           WRITE(5,904) IAM(),(SCFSFN(K,TCW),K=1,5)
        ENDIF
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C
903	FORMAT(1X,A,' Error while allocating ',5A4)
904     FORMAT(1X,A,1X,5A4,' successfully allocated')
	END
