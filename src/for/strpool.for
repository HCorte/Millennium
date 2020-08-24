C
C SUBROUTINE STRPOOL
C $Log:   GXAFXT:[GOLS]STRPOOL.FOV  $
C  
C
C SUBROUTINE TO DUMP TODAY'S TRIPLE POOL TO DISK
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expretrly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE STRPOOL(GIND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TRFREC.DEF'
C
C
	INTEGER*4 FDB(7)
	INTEGER*4 GIND, ST
C
C OPEN POOL FILE AND READ FIRST RECORD
C
10	CONTINUE
	CALL OPENQW(4,TRPPFN(1,GIND),4,0,0,ST)
	CALL IOQINIT(FDB,4,TRFSEC*256)
	IF(ST.NE.0) THEN
	   CALL FILERR(TRPPFN(1,GIND),1,ST,0)
	   GOTO 10
	ENDIF
C
	CALL READQW(FDB,1,TRFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(TRPPFN(1,GIND),2,ST,1)
	   CALL CLOSEQFIL(FDB)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C SAVE POOL ARRAYS TO FILE
C
        TRFLAMT = TROLAMT(GIND) 
        TRFFEL  = TROFEL(GIND) 
        TRFLEL  = TROLEL(GIND) 
        TRFTNUM = TROTNUM(GIND)
        CALL FASTMOV(TRODDS(1,1,GIND),TRFODDS,2*TRGPOL)
C
C WRITE POOLS TO FILE
C
	CALL WRITEQW(FDB,1,TRFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(TRPPFN(1,GIND),3,ST,1)
	   CALL CLOSEFIL(FDB)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
	CALL CLOSEQFIL(FDB)
	RETURN
	END

