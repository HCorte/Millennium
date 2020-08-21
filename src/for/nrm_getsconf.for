C SUBROUTINE GETSCONF
C  
C V03 10-MAR-2000 OXK  Search for free LUN added
C V02 01-AUG-1990 XXX  RELEASED FOR VAX
C V01 15-NOV-1989 GCAN INITIAL RELEASE FOR FINLAND
C
C SUBROUTINE TO GET THE SYSTEM CONFIGURATION RECORD FROM THE SCF.FIL
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
	SUBROUTINE GETSCONF(RECORD,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
C
	INTEGER*4 SCFLU
	LOGICAL*4 ISTHERE
C
	INTEGER*4 SFDB(7)
	INTEGER*4 SCFNAM(5)
	INTEGER*4 RECORD(*), I, ST
	DATA SCFNAM/'GXTS','K:SC','F.FI','L   ','    '/    
C
	ST=-1
C
C Find a free LUN to use
C
        DO 10 SCFLU = 1, 200
          INQUIRE(UNIT=SCFLU, OPENED=ISTHERE, IOSTAT=ST)
          IF(ST.EQ.0.AND..NOT.ISTHERE) GOTO 20
10      CONTINUE
	TYPE*,IAM(),'GETSCONF(): NO LUN AVAILABLE BETWEEN 1..200!'
	RETURN
20      CONTINUE
C
C OPEN THE SCF.FIL AND READ THE CONFIGURATION
C
	CALL OPENW(SCFLU,SCFNAM,4,0,0,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCFNAM,1,ST,0)
	   RETURN
	ENDIF
	CALL IOINIT(SFDB,SCFLU,SCFSEC*256)
	CALL READW(SFDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	   CALL FILERR(SCFNAM,2,ST,1)
	   CALL CLOSEFIL(SFDB)
	   RETURN
	ENDIF
	CALL CLOSEFIL(SFDB)
C
C IF NO VOLUME NAME, SET TO SYSTEM VOLUME
C
	DO 100 I=1,MAXFIL
	   IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
100	CONTINUE
C
	DO 200 I=1,MAXGAM
	   IF(SCFGFN(1,I).EQ.'    ') CALL SYSVOL(SCFGFN(1,I))
200	CONTINUE
C
C MOVE RECORD OVER TO OUTPUT RECORD
C
	CALL FASTMOV(SCFREC,RECORD,SCFLEN)
C
	ST=0
	RETURN
	END
