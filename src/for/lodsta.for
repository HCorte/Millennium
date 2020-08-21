C
C V01 05-JAN-2000 UXN Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

	PROGRAM LODSTA
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSTF.DEF'
        INCLUDE 'INCLIB:STACOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
C
	INTEGER*4 ST, FDB(7), GNUM, GIND, I, J, K 
	INTEGER*4 ROWTOT
	REAL*4    RROWTOT
C
	TYPE*,IAM(),'Loading VAKIO statistics'
        
        CALL OPENW(3,SFNAMES(1,STF),4,0,0,ST)
        CALL IOINIT(FDB,3,STFSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,STF),1,ST,0)

        CALL READW(FDB,1,STFREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,STF),2,ST,1)
        CALL CLOSEFIL(FDB)
      
	DO 100 GIND=1,NUMSPT
	   GNUM = GTNTAB(TSPT, GIND)
	   IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) GOTO 100
	    
      	   STASPT_CUP(GIND)=STFSPT_CUP(GIND)

      	   ROWTOT=0
      	   DO K=1,3
      	     ROWTOT=ROWTOT+STFSPT_TAB2(1,K,GIND)
      	   ENDDO
      	   IF(ROWTOT.EQ.0) ROWTOT=1
      	   RROWTOT=FLOAT(ROWTOT)
      	   SPSVER(GIND)=1
      	   SPSUPT(GIND)=P(ACTTIM)

      	   DO I=1,SPGNBR
      	     DO J=1,7
      	      STASPT_TAB1(I,J,GIND)=STFSPT_TAB1(I,J,GIND)
      	     ENDDO
      	     DO K=1,3
      	       STASPT_TAB2(I,K,GIND)=STFSPT_TAB2(I,K,GIND)
      	       SPSROP(I,K,GIND)=
     *		 JNINT(1.E4*FLOAT(STFSPT_TAB2(I,K,GIND))/RROWTOT)
      	     ENDDO
      	   ENDDO
100	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
	END
