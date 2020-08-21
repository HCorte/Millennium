C  GXSRC:FPTLOG.FOR
C  
C  $Log:   GXAFIP:[GOLS]FPTLOG.FOV  $
C  
C     Rev 1.0   13 Feb 1997 15:19:20   WPW
C  Initial revision.
C  
C     Rev 1.0   17 Apr 1996 15:13:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   25 Sep 1994 12:20:16   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 22:56:04   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:33:56   SYSTEM
C  Initial revision.
C FPTLOG
C
C V01 15-MAY-93 DSL INITIAL RELEASE FOR GEORGIA  (FPT0001)
C
C SUBROUTINE TO PROCESS FINANCIAL PASS-THROUGH STRATUS RESPONSE MSGS
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
	SUBROUTINE FPTLOG(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'					!CDH0001
C
        INTEGER*4 IND, LENGTH, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C
C GET CROSS REFERENCE NUMBER
C
	IND=5
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	I1TEMP(3)=OUTTAB(IND+2)
	I1TEMP(4)=OUTTAB(IND+3)
	TRABUF(TSDT1)= I4TEMP
C    
C   CHECK RESULT CODE
C
	IND=13
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TSDT2)=I4TEMP
	IF(TRABUF(TSDT2).NE.99) THEN
	    TRABUF(TERR)=BCRS
	    TRABUF(TSTAT)=REJT
	ENDIF
C
	RETURN
	END
