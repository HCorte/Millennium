C  GXSRC:SPE_ISSLOG.FOR
C  
C V03 06-OCT-2000 UXN AlphaIPS release.
C V02 13-JUL-1993 MCM ADDED GVTID
C V01 28-APR-1993 TJR INITIAL RELEASE FOR GEORGIA
C
C SUBROUTINE TO PROCESS INTRA-SYSTEM STRATUS RESPONSE MSGS
C  (CENTRAL/STRATUS MSG 11.15.2)
C REFERENCE DESIGN DOC CDH0001
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ISSLOG(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'				
C
        INTEGER*4 IND
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	INTEGER*4   MESS(EDLEN)
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
C GET TRANSACTION TYPE
C
	IND=IND+4
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TSDT2)=I4TEMP
C
C   GET RESULT CODE
C
	IND=IND+4
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TSDT3)=I4TEMP
	IF(TRABUF(TSDT3).NE.99) THEN	    !ONLY SAT CODE IS 99
	    TRABUF(TERR)=BCRS
	    TRABUF(TSTAT)=REJT
C	
C	BUILD ERROR MSG TO INDICATE STRATUS/DEC NOW OUT OF SYNC
C
            MESS(1)=SPE
            MESS(2)=TEGEN
            MESS(3)=39
            MESS(4)=TRABUF(TSER)
	    CALL QUEMES(MESS)
 	ENDIF
8000	CONTINUE
	RETURN
	END
