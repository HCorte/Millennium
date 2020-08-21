C CCCWEK.FOR
C
C V01 03-JAN-2011 HXK Initial release for Portugal (LOTTO2 CHANGES)
C  
C INPUT:
C    CCC  - CCC (DRAW ID) NUMBER FOR WHICH YOU WANT THE WEEK
C    YEAR
C    GNUM - GAME NUMBER
C
C OUTPUT:
C    CCCWEK - WEEK
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 2011,1999 GTECH Corporation. All rights reserved.            
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C    
!=======OPTIONS /CHECK=NOOVERFLOW/EXT
        INTEGER*4 FUNCTION CCCWEK(INYEAR,CCC,GNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 CCC, INYEAR, GNUM
	INTEGER*4 YEAR
	INTEGER*4 GTYP,GIND
C
C
	CCCWEK = 0
	YEAR = 0
C
C CHECK FOR VALID INPUT
C
	IF (CCC.LE.0.OR.CCC.GT.106) RETURN
C
        IF (GNUM.NE.6.AND.GNUM.NE.7) RETURN
C
	GTYP = GNTTAB(GAMTYP,GNUM)
	GIND = GNTTAB(GAMIDX,GNUM)
C
        IF (GTYP.NE.TLTO) RETURN
C
        IF (GIND.NE.3.OR.GIND.NE.4) RETURN
C
        YEAR = INYEAR - DAYYER
C
C CHECK IF THIS YEAR IS IN TABLE, IF SO GET WEEK CORRESPONDING TO CCC
C
        IF (YEAR.LE.1.AND.YEAR.GE.-4) CCCWEK = CCCDRW(YEAR,CCC,GIND-2,
     *                                                LTGCCC_WEEK)
C
	RETURN
C
	END
