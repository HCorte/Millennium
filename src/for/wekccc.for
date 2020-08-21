C WEKCCC.FOR
C
C V01 03-JAN-2011 HXK Initial release for Portugal (LOTTO2 CHANGES)
C  
C INPUT:
C    WEEK    - WEEK FOR WHICH YOU WANT THE CCC (DRAW ID) NUMBER
C    INYEAR  - YEAR
C    GNUM    - GAME NUMBER
C
C OUTPUT:
C    WEKCCC - WEEK
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
        INTEGER*4 FUNCTION WEKCCC(INYEAR,WEEK,GNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 WEEK, INYEAR, GNUM
	INTEGER*4 YEAR, DRAW, ST
	INTEGER*4 GTYP,GIND
C
C
	WEKCCC = 0
	YEAR = 0
C
C CHECK FOR VALID INPUT
C
	IF (WEEK.LE.0.OR.WEEK.GT.53) RETURN
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
        IF (YEAR.LE.1.AND.YEAR.GE.-4) THEN
           DRAW = WEKDRW(YEAR,WEEK,GNUM)            !get draw from week
           CALL GETWEK(DRAW,GNUM,WEKCCC,INYEAR,ST)  !get ccc from draw 
	ENDIF
C
	RETURN
C
	END
