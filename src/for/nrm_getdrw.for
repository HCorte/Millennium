C NRM_GETDRW.FOR
C
C V03 10-DEC-2010 HXK HANDLE DRAW ID FOR TOTOLOTO
C V02 12-JAN-2001 EPH DONT CHANGE INYEAR VALUE
C V01 12-DEC-2000 ANG Initial release for Portugal.
C  
C INPUT:
C    YEAR
C    WEKCCC (i.e. week OR ccc, depending if game is or isn't TOTOLOTO)
C    GAME NUMBER
C
C OUTPUT:
C    DRAW NUMBER
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
C Copyright 2010,1999 GTECH Corporation. All rights reserved.         
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C    
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        INTEGER*4 FUNCTION GETDRW(AUXYEAR,WEKCCC,GNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 INYEAR, WEKCCC, GNUM
        INTEGER*4 YEAR, AUXYEAR       !V02
	INTEGER*4 GTYP, GIND
        LOGICAL   TOTOLOTO !totoloto (Lotto3 and Lotto4) uniquely use a draw 
C                          !counter rather than week
C
	GETDRW = 0
        INYEAR = AUXYEAR           !V02
	INYEAR = MOD(INYEAR,100)
C
C CHECK FOR VALID INPUT
C
	IF (INYEAR.LT.0.OR.INYEAR.GT.99) THEN
            GETDRW = 0
	    RETURN
	ENDIF
C
	IF (GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
            GETDRW = 0
	    RETURN
	ENDIF
C
	GTYP = GNTTAB(GAMTYP,GNUM)
	GIND = GNTTAB(GAMIDX,GNUM)
C
	IF (GTYP.LE.0.OR.GTYP.GT.MAXTYP) THEN
            GETDRW = 0
	    RETURN
	ENDIF
C
	IF (GIND.LE.0.OR.GIND.GT.MAXIND) THEN
            GETDRW = 0
	    RETURN
	ENDIF
C
	TOTOLOTO=.FALSE.
	IF (GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
	  IF (WEKCCC.LE.0.OR.WEKCCC.GT.106.OR.GIND.GT.4) THEN     
            GETDRW = 0
	    RETURN
	  ENDIF
	  TOTOLOTO=.TRUE.
	ELSE
	  IF (WEKCCC.LE.0.OR.WEKCCC.GT.53) THEN     
            GETDRW = 0
	    RETURN
	  ENDIF
	ENDIF
C
C GET DRAW NUMBER FROM TABLE MEMORY
C
	YEAR = INYEAR - DAYYER
C
C CHECK IF THIS YEAR IS IN TABLE
C
	IF (YEAR.LE.1.AND.YEAR.GE.-4) THEN
	  IF (TOTOLOTO) THEN
	    GETDRW = CCCDRW(YEAR,WEKCCC,GIND-2,LTGCCC_DRAW)
	  ELSE
	    GETDRW = WEKDRW(YEAR,WEKCCC,GNUM)
	  ENDIF 
	ELSE
          GETDRW = 0
	ENDIF
        RETURN
C
	END
C
C ========================================================================== 
C
C FUNCTION: GET TOTOLOTO GAME
C
        INTEGER*4 FUNCTION GETTOTOLOTOGAM(AUXYEAR,CCC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 INYEAR, CCC
        INTEGER*4 YEAR, AUXYEAR       !V02
	INTEGER*4 GTYP, GIND, G
C
C
	GETTOTOLOTOGAM = 0
        INYEAR = AUXYEAR           !V02
	INYEAR = MOD(INYEAR,100)
C
C CHECK FOR VALID INPUT
C
	IF (INYEAR.LT.0.OR.INYEAR.GT.99) THEN
            GETTOTOLOTOGAM = 0
	    RETURN
	ENDIF
C
	GTYP = TLTO
	GIND = 3      !default is Lotto3
	G    = GIND-2
C
	IF (CCC.LE.0.OR.CCC.GT.106) THEN     
            GETTOTOLOTOGAM = 0
	    RETURN
	ENDIF
C
C GET DRAW NUMBER FROM TABLE MEMORY
C
	YEAR = INYEAR - DAYYER
C
C CHECK IF THIS YEAR IS IN TABLE
C
	IF (YEAR.LE.1.AND.YEAR.GE.-4) THEN
	  GETTOTOLOTOGAM = CCCDRW(YEAR,CCC,G,LTGCCC_GNUM)
	  IF(GETTOTOLOTOGAM.EQ.0) THEN
            GETTOTOLOTOGAM = CCCDRW(YEAR,CCC,G+1,LTGCCC_GNUM)
	  ENDIF
	ELSE
          GETTOTOLOTOGAM = 0
	ENDIF
        RETURN
C
	END

