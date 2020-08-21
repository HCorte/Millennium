C LOAD_WEKDRW.FOR
C
C V06 16-JAN-2011 HXK/FRP Apply CCC year to CCCDRW table and WEEK year to WEKDRW table
C V05 08-DEC-2010 HXK Update CCCDRW table in similar way to WEKDRW table
C V04 30-MAY-2001 ÈP  Release to load table even without an active draw 
C                     for the game
C V03 16-FEB-2001 ÈPH  Use FIGWEKP for Portugal Week
C V02 15-FEB-2001 ÈPH  Sunday must be first day in the week
C V01 11-DEC-2000 ANG Initial release for Portugal.
C  
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
C Copyright 2010 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C THIS PROGRAM LOADS A MEMORY TABLE WITH DRAW NUMBERS PER GAME FOR CURRENT 
C YEAR, NEXT YEAR AND LAST FOUR YEARS. THIS TABLE IS USED TO FIND A DRAW 
C NUMBER PASSING YEAR/WEEK. IT ALSO LOADS A SIMILAR DRAW TABLE FOR THE
C TOTOLOTO DRAW COUNTER SO THAT DRAW CAN BE FOUND PASSING DRAW COUNTER (CCC)
C I.E YEAR/CCC 
C
C
C TABELA PARA OBTER O NUMERO DO CONCURSO A PARTIR DE UMA SEMANA E UM ANO 
C INFORMADOS PELO USUARIO
C
C OFFSETS:
C 
C WEKDRW(YEAR,WEEK,GNUM)
C         |     |   |------ GAME NUMBER
C         |     |---------- WEEK NUMBER (1 - 53)
C         |---------------- YEAR (-4 = CURRENT YEAR - 4
C                                 -3 = CURRENT YEAR - 3
C                                 -2 = CURRENT YEAR - 2  
C                                 -1 = CURRENT YEAR - 1
C                                  0 = CURRENT YEAR 
C                                  1 = CURRENT YEAR + 1)
C
C CCCDRW(YEAR,CCC,2,4)
C         |    |  | |------ 1=DRAW,   2=DRAW_CDC,    3=GNUM,   4=WEEK
C         |    |  |-------- 1=LOTTO3, 2=LOTTO4
C         |    |----------- CCC NUMBER (1 - 106)
C         |---------------- YEAR (-4 = CURRENT YEAR - 4
C                                 -3 = CURRENT YEAR - 3
C                                 -2 = CURRENT YEAR - 2  
C                                 -1 = CURRENT YEAR - 1
C                                  0 = CURRENT YEAR 
C                                  1 = CURRENT YEAR + 1)
C
    
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOAD_WEKDRW()
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

	INTEGER*4 GAM,GTYP
C
C	LOAD TABLE
C
	DO GAM=1,MAXGAM
          GTYP = GNTTAB(GAMTYP, GAM)
          IF(GTYP .GE. 1 .AND. GTYP .LE. MAXTYP) THEN
            CALL LOAD_TABLE(GTYP, GAM)
          ENDIF
	ENDDO

	RETURN
	END

    
C*********************************************
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE LOAD_TABLE(GTYP,GAM)
        IMPLICIT NONE
C*********************************************
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*4 WEEK,GAM,FDB(7),ST,DRAW,CCC,DRWCDC,GIND
        INTEGER*4 YEARIN,YEAR,I,GTYP,CIV,NEGDRWCDC,DUMMY
        INTEGER*4 CCC_YEAR
        INTEGER*4 CCC_YEARIN
	LOGICAL   FIRST


	FIRST=.TRUE.
	CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)

	IF (GTYP.EQ.TLTO) THEN
            CALL IOINIT(FDB,3,DLTSEC*256)
	ELSEIF (GTYP.EQ.TSPT) THEN
            CALL IOINIT(FDB,3,DSPSEC*256)
	ELSEIF (GTYP.EQ.TKIK) THEN
            CALL IOINIT(FDB,3,DKKSEC*256)
	ELSEIF (GTYP.EQ.TPAS) THEN
	    CALL IOINIT(FDB,3,DPASEC*256)
	ELSEIF (GTYP.EQ.TTGL) THEN
	    CALL IOINIT(FDB,3,DTGSEC*256)
	ELSE
	    RETURN
	ENDIF

        IF(ST.NE.0) THEN
           CALL FILERR(GFNAMES(1,GAM),1,ST,0)

	   TYPE*,IAM()
	   WRITE(6,110) IAM(),GAM,(GLNAMES(I,GAM),I=1,4)
	   TYPE*,IAM()

	   RETURN
        ENDIF

        WRITE(6,100) IAM(),GAM,(GLNAMES(I,GAM),I=1,4)
	DRAW = 1
	ST   = 0
	CIV  = 0

	DO WHILE (ST.EQ.0)
	   WEEK   = 0
	   YEARIN = 0
	   YEAR   = 0
	   IF (GTYP.EQ.TLTO) THEN
               CALL READW(FDB,DRAW,DLTREC,ST)
               IF(DLTDRW.EQ.0) ST=144    !Force end of file when an empty record is readed
	       CIV = DLTBSD
               GIND=GNTTAB(GAMIDX,GAM)
	       IF(GIND.GT.2) CIV=DLTESD
               CCC = DLTCCC
               DRWCDC = DLTDAT(CURDRW)
	   ELSEIF (GTYP.EQ.TSPT) THEN
               CALL READW(FDB,DRAW,DSPREC,ST)
               IF(DSPDRW.EQ.0) ST=144    !Force end of file when an empty record is readed               
               CIV = DSPBSD
	   ELSEIF (GTYP.EQ.TKIK) THEN
               CALL READW(FDB,DRAW,DKKREC,ST)
               IF(DKKDRW.EQ.0) ST=144    !Force end of file when an empty record is readed                              
               CIV = DKKBSD
	   ELSEIF (GTYP.EQ.TPAS) THEN           
               CALL READW(FDB,DRAW,DPAREC,ST)
               IF(DPAEMIS.EQ.0) THEN
               	 ST=144    !Force end of file when an empty record is readed
               ELSE
	         CALL GETPASDRW(DPADRAW,WEEK,YEARIN)
	         CIV = DPABSD   !JUST TO CHECK IF GAME IS OK               	
               ENDIF	 
	   ELSEIF (GTYP.EQ.TTGL) THEN
               CALL READW(FDB,DRAW,DTGREC,ST)
               IF(DTGDRW.EQ.0) ST=144    !Force end of file when an empty record is readed
               CIV = DTGBSD
           ENDIF

           IF(ST.NE.0.AND.ST.NE.144) THEN
               CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
	       TYPE*,IAM()
	       WRITE(6,110) IAM(),GAM,(GLNAMES(I,GAM),I=1,4)
	       TYPE*,IAM()
	       RETURN
           ELSEIF (ST.EQ.0) THEN
               IF (CIV.NE.0) THEN 
		   IF (GTYP.NE.TPAS) THEN
		      CALL FIGWEK (CIV,WEEK,YEARIN)
		   ENDIF
	           IF(GTYP.EQ.TLTO) THEN
                      GIND=GNTTAB(GAMIDX,GAM)
	              IF(GIND.GT.2) THEN
	                NEGDRWCDC = 0-DRWCDC
	                CALL FIGCCC(NEGDRWCDC,DUMMY,CCC_YEARIN)
	              ENDIF
                      IF (CCC_YEARIN.LT.0) THEN
                        TYPE*,IAM()
                        WRITE(6,111) IAM(),GAM,WEEK,CCC_YEARIN,DRAW
                        TYPE*,IAM()
                        CALL GPAUSE()
                      ENDIF
	           ENDIF
		   IF (YEARIN.LT.0.OR.WEEK.LE.0.OR.WEEK.GT.53) THEN
	              TYPE*,IAM()
	              WRITE(6,111) IAM(),GAM,WEEK,YEARIN,DRAW
	              TYPE*,IAM()
		      CALL GPAUSE()
		   ELSE
		      YEAR = MOD(YEARIN,100) - DAYYER
                      CCC_YEAR = MOD(CCC_YEARIN,100) - DAYYER
		      IF (YEAR.LE.1.AND.YEAR.GE.-4) THEN
		          WEKDRW(YEAR,WEEK,GAM) = DRAW
	                  IF (GTYP.EQ.TLTO) THEN
                              GIND=GNTTAB(GAMIDX,GAM)
	                      IF(CCC.NE.0.AND.GIND.GT.2) THEN 
                                IF(FIRST) THEN
                                  WRITE(6,112) IAM(),GAM,(GLNAMES(I,GAM),I=1,4)
	                          FIRST=.FALSE. 
	                        ENDIF
                                CCCDRW(CCC_YEAR,CCC,GIND-2,LTGCCC_DRAW) = DRAW
                                CCCDRW(CCC_YEAR,CCC,GIND-2,LTGCCC_DCDC) = DRWCDC
                                CCCDRW(CCC_YEAR,CCC,GIND-2,LTGCCC_GNUM) = GAM
                                CCCDRW(CCC_YEAR,CCC,GIND-2,LTGCCC_WEEK) = WEEK
                              ENDIF
	                  ENDIF
		      ELSEIF (YEAR.GT.1) THEN
		          ST = 144
	              ENDIF
	           ENDIF
	       ENDIF
           ENDIF
           DRAW = DRAW + 1
	ENDDO

	CALL CLOSEFIL(FDB)

	RETURN

100     FORMAT(1X,A18,' Loading WEKDRW table for game number ',I2,1X,4A4)
110	FORMAT(1X,A18,' WEKDRW LOAD NOT SUCCESSFULLY COMPLETED FOR GAME NUMBER ',I2,1X,4A4)
111	FORMAT(1X,A18,' Invalid week/year for game ',I2,' week/year = ',I2.2,'/',I4,' drw ',I4) 
112     FORMAT(1X,A18,' Loading CCCDRW table for game number ',I2,1X,4A4)
	END
