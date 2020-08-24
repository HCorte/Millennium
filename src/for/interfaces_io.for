C
C INTERFACES_IO.FOR                                                                    
C
C V16 29-NOV-2010 FRP Lotto2 Changes
C                 FJG Compilation error
C V15 27-APR-2010 FRP ePassive
C V14 06-APR-2009 FRP Modify for EM Joker
C v13 21-NOV-2002 TRG FIX LINHA_DISTIBUICAO TO CORRECT OFFSET
C V12 25-OCT-2002 TRG CONVERT 2->4 DIGITS YEAR IN AGTMIL
C V11 11-MAY-2001 EPH INCLUDE READING OF LINHA_DISTRIBUICAO / CENTRAL_RECEPCAO / STATUS_TRANSPORTE FROM AGTMIL
C V10 28-MAY-2001 EPH INCLUDE READING OF TIPCENREC FIELD IN OFFWAGFIN DETAIL RECORD
C V09 27-MAY-2001 EPH READS ODJ USING SCML CONVENTION (IF GAME NUMBER GT 5 THEN IT IS ADDED OF 1)
C V08 27-MAY-2001 EPH GENERATES ODJ USING SCML CONVENTION (IF GAME NUMBER GT 5 THEN IT IS SUBTRACTED FROM 1)
C V07 27-MAY-2001 EPH USE ONLY 10 DIGTS OF THE RIGTH FOR THE ACCOUNT IN OCR LINE (WITHOUT DV)
C V06 17-MAY-2001 EPH CHANGE OPEN_BNK TO FIXED SIZE FILE (80 BYTES)
C V05 03-MAY-2001 EPH INCLUDED WORSAP FILE ROUTINES
C V04 27-APR-2001 ANG INCLUDED OFFWAG AND OFFWAGFIN ROUTINES
C V03 28-MAR-2001 EPH INCLUDE ODJ FILE MANIPULATING ROUTINES
C V02 16-FEB-2001 EPH INCLUDE X2XADDRESS
C V01 31-JAN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C ROUTINES FOR OPENING, READING AND WRITING FROM/TO INTERFACE FILES
C READING AND WRITE ROUTINES CODE/DECODE TO RECORDS IN 
C INTERFACES_REC.DEF
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
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C 	**************************************************
     	SUBROUTINE OPEN_OOFWAG (LUN, ST)
C	**************************************************
   	IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 ST
	INTEGER*4 LUN

	OOFWAG_REC.LUN = 0

	CALL CLEAR_OOFWAG_REC

	OOFWAG_REC.LUN = LUN

        OPEN (UNIT   =  OOFWAG_REC.LUN,
     *        FILE   = 'FILE:OOFWAG.ASC',
     *        STATUS = 'OLD', 
     *        IOSTAT =  ST)
	IF (ST.NE.0) THEN    
	   OOFWAG_REC.ERRSTR  = 'Error opening file OOFWAG.ASC'
	ENDIF

	RETURN
	END



C 	**************************************************
     	SUBROUTINE READ_OOFWAG (ST)
C	**************************************************
   	IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4     ST            ! (OUTPUT)

	INTEGER*4     DRAW, GAMEON
	INTEGER*4     GETDRW         !FUNCTION
	INTEGER*4     SZ
	CHARACTER*500 ASCREC
	CHARACTER*3   REFREC
        CHARACTER*50  AUXSTR

	ST = 0
	CALL CLEAR_OOFWAG_REC

  	READ (OOFWAG_REC.LUN, 100, END=200, ERR=300, IOSTAT=ST) ASCREC
100	FORMAT (A500)

        REFREC = ASCREC(1:3)

        IF (REFREC.EQ.'OP1' .OR. REFREC.EQ.'OP2' .OR. REFREC.EQ.'OP9') THEN

           OOFWAG_REC.RECNUM = OOFWAG_REC.RECNUM + 1

	   IF (REFREC.EQ.'OP1') THEN          !***** HEADER *****
	      IF (OOFWAG_REC.RECTYPE.NE.'  ') THEN
C	         * HEADER SHOULD BE THE FIRST RECORD ON FILE *
	         ST = -19
                 AUXSTR = ITOC(OOFWAG_REC.RECNUM,SZ)
                 OOFWAG_REC.ERRSTR = 'Header found in REC = ' // AUXSTR(1:SZ) 
	      ENDIF
              OOFWAG_REC.RECTYPE    = 'HD'
              OOFWAG_REC.YEAR       = CTOI(ASCREC(4:7), SZ) 
              OOFWAG_REC.MONTH      = CTOI(ASCREC(8:9), SZ) 
              OOFWAG_REC.DAY        = CTOI(ASCREC(10:11), SZ) 
              OOFWAG_REC.DATAHD     = ASCREC(10:11) // '/' // ASCREC(8:9) // '/' // ASCREC(4:7)

              IF (OOFWAG_REC.YEAR.LE.0)                              ST = -11    
              IF (OOFWAG_REC.MONTH.LE.0 .OR. OOFWAG_REC.MONTH.GT.12) ST = -11
              IF (OOFWAG_REC.DAY.LE.0 .OR. OOFWAG_REC.DAY.GT.31)     ST = -11
	      IF (ST.EQ.-11) THEN
	         OOFWAG_REC.ERRSTR = 'Invalid date on Header : ' // OOFWAG_REC.DATAHD
              ENDIF
	   ENDIF

           IF (REFREC.EQ.'OP2') THEN          !***** DETAIL *****

	      IF (OOFWAG_REC.RECTYPE.EQ.'  ') THEN
C	         * DETAIL IS THE FIRST RECORD IN FILE *
	         ST = -20
                 OOFWAG_REC.ERRSTR = 'Header not found'
	      ENDIF
	      IF (OOFWAG_REC.RECTYPE.EQ.'TL') THEN
C	         * TRAILLER SHOUL BE THE LAST RECORD *
	         ST = -21
                 OOFWAG_REC.ERRSTR = 'There are other records after the trailler'
	      ENDIF
              OOFWAG_REC.RECTYPE    = 'DT'               
	      OOFWAG_REC.D_WEEK     = CTOI(ASCREC(4:6), SZ)
	      OOFWAG_REC.D_YEAR     = CTOI(ASCREC(7:10), SZ)  

	      OOFWAG_REC.OFF_GAME   = CTOI(ASCREC(11:12), SZ) 
              IF (OOFWAG_REC.OFF_GAME.GE.9) OOFWAG_REC.OFF_GAME = OOFWAG_REC.OFF_GAME + 1   !THERE IS A SHIFT HERE,
										            !BECAUSE OF JOKER
              OOFWAG_REC.AGENT      = CTOI(ASCREC(13:19), SZ) 
	      OOFWAG_REC.BILHETE    = CTOI(ASCREC(20:26), SZ) 
	      OOFWAG_REC.WINS(1)    = CTOI(ASCREC(27:30), SZ) 
	      OOFWAG_REC.WINS(2)    = CTOI(ASCREC(31:34), SZ) 
	      OOFWAG_REC.WINS(3)    = CTOI(ASCREC(35:38), SZ) 
	      OOFWAG_REC.WINS(4)    = CTOI(ASCREC(39:42), SZ) 
	      OOFWAG_REC.WINS(5)    = CTOI(ASCREC(43:46), SZ) 
	      OOFWAG_REC.WINS(6)    = CTOI(ASCREC(47:50), SZ) 
	      OOFWAG_REC.JOKER_DIV  = CTOI(ASCREC(51:51), SZ) 

              IF (OOFWAG_REC.OFF_GAME.LE.0) THEN
                 ST = -12
                 AUXSTR = ITOC(OOFWAG_REC.OFF_GAME,SZ)
	         OOFWAG_REC.ERRSTR = 'Error on Offline game number : ' // AUXSTR(1:SZ)
	      ENDIF
              IF (OOFWAG_REC.D_WEEK.LE.0 .OR. OOFWAG_REC.D_WEEK.GT.106) ST = -13
              IF (OOFWAG_REC.D_YEAR.LE.0)                               ST = -13
              IF (ST.EQ.-13) THEN
                 AUXSTR = ITOC(OOFWAG_REC.D_WEEK*1000+OOFWAG_REC.D_YEAR,SZ)
	         OOFWAG_REC.ERRSTR = 'Invalid Week/year value' // AUXSTR(1:SZ)
              ENDIF                
              IF (OOFWAG_REC.AGENT.LE.0) THEN
                 ST = -14
                 AUXSTR = ITOC(OOFWAG_REC.AGENT,SZ)
 	         OOFWAG_REC.ERRSTR = 'Invalid agent number : ' // AUXSTR(1:SZ)
              ENDIF
              IF (OOFWAG_REC.BILHETE.LE.0) THEN
                 ST = -15
                 AUXSTR = ITOC(OOFWAG_REC.BILHETE,SZ)
	         OOFWAG_REC.ERRSTR = 'Invalid bilhete : ' // AUXSTR(1:SZ)
              ENDIF

	      GAMEON = OOFWAG_REC.OFF_GAME
              IF (GAMEON.GT.MAXGAM .OR. GAMEON.LE.0) THEN
      	         ST = -16
                 AUXSTR = ITOC(OOFWAG_REC.OFF_GAME,SZ)
	         OOFWAG_REC.ERRSTR = 'No Online game to Offline game : ' // AUXSTR(1:SZ) 
              ELSE
	         OOFWAG_REC.ON_GAME    = GAMEON
              ENDIF

	
	      IF (ST.NE.-13 .AND. ST.NE.-16) THEN
                 DRAW = GETDRW (OOFWAG_REC.D_YEAR, OOFWAG_REC.D_WEEK, GAMEON)
		 IF (DRAW.LE.0) THEN
                    ST = -17
                    AUXSTR = ITOC(OOFWAG_REC.D_WEEK*10000+OOFWAG_REC.D_YEAR,SZ)
	            OOFWAG_REC.ERRSTR = 'No equivalent draw to week/year :' // AUXSTR(1:SZ)
		 ELSE
                    IF (OOFWAG_REC.DRAW(GAMEON).NE.0) THEN
                       IF (OOFWAG_REC.DRAW(GAMEON).NE.DRAW) THEN
                          ST = -18
                          AUXSTR = ITOC(OOFWAG_REC.ON_GAME,SZ)
			  OOFWAG_REC.ERRSTR = 'More than one draw for ONLINE game : ' // AUXSTR(1:SZ)
                       ENDIF
                    ELSE
                       OOFWAG_REC.DRAW(GAMEON) = DRAW
                    ENDIF
                 ENDIF
	      ENDIF

	   ENDIF


	   IF (REFREC.EQ.'OP9') THEN          !***** TRAILLER *****
              OOFWAG_REC.RECTYPE= 'TL'               
	      OOFWAG_REC.NUM_RECORDS= CTOI(ASCREC(4:11), SZ) 
              IF (OOFWAG_REC.NUM_RECORDS.NE.OOFWAG_REC.RECNUM) THEN
                 ST = -22
		 OOFWAG_REC.ERRSTR = 'Number of records declared <> found'
              ENDIF
           ENDIF           

        ELSE
           ST = -10   ! INVALID RECORD
	   OOFWAG_REC.ERRSTR = 'Invalid record type : ' // REFREC
        ENDIF

        RETURN       !NORMAL RETURN
	
200	CONTINUE     !END-OF-FILE
        IF (OOFWAG_REC.RECTYPE.NE.'TL') THEN
           ST = -144
           OOFWAG_REC.ERRSTR = 'End of file and Trailler Record not found'
        ELSE
           ST = 144
        ENDIF
        RETURN

300     CONTINUE     !READ ERROR
	OOFWAG_REC.ERRSTR = 'Error reading OOFWAG.ASC'
        RETURN

	END


C	*********************************************************
	SUBROUTINE CLEAR_OOFWAG_REC
C	*********************************************************
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 GAM

	IF (OOFWAG_REC.LUN.EQ.0) THEN     !FIRST TIME (OPEN)
	   OOFWAG_REC.RECTYPE    = '  '

	   OOFWAG_REC.RECNUM     = 0

	   OOFWAG_REC.DAY        = 0
	   OOFWAG_REC.MONTH      = 0
	   OOFWAG_REC.YEAR       = 0
	   OOFWAG_REC.DATAHD     = '  /  /    '

	   OOFWAG_REC.NUM_RECORDS= 0

           DO GAM=1,MAXGAM
              OOFWAG_REC.DRAW(GAM) = 0
           ENDDO
	ENDIF

	OOFWAG_REC.ERRSTR     = '  '

	OOFWAG_REC.OFF_GAME   = 0
        OOFWAG_REC.ON_GAME    = 0
	OOFWAG_REC.D_WEEK     = 0
	OOFWAG_REC.D_YEAR     = 0
        OOFWAG_REC.AGENT      = 0
	OOFWAG_REC.BILHETE    = 0
	OOFWAG_REC.WINS(1)    = 0
	OOFWAG_REC.WINS(2)    = 0
	OOFWAG_REC.WINS(3)    = 0
	OOFWAG_REC.WINS(4)    = 0
	OOFWAG_REC.WINS(5)    = 0
	OOFWAG_REC.WINS(6)    = 0
	OOFWAG_REC.JOKER_DIV  = 0

	RETURN
	END



C 	**************************************************
     	SUBROUTINE OPEN_OOFWAGANT (LUN, ST)
C	**************************************************
   	IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 ST
	INTEGER*4 LUN

	OOFWAGANT_REC.LUN = 0

	CALL CLEAR_OOFWAGANT_REC

	OOFWAGANT_REC.LUN = LUN

        OPEN (UNIT   =  OOFWAGANT_REC.LUN,
     *        FILE   = 'FILE:OOFWAGANT.ASC',
     *        STATUS = 'OLD', 
     *        IOSTAT =  ST)
	IF (ST.NE.0) THEN    
	   OOFWAG_REC.ERRSTR  = 'Error opening file OOFWAGANT.ASC'
	ENDIF

	RETURN
	END



C 	**************************************************
     	SUBROUTINE READ_OOFWAGANT (ST)
C	**************************************************
   	IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4     ST            ! (OUTPUT)

C	INTEGER*4     DRAW
        integer*4     GAMEON
	INTEGER*4     SZ
	CHARACTER*500 ASCREC
	CHARACTER*3   REFREC
        CHARACTER*50  AUXSTR

	ST = 0
	CALL CLEAR_OOFWAGANT_REC

  	READ (OOFWAGANT_REC.LUN, 100, END=200, ERR=300, IOSTAT=ST) ASCREC
100	FORMAT (A500)

        REFREC = ASCREC(1:3)

        IF (REFREC.EQ.'OA1' .OR. REFREC.EQ.'OA2' .OR. REFREC.EQ.'OA9') THEN

           OOFWAGANT_REC.RECNUM = OOFWAGANT_REC.RECNUM + 1

	   IF (REFREC.EQ.'OA1') THEN
	      IF (OOFWAGANT_REC.RECTYPE.NE.'  ') THEN
C	         * HEADER SHOULD BE THE FIRST RECORD ON FILE *
	         ST = -19
                 AUXSTR = ITOC(OOFWAGANT_REC.RECNUM,SZ)
                 OOFWAGANT_REC.ERRSTR = 'Header found in REC = ' // AUXSTR(1:SZ) 
	      ENDIF
              OOFWAGANT_REC.RECTYPE    = 'HD'
              OOFWAGANT_REC.YEAR       = CTOI(ASCREC(4:7), SZ) 
              OOFWAGANT_REC.MONTH      = CTOI(ASCREC(8:9), SZ) 
              OOFWAGANT_REC.DAY        = CTOI(ASCREC(10:11), SZ) 
              OOFWAGANT_REC.DATAHD     = ASCREC(10:11) // '/' // ASCREC(8:9) // '/' // ASCREC(4:7)

              IF (OOFWAGANT_REC.YEAR.LE.0)                              ST = -11    
              IF (OOFWAGANT_REC.MONTH.LE.0 .OR. OOFWAGANT_REC.MONTH.GT.12) ST = -11
              IF (OOFWAGANT_REC.DAY.LE.0 .OR. OOFWAGANT_REC.DAY.GT.31)     ST = -11
	      IF (ST.EQ.-11) THEN
	         OOFWAGANT_REC.ERRSTR = 'Invalid date on Header : ' // OOFWAGANT_REC.DATAHD
              ENDIF
	   ENDIF

           IF (REFREC.EQ.'OA2') THEN

	      IF (OOFWAGANT_REC.RECTYPE.EQ.'  ') THEN
C	         * DETAIL IS THE FIRST RECORD IN FILE *
	         ST = -20
                 OOFWAGANT_REC.ERRSTR = 'Header not found'
	      ENDIF
	      IF (OOFWAGANT_REC.RECTYPE.EQ.'TL') THEN
C	         * TRAILLER SHOUL BE THE LAST RECORD *
	         ST = -21
                 OOFWAGANT_REC.ERRSTR = 'There are other records after the trailler'
	      ENDIF
              OOFWAGANT_REC.RECTYPE    = 'DT'               

	      OOFWAGANT_REC.OFF_GAME   = CTOI(ASCREC(4:4), SZ) 
              IF (OOFWAGANT_REC.OFF_GAME.GE.9) OOFWAGANT_REC.OFF_GAME = OOFWAGANT_REC.OFF_GAME + 1   !THERE IS A SHIFT HERE,
										                     !BECAUSE OF JOKER
	      OOFWAGANT_REC.D_WEEK     = CTOI(ASCREC(5:7), SZ)
	      OOFWAGANT_REC.D_YEAR     = CTOI(ASCREC(14:17), SZ)  
	      OOFWAGANT_REC.NUM_OP     = CTOI(ASCREC(8:13), SZ)  
              OOFWAGANT_REC.AGENT      = CTOI(ASCREC(18:24), SZ) 
	      OOFWAGANT_REC.BILHETE    = CTOI(ASCREC(25:31), SZ) 

              IF (ASCREC(32:32).EQ.'1') THEN
	         OOFWAGANT_REC.RECLAMACAO = .TRUE.
      	      ELSE
	         OOFWAGANT_REC.RECLAMACAO = .FALSE.
	      ENDIF

	      OOFWAGANT_REC.BANCO      = CTOI(ASCREC(33:36), SZ) 
	      OOFWAGANT_REC.BALCAO     = CTOI(ASCREC(37:40), SZ) 

	      OOFWAGANT_REC.DATA_PAGAMENTO  = ASCREC(41:48) 

	      OOFWAGANT_REC.GAME_VALUE  = CTOI(ASCREC(49:57),SZ)
	      OOFWAGANT_REC.LOTO2_VALUE = CTOI(ASCREC(58:66),SZ)
	      OOFWAGANT_REC.JOKER_VALUE = CTOI(ASCREC(67:75),SZ)

	      OOFWAGANT_REC.JOKER_DIV  = CTOI(ASCREC(76:76), SZ) 

	      OOFWAGANT_REC.WINS_GAME(1) = CTOI(ASCREC(77:80), SZ) 
	      OOFWAGANT_REC.WINS_GAME(2) = CTOI(ASCREC(81:84), SZ) 
	      OOFWAGANT_REC.WINS_GAME(3) = CTOI(ASCREC(85:88), SZ) 
	      OOFWAGANT_REC.WINS_GAME(4) = CTOI(ASCREC(89:92), SZ) 
	      OOFWAGANT_REC.WINS_GAME(5) = CTOI(ASCREC(93:96), SZ) 
	      OOFWAGANT_REC.WINS_GAME(6) = CTOI(ASCREC(97:100), SZ) 

	      OOFWAGANT_REC.WINS_LOTO2(1) = CTOI(ASCREC(91:104), SZ) 
	      OOFWAGANT_REC.WINS_LOTO2(2) = CTOI(ASCREC(105:108), SZ) 
	      OOFWAGANT_REC.WINS_LOTO2(3) = CTOI(ASCREC(109:112), SZ) 
	      OOFWAGANT_REC.WINS_LOTO2(4) = CTOI(ASCREC(113:116), SZ) 
	      OOFWAGANT_REC.WINS_LOTO2(5) = CTOI(ASCREC(117:120), SZ) 

              IF (OOFWAGANT_REC.OFF_GAME.LE.0) THEN
                 ST = -12
                 AUXSTR = ITOC(OOFWAGANT_REC.OFF_GAME,SZ)
	         OOFWAGANT_REC.ERRSTR = 'Error on Offline game number : ' // AUXSTR(1:SZ)
	      ENDIF
              IF (OOFWAGANT_REC.D_WEEK.LE.0 .OR. OOFWAGANT_REC.D_WEEK.GT.106) ST = -13
              IF (OOFWAGANT_REC.D_YEAR.LE.0)                              ST = -13
              IF (ST.EQ.-13) THEN
                 AUXSTR = ITOC(OOFWAGANT_REC.D_WEEK*1000+OOFWAGANT_REC.D_YEAR,SZ)
	         OOFWAGANT_REC.ERRSTR = 'Invalid Week/year value' // AUXSTR(1:SZ)
              ENDIF                
              IF (OOFWAGANT_REC.AGENT.LE.0) THEN
                 ST = -14
                 AUXSTR = ITOC(OOFWAGANT_REC.AGENT,SZ)
 	         OOFWAGANT_REC.ERRSTR = 'Invalid agent number : ' // AUXSTR(1:SZ)
              ENDIF
              IF (OOFWAGANT_REC.BILHETE.LE.0) THEN
                 ST = -15
                 AUXSTR = ITOC(OOFWAGANT_REC.BILHETE,SZ)
	         OOFWAGANT_REC.ERRSTR = 'Invalid bilhete : ' // AUXSTR(1:SZ)
              ENDIF

	      GAMEON = OOFWAGANT_REC.OFF_GAME
              IF (GAMEON.GT.MAXGAM .OR. GAMEON.LE.0) THEN
      	         ST = -16
                 AUXSTR = ITOC(OOFWAGANT_REC.OFF_GAME,SZ)
	         OOFWAGANT_REC.ERRSTR = 'No Online game to Offline game : ' // AUXSTR(1:SZ) 
              ELSE
	         OOFWAGANT_REC.ON_GAME    = GAMEON
                 IF (OOFWAGANT_REC.NUM_OP.GT.OOFWAGANT_REC.LAST_OP_NUM(GAMEON)) THEN
                    OOFWAGANT_REC.LAST_OP_NUM(GAMEON) = OOFWAGANT_REC.NUM_OP
                 ENDIF
              ENDIF

C	      IF (ST.NE.-13 .AND. ST.NE.-16) THEN
C                 DRAW = GETDRW (OOFWAGANT_REC.D_YEAR, OOFWAGANT_REC.D_WEEK, GAMEON)
C		 IF (DRAW.LE.0) THEN
C                    ST = -17
C                    AUXSTR = ITOC(OOFWAGANT_REC.D_WEEK*10000+OOFWAGANT_REC.D_YEAR,SZ)
C	            OOFWAGANT_REC.ERRSTR = 'No equivalent draw to week/year :' // AUXSTR(1:SZ)
C                 ENDIF
C	      ENDIF

	   ENDIF

	   IF (REFREC.EQ.'OA9') THEN
              OOFWAGANT_REC.RECTYPE= 'TL'               
	      OOFWAGANT_REC.NUM_RECORDS = CTOI(ASCREC(4:8), SZ) 
              IF (OOFWAGANT_REC.NUM_RECORDS.NE.OOFWAGANT_REC.RECNUM) THEN
                 ST = -22
		 OOFWAGANT_REC.ERRSTR = 'Number of records declared differs from those found'
              ENDIF
           ENDIF           

        ELSE
           ST = -10   ! INVALID RECORD
	   OOFWAGANT_REC.ERRSTR = 'Invalid record type : ' // REFREC
        ENDIF

        RETURN       !NORMAL RETURN
	
200	CONTINUE     !END-OF-FILE
        IF (OOFWAGANT_REC.RECTYPE.NE.'TL') THEN
           ST = -144
           OOFWAGANT_REC.ERRSTR = 'End of file and Trailler Record not found'
        ELSE
           ST = 144
        ENDIF
        RETURN

300     CONTINUE     !READ ERROR
	OOFWAGANT_REC.ERRSTR = 'Error reading OOFWAGANT.ASC'
        RETURN

	END


C	*********************************************************
	SUBROUTINE CLEAR_OOFWAGANT_REC
C	*********************************************************
	IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4 GAM

	IF (OOFWAGANT_REC.LUN.EQ.0) THEN     !FIRST TIME (OPEN)
	   OOFWAGANT_REC.RECTYPE    = '  '

	   OOFWAGANT_REC.RECNUM     = 0

	   OOFWAGANT_REC.DAY        = 0
	   OOFWAGANT_REC.MONTH      = 0
	   OOFWAGANT_REC.YEAR       = 0
	   OOFWAGANT_REC.DATAHD     = '  /  /    '

	   OOFWAGANT_REC.NUM_RECORDS= 0

           DO GAM = 1,MAXGAM
	      OOFWAGANT_REC.LAST_OP_NUM(GAM) = 0
           ENDDO
	ENDIF

	OOFWAGANT_REC.ERRSTR     = '  '

	OOFWAGANT_REC.OFF_GAME   = 0
        OOFWAGANT_REC.ON_GAME    = 0
	OOFWAGANT_REC.D_WEEK     = 0
	OOFWAGANT_REC.D_YEAR     = 0
	OOFWAGANT_REC.NUM_OP     = 0
        OOFWAGANT_REC.AGENT      = 0
	OOFWAGANT_REC.BILHETE    = 0

	OOFWAGANT_REC.RECLAMACAO = .FALSE.
	OOFWAGANT_REC.BANCO      = 0
	OOFWAGANT_REC.BALCAO     = 0
	OOFWAGANT_REC.DATA_PAGAMENTO  = '        '
	OOFWAGANT_REC.GAME_VALUE      = 0
	OOFWAGANT_REC.LOTO2_VALUE     = 0
	OOFWAGANT_REC.JOKER_VALUE     = 0

	OOFWAGANT_REC.WINS_GAME(1)    = 0
	OOFWAGANT_REC.WINS_GAME(2)    = 0
	OOFWAGANT_REC.WINS_GAME(3)    = 0
	OOFWAGANT_REC.WINS_GAME(4)    = 0
	OOFWAGANT_REC.WINS_GAME(5)    = 0
	OOFWAGANT_REC.WINS_LOTO2(1)   = 0
	OOFWAGANT_REC.WINS_LOTO2(2)   = 0
	OOFWAGANT_REC.WINS_LOTO2(3)   = 0
	OOFWAGANT_REC.WINS_LOTO2(4)   = 0
	OOFWAGANT_REC.WINS_LOTO2(5)   = 0
	OOFWAGANT_REC.JOKER_DIV       = 0

	RETURN
	END


C
C       **************************************************
        SUBROUTINE OPEN_OFFWAG (LUN, ST)
C       **************************************************
        IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4 ST
        INTEGER*4 LUN

        OFFWAG_REC.LUN = 0

        CALL CLEAR_OFFWAG_REC

        OFFWAG_REC.LUN = LUN

        OPEN (UNIT   =  OFFWAG_REC.LUN,
     *        FILE   = 'FILE:OFFWAG.ASC',
     *        STATUS = 'OLD', 
     *        IOSTAT =  ST)
        IF (ST.NE.0) THEN    
           OFFWAG_REC.ERRSTR  = 'Error opening file OFFWAG.ASC'
        ENDIF

        RETURN
        END

C       **************************************************
        SUBROUTINE READ_OFFWAG (ST)
C       **************************************************
        IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:AGTCOM.DEF' 
        INCLUDE 'INCLIB:LTOCOM.DEF' 
        INCLUDE 'INCLIB:KIKCOM.DEF' 
        INCLUDE 'INCLIB:SPTCOM.DEF' 
        INCLUDE 'INCLIB:TGLCOM.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4     ST            ! (OUTPUT)

        INTEGER*4     DRAW
        INTEGER*4     GETDRW         !FUNCTION
        INTEGER*4     SZ,SZ2
        INTEGER*4     INDTAB,OFFSET,QTDE,IND,GTYP,GIND,BTYP
        INTEGER*4     CURWEEK,CURYEAR

        INTEGER*4      ONEDRAW
        PARAMETER      (ONEDRAW  = 1)
        INTEGER*4      FIVEDRAW
        PARAMETER      (FIVEDRAW = 2)

        INTEGER*4     SPTMAXMLT,LTOMAXMLT,KIKMAXMLT,LTOMAXBET
        PARAMETER     (SPTMAXMLT = 35)                    
        PARAMETER     (LTOMAXMLT = 17)
        PARAMETER     (KIKMAXMLT = 4)
        PARAMETER     (LTOMAXBET = 2) 

        INTEGER*4     SMULT(SPTMAXMLT),LMULT(LTOMAXMLT)  !NUMBER OF BETS, ACORDING WITH OFFWAG.ASC DETAIL
C       INTEGER*4     LMDRW(LTOMAXBET)                   !INDICATES SIMPLE BET OR MULTI DRAW
        INTEGER*4     KGAM(KIKMAXMLT)                    !GAMES WITH JOKER. THE ORDER OF ARRAY IS ACORDING TO OFFWAG.ASC LAYOUT

        LOGICAL       FOUND

        CHARACTER*500 ASCREC
        CHARACTER*3   REFREC
        CHARACTER*50  AUXSTR,AUXSTR2

        DATA LMULT/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,                              !SIMPLE BETS
     *              6, 21, 56, 126, 252, 462, 45/                               !SYSTEM BET


        DATA SMULT/ 2, 4, 6, 8, 10,                                                     !SIMPLE BETS
     *              2, 3, 4, 6, 8, 9, 12, 16, 18, 24, 27, 32, 36, 48, 54, 64, 72,       !SYSTEM BET
     *              81, 96, 108, 128, 144, 162, 192, 216, 243, 256, 288, 324, 384/      !SYSTEM BET

        DATA KGAM /1,1,           !TOTOBOLA
     *             3,3/           !TOTOBOLA EXTRA

        ST = 0
        CALL CLEAR_OFFWAG_REC

        READ (OFFWAG_REC.LUN, 100, END=200, ERR=300, IOSTAT=ST) ASCREC
100     FORMAT (A500)

        REFREC = ASCREC(1:3)

        IF (REFREC.EQ.'HW1' .OR. REFREC.EQ.'HW2' .OR. REFREC.EQ.'HW3' .OR. REFREC.EQ.'HW4' .OR. REFREC.EQ.'HW9') THEN

           OFFWAG_REC.RECNUM = OFFWAG_REC.RECNUM + 1

           IF (REFREC.EQ.'HW1') THEN
              IF (OFFWAG_REC.RECTYPE.NE.'   ') THEN
C                * HEADER SHOULD BE THE FIRST RECORD ON FILE *
                 ST = -19
                 AUXSTR = ITOC(OFFWAG_REC.RECNUM,SZ)
                 OFFWAG_REC.ERRSTR = 'Header found in REC = ' // AUXSTR(1:SZ)               
	      ENDIF
              OFFWAG_REC.RECTYPE    = 'HD '
              OFFWAG_REC.YEAR       = CTOI(ASCREC(4:7), SZ) 
              OFFWAG_REC.MONTH      = CTOI(ASCREC(8:9), SZ) 
              OFFWAG_REC.DAY        = CTOI(ASCREC(10:11), SZ) 
              OFFWAG_REC.DATAHD     = ASCREC(10:11) // '/' // ASCREC(8:9) // '/' // ASCREC(4:7)

              IF (OFFWAG_REC.YEAR.LE.0)                              ST = -11    
              IF (OFFWAG_REC.MONTH.LE.0 .OR. OFFWAG_REC.MONTH.GT.12) ST = -11
              IF (OFFWAG_REC.DAY.LE.0 .OR. OFFWAG_REC.DAY.GT.31)     ST = -11
              IF (ST.EQ.-11) THEN
                 OFFWAG_REC.ERRSTR = 'Invalid date on Header : ' // OFFWAG_REC.DATAHD
              ENDIF
           ENDIF

           IF (REFREC.EQ.'HW2' .OR. REFREC.EQ.'HW3' .OR. REFREC.EQ.'HW4') THEN

              IF (OFFWAG_REC.RECTYPE.EQ.'   ') THEN
C                * DETAIL IS THE FIRST RECORD IN FILE *
                 ST = -20
                 OFFWAG_REC.ERRSTR = 'Header not found'
              ENDIF
              IF (OFFWAG_REC.RECTYPE.EQ.'TL ') THEN
C                * TRAILLER SHOUL BE THE LAST RECORD *
                 ST = -21
                 OFFWAG_REC.ERRSTR = 'There are other records after the trailler'
              ENDIF
              OFFWAG_REC.RECTYPE    = 'DT '               
              OFFWAG_REC.ON_GAME    = CTOI(ASCREC(4:5), SZ) 
              OFFWAG_REC.D_WEEK     = CTOI(ASCREC(6:8), SZ)
              OFFWAG_REC.D_YEAR     = CTOI(ASCREC(9:12), SZ)  
              OFFWAG_REC.AGENT      = CTOI(ASCREC(13:19), SZ) 

              IF (OFFWAG_REC.ON_GAME.LE.0 .OR. OFFWAG_REC.ON_GAME.GT.MAXGAM) THEN
                 ST = -12
                 AUXSTR = ITOC(OFFWAG_REC.ON_GAME,SZ)
                 OFFWAG_REC.ERRSTR = 'Error on Offline game number : ' // AUXSTR(1:SZ)
              ENDIF
              IF (OFFWAG_REC.D_WEEK.LE.0 .OR. OFFWAG_REC.D_WEEK.GT.106) ST = -13
              IF (OFFWAG_REC.D_YEAR.LE.0)                              ST = -13
              IF (ST.EQ.-13) THEN
                 AUXSTR = ITOC(OFFWAG_REC.D_WEEK*1000+OFFWAG_REC.D_YEAR,SZ)
                 OFFWAG_REC.ERRSTR = 'Invalid Week/year value' // AUXSTR(1:SZ)
              ENDIF

              CALL FIGCCC(DAYCDC,CURWEEK,CURYEAR)
              IF (CURWEEK.NE.OFFWAG_REC.D_WEEK) THEN
                 ST = -30
                 AUXSTR = ITOC(CURWEEK,SZ)
                 OFFWAG_REC.ERRSTR = 'Invalid week: '// AUXSTR(1:SZ)  
              ENDIF

              IF (CURYEAR.NE.OFFWAG_REC.D_YEAR) THEN
                 ST = -31
                 AUXSTR = ITOC(CURYEAR,SZ)
                 OFFWAG_REC.ERRSTR = 'Invalid year: '// AUXSTR(1:SZ)
              ENDIF
                
              IF (OFFWAG_REC.AGENT.LE.0) THEN
                 ST = -14
                 AUXSTR = ITOC(OFFWAG_REC.AGENT,SZ)
                 OFFWAG_REC.ERRSTR = 'Invalid agent number : ' // AUXSTR(1:SZ)
              ENDIF

              IF (ST.NE.-14) THEN
                 FOUND  = .FALSE.
                 INDTAB = 1
                 DO  WHILE(INDTAB.LE.NUMAGT .AND. .NOT. FOUND)
                     IF  ( AGTTAB(AGTNUM,INDTAB) .EQ. OFFWAG_REC.AGENT ) THEN
                         FOUND = .TRUE.
                     ELSE
                         INDTAB = INDTAB + 1
                     ENDIF
                 ENDDO

                 IF (.NOT.FOUND) THEN
                    ST = -15
                    AUXSTR = ITOC(OFFWAG_REC.AGENT,SZ)
                    OFFWAG_REC.ERRSTR = 'Agent number not found in database: ' // AUXSTR(1:SZ)
                 ELSE
                    OFFWAG_REC.TERM = INDTAB
                 ENDIF
              ENDIF

              IF (ST.NE.-13 .AND. ST.NE.-12) THEN
                 DRAW = GETDRW (OFFWAG_REC.D_YEAR, OFFWAG_REC.D_WEEK, OFFWAG_REC.ON_GAME)
                 IF (DRAW.LE.0) THEN
                    ST = -17
                    AUXSTR = ITOC(OFFWAG_REC.D_WEEK*1000+OFFWAG_REC.D_YEAR,SZ)
                    OFFWAG_REC.ERRSTR = 'No equivalent draw to week/year :' // AUXSTR(1:SZ)
                 ENDIF

                 IF (DRAW.NE.DAYDRW(OFFWAG_REC.ON_GAME)) THEN
                    ST = -32
                    AUXSTR = ITOC(DRAW,SZ)
                    AUXSTR2 = ITOC(OFFWAG_REC.ON_GAME,SZ2) 
                    OFFWAG_REC.ERRSTR = AUXSTR(1:SZ) // ' is not the current DRAW for game ' // AUXSTR2(1:SZ2)
                 ENDIF
              ENDIF

              IF (ST.EQ.0) THEN
                 GTYP = GNTTAB(GAMTYP,OFFWAG_REC.ON_GAME)
                 IF (GTYP.LE.0.OR.GTYP.GT.MAXTYP) THEN
                    ST = -18
                    AUXSTR = ITOC(OFFWAG_REC.ON_GAME,SZ)
                    OFFWAG_REC.ERRSTR = 'Invalid game type for game number :' // AUXSTR(1:SZ)
                 ELSE
                    GIND = GNTTAB(GAMIDX,OFFWAG_REC.ON_GAME)
                    IF (GTYP.EQ.TLTO) THEN
                        OFFWAG_REC.B_PRICE = LTOPRC(GIND)
                        OFFSET = 20           !INITAL POSITION ON RECORD
                        DO BTYP=1,LTOMAXBET
                            DO IND=1,LTOMAXMLT
                               QTDE   = CTOI(ASCREC(OFFSET:OFFSET+4),SZ)
                               OFFSET = OFFSET + 5
                               IF (QTDE.LT.0) THEN
                                  ST = -20
                                  AUXSTR = ITOC(QTDE,SZ)
                                  OFFWAG_REC.ERRSTR = 'Invalid number of wagers :' // AUXSTR(1:SZ)
                               ELSE
                                  OFFWAG_REC.WAG_CNT(BTYP) = OFFWAG_REC.WAG_CNT(BTYP) + ( QTDE * LMULT(IND) )
                                  OFFWAG_REC.TKT_CNT(BTYP) = OFFWAG_REC.TKT_CNT(BTYP) + QTDE 
                               ENDIF
                            ENDDO
                        ENDDO

                    ELSEIF (GTYP.EQ.TSPT.OR.GTYP.EQ.TTGL) THEN
                        IF (GTYP.EQ.TSPT) THEN
                            OFFWAG_REC.B_PRICE = SPTPRC(GIND)
                        ELSE
                            OFFWAG_REC.B_PRICE = TGLPRC(GIND)
                        ENDIF

                        OFFSET = 20           !INITAL POSITION ON RECORD
                        DO IND=1,SPTMAXMLT
                            QTDE   = CTOI(ASCREC(OFFSET:OFFSET+4),SZ)
                            OFFSET = OFFSET + 5

                            IF (QTDE.LT.0) THEN
                               ST = -20
                               AUXSTR = ITOC(QTDE,SZ)
                               OFFWAG_REC.ERRSTR = 'Invalid number of wagers :' // AUXSTR(1:SZ)
                            ELSE
                               OFFWAG_REC.WAG_CNT(ONEDRAW) = OFFWAG_REC.WAG_CNT(ONEDRAW) + ( QTDE * SMULT(IND) )
                               OFFWAG_REC.TKT_CNT(ONEDRAW) = OFFWAG_REC.TKT_CNT(ONEDRAW) + QTDE 
                            ENDIF
                        ENDDO
                    ELSEIF (GTYP.EQ.TKIK) THEN
                        OFFWAG_REC.B_PRICE = KIKPRC(GIND)

                        OFFSET = 20                        !INITAL POSITION ON RECORD
                        DO IND=1,KIKMAXMLT               
                            QTDE   = CTOI(ASCREC(OFFSET:OFFSET+4),SZ)
                            OFFSET = OFFSET + 5

                            IF (QTDE.LT.0) THEN
                               ST = -20
                               AUXSTR = ITOC(QTDE,SZ)
                               OFFWAG_REC.ERRSTR = 'Invalid number of wagers :' // AUXSTR(1:SZ)
                            ELSE
                               OFFWAG_REC.KIK_WAG_CNT(KGAM(IND),ONEDRAW) = OFFWAG_REC.KIK_WAG_CNT(KGAM(IND),ONEDRAW) + QTDE 
                               OFFWAG_REC.WAG_CNT(ONEDRAW)  = OFFWAG_REC.WAG_CNT(ONEDRAW) + QTDE 
                               OFFWAG_REC.TKT_CNT(ONEDRAW)  = OFFWAG_REC.WAG_CNT(ONEDRAW)  
                            ENDIF
                        ENDDO
C
C GET MULTI DRAW WAGERS (TOTOLOTO)
C
                        OFFSET = 70                        !INITAL POSITION ON RECORD
                        DO IND=1,2                         !GET LAST 2 VALUES 
                            QTDE   = CTOI(ASCREC(OFFSET:OFFSET+4),SZ)
                            OFFSET = OFFSET + 5

                            IF (QTDE.LT.0) THEN
                               ST = -20
                               AUXSTR = ITOC(QTDE,SZ)
                               OFFWAG_REC.ERRSTR = 'Invalid number of wagers :' // AUXSTR(1:SZ)
                            ELSE
                               OFFWAG_REC.KIK_WAG_CNT(KGAM(IND),FIVEDRAW) = OFFWAG_REC.KIK_WAG_CNT(KGAM(IND),FIVEDRAW) + QTDE 
                               OFFWAG_REC.WAG_CNT(FIVEDRAW)  = OFFWAG_REC.WAG_CNT(FIVEDRAW) + QTDE 
                               OFFWAG_REC.TKT_CNT(FIVEDRAW)  = OFFWAG_REC.WAG_CNT(FIVEDRAW) 
                            ENDIF
                        ENDDO

                    ELSE
                        ST = -19
                        AUXSTR = ITOC(GTYP,SZ)
                        OFFWAG_REC.ERRSTR = 'Invalid game type :' // AUXSTR(1:SZ)
                    ENDIF
                    
                 ENDIF

              ENDIF

           ENDIF

           IF (REFREC.EQ.'HW9') THEN
              OFFWAG_REC.RECTYPE= 'TL '                
              OFFWAG_REC.NUM_RECORDS= CTOI(ASCREC(4:11), SZ) 
              IF (OFFWAG_REC.NUM_RECORDS.NE.OFFWAG_REC.RECNUM-2) THEN
                 ST = -22
                 OFFWAG_REC.ERRSTR = 'Number of records declared <> found'
              ENDIF
           ENDIF           

        ELSE
           ST = -10   ! INVALID RECORD
           OFFWAG_REC.ERRSTR = 'Invalid record type : ' // REFREC
        ENDIF

        RETURN       !NORMAL RETURN
        
200     CONTINUE     !END-OF-FILE
        IF (OFFWAG_REC.RECTYPE.NE.'TL ') THEN
           ST = -144
           OFFWAG_REC.ERRSTR = 'End of file and Trailler Record not found'
        ELSE
           ST = 144
        ENDIF
        RETURN

300     CONTINUE     !READ ERROR
        OFFWAG_REC.ERRSTR = 'Error reading OFFWAG.ASC'
        RETURN

        END


C       *********************************************************
        SUBROUTINE CLEAR_OFFWAG_REC
C       *********************************************************
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4 GAM,I

        IF (OFFWAG_REC.LUN.EQ.0) THEN     !FIRST TIME (OPEN)
           OFFWAG_REC.RECTYPE    = '   '

           OFFWAG_REC.RECNUM     = 0

           OFFWAG_REC.DAY        = 0
           OFFWAG_REC.MONTH      = 0
           OFFWAG_REC.YEAR       = 0
           OFFWAG_REC.DATAHD     = '  /  /    '

           OFFWAG_REC.NUM_RECORDS= 0
        ENDIF

        OFFWAG_REC.ERRSTR     = '  '

        OFFWAG_REC.ON_GAME    = 0
        OFFWAG_REC.B_PRICE    = 0
        OFFWAG_REC.D_WEEK     = 0
        OFFWAG_REC.D_YEAR     = 0
        OFFWAG_REC.AGENT      = 0
        OFFWAG_REC.TERM       = 0
        DO I=1,2
           OFFWAG_REC.WAG_CNT(I)    = 0
           OFFWAG_REC.TKT_CNT(I)    = 0
        ENDDO
        DO GAM=1,MAXGAM
           DO I=1,2 
              OFFWAG_REC.KIK_WAG_CNT(GAM,I) = 0
           ENDDO
        ENDDO

        RETURN
        END

C       **************************************************
        SUBROUTINE OPEN_OFFWAGFIN (LUN, ST)
C       **************************************************
        IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4 ST
        INTEGER*4 LUN

        OFFWAGFIN_REC.LUN = 0

        CALL CLEAR_OFFWAGFIN_REC

        OFFWAGFIN_REC.LUN = LUN

        OPEN (UNIT   =  OFFWAGFIN_REC.LUN,
     *        FILE   = 'FILE:OFFWAGFIN.ASC',
     *        STATUS = 'OLD', 
     *        IOSTAT =  ST)
        IF (ST.NE.0) THEN    
           OFFWAGFIN_REC.ERRSTR  = 'Error opening file OFFWAGFIN.ASC'
        ENDIF

        RETURN
        END

C       **************************************************
        SUBROUTINE READ_OFFWAGFIN (ST)
C       **************************************************
        IMPLICIT NONE                                                  
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:AGTCOM.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        INTEGER*4     ST,SZ,I
        CHARACTER*3   REFREC
        CHARACTER*500 ASCREC
        CHARACTER*50  AUXSTR

        ST = 0
        CALL CLEAR_OFFWAGFIN_REC

        READ (OFFWAGFIN_REC.LUN, 100, END=200, ERR=300, IOSTAT=ST) ASCREC
100     FORMAT (A500)

        OFFWAGFIN_REC.RECNUM = OFFWAGFIN_REC.RECNUM + 1
        REFREC = ASCREC(1:3)

        IF (REFREC.EQ.'HV1') THEN
            IF (OFFWAGFIN_REC.RECTYPE.NE.'   ') THEN
C                * HEADER SHOULD BE THE FIRST RECORD ON FILE *
                 ST = -19
                 AUXSTR = ITOC(OFFWAGFIN_REC.RECNUM,SZ)
                 OFFWAGFIN_REC.ERRSTR = 'Header found in REC = ' // AUXSTR(1:SZ) 
            ENDIF
            OFFWAGFIN_REC.RECTYPE    = 'HD '
            OFFWAGFIN_REC.YEAR       = CTOI(ASCREC(4:7), SZ) 
            OFFWAGFIN_REC.MONTH      = CTOI(ASCREC(8:9), SZ) 
            OFFWAGFIN_REC.DAY        = CTOI(ASCREC(10:11), SZ) 
            OFFWAGFIN_REC.DATAHD     = ASCREC(10:11) // '/' // ASCREC(8:9) // '/' // ASCREC(4:7)

            IF (OFFWAGFIN_REC.YEAR.LE.0)                              ST = -11    
            IF (OFFWAGFIN_REC.MONTH.LE.0 .OR. OFFWAGFIN_REC.MONTH.GT.12) ST = -11
            IF (OFFWAGFIN_REC.DAY.LE.0 .OR. OFFWAGFIN_REC.DAY.GT.31)     ST = -11
            IF (ST.EQ.-11) THEN
                OFFWAGFIN_REC.ERRSTR = 'Invalid date on Header : ' // OFFWAGFIN_REC.DATAHD
            ENDIF
        ELSEIF (REFREC.EQ.'HV2') THEN
            IF (OFFWAGFIN_REC.RECTYPE.EQ.'   ') THEN
C                * DETAIL IS THE FIRST RECORD IN FILE *
                ST = -20
                OFFWAGFIN_REC.ERRSTR = 'Header not found'
            ENDIF
            IF (OFFWAGFIN_REC.RECTYPE.EQ.'TL ') THEN
C                * TRAILLER SHOUL BE THE LAST RECORD *
                ST = -21
                OFFWAGFIN_REC.ERRSTR = 'There are other records after the trailler'
            ENDIF

            OFFWAGFIN_REC.RECTYPE    = 'DT '
            OFFWAGFIN_REC.D_WEEK = CTOI(ASCREC(4:6), SZ)
            OFFWAGFIN_REC.D_YEAR = CTOI(ASCREC(7:10), SZ)
            IF (OFFWAGFIN_REC.D_WEEK.LE.0 .OR. OFFWAGFIN_REC.D_WEEK.GT.106) ST = -13
            IF (OFFWAGFIN_REC.D_YEAR.LE.0)                               ST = -13
            IF (ST.EQ.-13) THEN
                AUXSTR = ITOC(OFFWAGFIN_REC.D_WEEK*1000+OFFWAGFIN_REC.D_YEAR,SZ)
                OFFWAGFIN_REC.ERRSTR = 'Invalid Week/year value' // AUXSTR(1:SZ)
            ENDIF

            OFFWAGFIN_REC.AGENT = CTOI(ASCREC(11:17), SZ)
            OFFWAGFIN_REC.TERM = 0
            DO I=1,NUMAGT
                IF (AGTTAB(AGTNUM,I).EQ.OFFWAGFIN_REC.AGENT) THEN
                    OFFWAGFIN_REC.TERM = I
                    EXIT
                ENDIF
            ENDDO
            IF (OFFWAGFIN_REC.TERM.EQ.0) THEN
                ST = -14
                AUXSTR = ITOC(OFFWAGFIN_REC.AGENT, SZ)
                OFFWAGFIN_REC.ERRSTR = 'Invalid Agent number ' // AUXSTR(1:SZ)
            ENDIF

            OFFWAGFIN_REC.PRMPAG    = CTOI(ASCREC(18:26), SZ)
            OFFWAGFIN_REC.TRANSP    = CTOI(ASCREC(27:35), SZ)
            OFFWAGFIN_REC.CENREC    = CTOI(ASCREC(36:44), SZ)
            OFFWAGFIN_REC.TIPCENREC = CTOI(ASCREC(45:45), SZ)
        ELSEIF (REFREC.EQ.'HV9') THEN
            OFFWAGFIN_REC.RECTYPE= 'TL '                
            OFFWAGFIN_REC.NUM_RECORDS= CTOI(ASCREC(4:11), SZ) 
            IF (OFFWAGFIN_REC.NUM_RECORDS.NE.OFFWAGFIN_REC.RECNUM-2) THEN
                ST = -22
                OFFWAGFIN_REC.ERRSTR = 'Number of records declared <> found'
            ENDIF
        ELSE
            ST = -10   ! INVALID RECORD
            OFFWAGFIN_REC.ERRSTR = 'Invalid record type : ' // REFREC
        ENDIF

        RETURN       !NORMAL RETURN
        
200     CONTINUE     !END-OF-FILE
        IF (OFFWAGFIN_REC.RECTYPE.NE.'TL ') THEN
           ST = -144
           OFFWAGFIN_REC.ERRSTR = 'End of file and Trailler Record not found'
        ELSE
           ST = 144
        ENDIF
        RETURN

300     CONTINUE     !READ ERROR
        OFFWAGFIN_REC.ERRSTR = 'Error reading OFFWAGFIN.ASC'
        RETURN

        END


C       *********************************************************
        SUBROUTINE CLEAR_OFFWAGFIN_REC
C       *********************************************************
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

        IF (OFFWAGFIN_REC.LUN.EQ.0) THEN     !FIRST TIME (OPEN)
           OFFWAGFIN_REC.RECTYPE     = '   '

           OFFWAGFIN_REC.RECNUM      = 0

           OFFWAGFIN_REC.DAY         = 0
           OFFWAGFIN_REC.MONTH       = 0
           OFFWAGFIN_REC.YEAR        = 0
           OFFWAGFIN_REC.DATAHD      = '  /  /    '

           OFFWAGFIN_REC.NUM_RECORDS = 0
        ENDIF

        OFFWAGFIN_REC.ERRSTR = '  '

        OFFWAGFIN_REC.D_WEEK   = 0
        OFFWAGFIN_REC.D_YEAR   = 0
        OFFWAGFIN_REC.AGENT    = 0
        OFFWAGFIN_REC.TERM     = 0

        OFFWAGFIN_REC.PRMPAG = 0
        OFFWAGFIN_REC.TRANSP = 0
        OFFWAGFIN_REC.CENREC = 0

        RETURN
        END
