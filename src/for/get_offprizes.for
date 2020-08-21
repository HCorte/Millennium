C
C GET_OFFPRIZES.FOR                                                                    
C
C
C V02 27-DEC-2010 FRP Lotto2 Changes
C V01 02-JAN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C READ BIG PRIZES SENT BY OFFLINE SYSTEM AND CREATE 
C THE CORRESPONDING PAYMENT ORDERS ON ORDERS FILE                                                                               
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM GET_OFFPRIZES      
        IMPLICIT NONE                                                  
	                                                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
        INTEGER*4   OOFWAG_LUN /TMP1_LUN/
        INTEGER*4   MSG_LUN /6/                                                   
	INTEGER*4   GAM_LUN 

        LOGICAL     ERROR_FOUND
        INTEGER*4   ST, K
        INTEGER*4   GAME_STATUS
	INTEGER*4   JOKER_NUMBER /5/
        INTEGER*4   ORDER_NUMBER(MAXGAM)
        INTEGER*4   GAM, GTYPE, DIV, GIND
        INTEGER*4   GAME_PRIZE, JOKER_PRIZE, DRAW, CLOSE_DAY, GAMEON
        INTEGER*4   DRAWFORGAME(MAXGAM)
	INTEGER*4   WEEK, YEAR
	INTEGER*4   OPS_AMT(MAXGAM), OPS_CNT, OPS_CNT_HI
	INTEGER*4   TOTAL

        INTEGER*4   GETDRW   !FUNCTION
	INTEGER*4   YESNO

        STRUCTURE /STRU_FDB/
           INTEGER*4 FDB(7)
        ENDSTRUCTURE
        RECORD /STRU_FDB/ FDB_REC(MAXGAM)

        CALL COPYRITE                                                             
  	                                                                         
        TYPE*,IAM(),' '
        TYPE *,'------------------------------------------------------------------------'   
        TYPE *,'<<<<< CRIACAO DAS ORDENS DE PAGAMENTO A PARTIR DOS PREMIOS OFFLINE >>>>>'   
        TYPE *,'------------------------------------------------------------------------'
        TYPE *,'      CERTIFIQUE-SE DE QUE OS FICHEROS  < OPS.FIL >  E  < SCF.FIL >     ' 
        TYPE *,'      TENHAM COPIAS DE SEGURANCA ANTES DE REALIZAR ESTE PROCESSAMENTO   '
        TYPE *,'------------------------------------------------------------------------'
        TYPE *,'      EM CASO DE ERRO, RESTAURE AMBOS OS ARQUIVOS MENCIONADOS ACIMA     ' 
        TYPE *,'      (EXCETO ERROS DURANTE A CONSISTENCIA DO ARQUIVO),                 '
        TYPE *,'      CORRIJA O ERRO, E EXECUTE NOVAMENTE O PROCESSO                    '
        TYPE *,'------------------------------------------------------------------------'
        TYPE*,IAM(),' '

C                                                                               
C       GET AND OPEN INPUT FILE
C       =======================
        CALL OPEN_OOFWAG (OOFWAG_LUN,ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening OOFWAG.ASC file.  STATUS = ', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
    
C
C       VALIDATE INTEGRITY OF INPUT FILE 
C       ================================
        CALL CHECK_INPUT_FILE (ST)
        IF (ST.NE.0) THEN
  	   CALL DISPERR (MSG_LUN, 'There where errors Consisting OOFWAG.ASC', 0, ' ', 0, ' ', 0)
	   CALL PRMYESNO('Continua mesmo assim (Y/N) ? ', YESNO)
	   IF (YESNO.NE.1) THEN
              CALL GSTOP (GEXIT_FATAL)
	   ENDIF
        ENDIF
C
C        SAVE DRAW TABLE FILLED DURING CHECK_INPUT_FILE
C	
      	DO GAM=1,MAXGAM
           DRAWFORGAME(GAM) = OOFWAG_REC.DRAW(GAM)
      	ENDDO
C
C     	CLOSE AND GO TO BEGINNING OF INPUT FILE AGAIN
C       =============================================
      	CLOSE (OOFWAG_LUN)

      	CALL OPEN_OOFWAG (OOFWAG_LUN, ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, '>> OOFWAG.ASC Open error,  status =', ST, ' ', 0, ' ', 0)
           CALL GSTOP(GEXIT_FATAL)
      	ENDIF

	CALL FASTSET(0,OPS_AMT,MAXGAM)
	OPS_CNT    = 0
	OPS_CNT_HI = 0
	TOTAL   = 0
C
C     	OPEN ALL DRAW FILES FOR MUTUAS GAMES
C     	====================================
      	DO 707 GAM=1,MAXGAM

      	   GTYPE = GNTTAB(GAMTYP,GAM)   
      	   GIND  = GNTTAB(GAMIDX, GAM)

	   IF (GTYPE.EQ.0 .OR. GIND.EQ.0) GOTO 707

           IF (GTYPE.EQ.TLTO .OR. GTYPE.EQ.TSPT .OR. GTYPE.EQ.TTGL .OR. GTYPE.EQ.TKIK) THEN            

	      GAM_LUN = TMP2_LUN + GAM

              CALL OPENW(GAM_LUN,GFNAMES(1,GAM),4,0,0,ST)
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error opening game file for game : ', GAM, ' ', 0, ' ', 0)
                 CALL GSTOP (GEXIT_FATAL)
              ENDIF

              IF (GTYPE.EQ.TLTO) THEN             !LOTTO
                 CALL IOINIT(FDB_REC(GAM).FDB,GAM_LUN,DLTSEC*256)
              ENDIF
              IF (GTYPE.EQ.TSPT) THEN             !BOLA
	         CALL IOINIT(FDB_REC(GAM).FDB,GAM_LUN,DSPSEC*256)
              ENDIF
              IF (GTYPE.EQ.TTGL) THEN             !GOLO
	         CALL IOINIT(FDB_REC(GAM).FDB,GAM_LUN,DTGSEC*256)
              ENDIF
              IF (GTYPE.EQ.TKIK) THEN             !JOKER
	         CALL IOINIT(FDB_REC(GAM).FDB,GAM_LUN,DKKSEC*256)
	      ENDIF

           ENDIF

707     CONTINUE

C
C     	GET LAST ORDER NUMBER FROM SCF
C     	==============================         
      	CALL GET_ORDER_NUMBER (ORDER_NUMBER, ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error getting Order Number from Configuration File', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF

C
C     	OPEN ORDENS DE PAGAMENTO FILE
C     	=============================
     	CALL OPEN_OPS('KEYED',ST)
      	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
      	   CALL GSTOP (GEXIT_FATAL)
      	ENDIF             

C
C     	READ FIRST RECORD HERE TO PROCESS THE HEADER
C     	============================================
      	CALL READ_OOFWAG (ST)
        IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, OOFWAG_REC.ERRSTR, 0, ' ', 0, ' ', 0)
           CALL GSTOP(GEXIT_FATAL)
        ENDIF


	ERROR_FOUND = .FALSE.
C
C       LOOP READING INPUT FILE
C       =======================
C   
        DO 300 WHILE (ABS(ST).NE.144)
                                                                  
           CALL READ_OOFWAG (ST)
           IF (ABS(ST).EQ.144.OR.OOFWAG_REC.RECTYPE.EQ.'TL') GOTO 300 
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, OOFWAG_REC.ERRSTR, 0, ' ', 0, ' ', 0)
	      ERROR_FOUND = .TRUE.
              CALL GSTOP(GEXIT_FATAL)
           ENDIF
C
C	   IF NO PRIZE, JUMP IT
C
           IF (OOFWAG_REC.WINS(1)+OOFWAG_REC.WINS(2)+OOFWAG_REC.WINS(3)+OOFWAG_REC.WINS(4)+OOFWAG_REC.WINS(5)+
     *         OOFWAG_REC.WINS(6)+OOFWAG_REC.JOKER_DIV .EQ. 0) GOTO 300

C
C          FILL OP RECORD WITH DATA READ FROM INPUT FILE
C          ==============================================
C
           OPS_REC.ONLINE_ORDER = 'N'
           GAMEON = OOFWAG_REC.ON_GAME
C
C	   GIVE A NUMBER TO THE OP (BY GAME)
C          ---------------------------------
           ORDER_NUMBER(GAMEON) = ORDER_NUMBER(GAMEON) + 1
           IF (ORDER_NUMBER(GAMEON).GT.999999) THEN  !SHOULD NOT BE GREATER THAN 6 DIGITS
              ORDER_NUMBER(GAMEON) = 1
           ENDIF
           WRITE (OPS_REC.ORDER, FMT='(I6.6)') ORDER_NUMBER(GAMEON)

           WRITE (OPS_REC.GAME,FMT='(I2.2)')   GAMEON

           YEAR = OOFWAG_REC.D_YEAR
           WEEK = OOFWAG_REC.D_WEEK
           WRITE (OPS_REC.YEARWEEK,FMT='(I4.4,I3.3)')  YEAR, WEEK

           WRITE (OPS_REC.AGENT,FMT='(I7.7)') OOFWAG_REC.AGENT   

           DO DIV=1,6
              OPS_REC.WINS(DIV) = OOFWAG_REC.WINS(DIV)
           ENDDO      
	
           OPS_REC.JOKER_DIV = OOFWAG_REC.JOKER_DIV
           WRITE (OPS_REC.BILHETE,155)  OOFWAG_REC.BILHETE, '       '
155        FORMAT(I7.7,A7)
C
C     	   CALCULATE TOTAL PRIZE FOR GAME AND JOKER 
C          ----------------------------------------
           GTYPE = GNTTAB(GAMTYP, GAMEON)
           GIND  = GNTTAB(GAMIDX, GAMEON)
C
C	   PRIZE FOR THE GAME
C
           DRAW = DRAWFORGAME(GAMEON)
           IF (DRAW.LE.0) THEN
              CALL DISPERR(MSG_LUN, 'Draw for game : ', GAMEON, 'Is equals to ', DRAW, ' ', 0)
              ERROR_FOUND = .TRUE.
              GOTO 300
           ENDIF

      	   GAME_PRIZE  = 0
           OPS_REC.HI_PRIZE = .FALSE.

           IF     (GTYPE.EQ.TLTO) THEN             !LOTTO
              CALL READW (FDB_REC(GAMEON).FDB, DRAW, DLTREC, ST)
              CLOSE_DAY    = DLTESD
              GAME_STATUS = DLTSTS
              DO DIV=1,DLTDIV
                 GAME_PRIZE = GAME_PRIZE + OOFWAG_REC.WINS(DIV)*DLTSHV(DIV,1)
                 IF (OOFWAG_REC.WINS(DIV).GT.0 .AND. DLTSHV(DIV,1).GE.P(VALPRZHI)) THEN
                    OPS_REC.HI_PRIZE = .TRUE.
                 ENDIF         
              ENDDO
           ELSEIF (GTYPE.EQ.TSPT) THEN             !BOLA
              CALL READW (FDB_REC(GAMEON).FDB, DRAW, DSPREC, ST)
              CLOSE_DAY = DSPESD
              GAME_STATUS = DSPSTS
              DO DIV=1,DSPDIV
                 GAME_PRIZE = GAME_PRIZE + OOFWAG_REC.WINS(DIV)*DSPSHV(DIV)         
                 IF (OOFWAG_REC.WINS(DIV).GT.0 .AND. DSPSHV(DIV).GE.P(VALPRZHI)) THEN
                    OPS_REC.HI_PRIZE = .TRUE.
                 ENDIF         
              ENDDO
           ELSEIF (GTYPE.EQ.TTGL) THEN             !GOLO
              CALL READW (FDB_REC(GAMEON).FDB, DRAW, DTGREC, ST)
              CLOSE_DAY = DTGESD
              GAME_STATUS = DTGSTS
              DO DIV=1,DTGDIV
                 GAME_PRIZE = GAME_PRIZE + OOFWAG_REC.WINS(DIV)*DTGSHV(DIV)         
                 IF (OOFWAG_REC.WINS(DIV).GT.0 .AND. DTGSHV(DIV).GE.P(VALPRZHI)) THEN
                    OPS_REC.HI_PRIZE = .TRUE.
                 ENDIF         
              ENDDO
	   ELSE
	      CALL DISPERR (MSG_LUN, 'Invalid game type : ', GTYPE, ' ', 0, ' ', 0)
	      ERROR_FOUND = .TRUE.
              GOTO 300
           ENDIF
           IF (GTYPE.EQ.TLTO .OR. GTYPE.EQ.TSPT .OR. GTYPE.EQ.TTGL) THEN
              IF (ST.NE.0) THEN
                 CALL DISPERR(MSG_LUN, 'Draw : ', DRAW, 'Not found for game : ', GAMEON, 'On game file', 0)
		 ERROR_FOUND = .TRUE.
                 GOTO 300
              ENDIF	 
              IF (GAME_STATUS.LT.GAMDON) THEN
	         CALL DISPERR (MSG_LUN, 'Prizes not available for the week', 0, 'Draw : ', DRAW, 'Game : ', GAMEON)
                 ERROR_FOUND = .TRUE.
		 GOTO 300
              ENDIF
           ENDIF
C
C          PRIZE FOR JOKER
C
           JOKER_PRIZE = 0

           IF (OOFWAG_REC.JOKER_DIV.NE.0) THEN        !Won on Joker here
              DRAW = GETDRW (OOFWAG_REC.D_YEAR, OOFWAG_REC.D_WEEK, JOKER_NUMBER)   !Get Joker draw from the week / year
              IF (DRAW.LE.0) THEN
                 CALL DISPERR(MSG_LUN, 'Draw for Joker ', 0, 'Is equals to ', DRAW, ' ', 0)
                 ERROR_FOUND = .TRUE.
		 GOTO 300
              ENDIF
              DIV = OOFWAG_REC.JOKER_DIV
              CALL READW (FDB_REC(JOKER_NUMBER).FDB, DRAW, DKKREC, ST)
              IF (ST.NE.0) THEN
                 CALL DISPERR(MSG_LUN, 'Draw ', DRAW, 'Not found for JOKER ', 0, 'On game file', 0)
		 ERROR_FOUND = .TRUE.
                 GOTO 300
              ENDIF	 
              IF (DKKSTS.LT.GAMDON) THEN
	         CALL DISPERR (MSG_LUN, 'Prizes not available for the week', 0, 'Draw : ', DRAW, 'JOKER Game : ', 0)
		 ERROR_FOUND = .TRUE.
                 GOTO 300
              ENDIF
              JOKER_PRIZE = DKKSHV(DIV)         
              IF (DKKSHV(DIV).GE.P(VALPRZHI)) THEN
                 OPS_REC.HI_PRIZE = .TRUE.
              ENDIF         
	   ENDIF

           OPS_REC.PAYABLE_CDC = CLOSE_DAY + PRGDAY(GAMEON)

           OPS_REC.TOTAL_GAME  = GAME_PRIZE
           OPS_REC.TOTAL_JOKER = JOKER_PRIZE

	   OPS_CNT = OPS_CNT + 1
	   IF (OPS_REC.HI_PRIZE) THEN
	      OPS_CNT_HI = OPS_CNT_HI + 1
           ENDIF
	   TOTAL = TOTAL + JOKER_PRIZE + GAME_PRIZE
           IF (JOKER_PRIZE.GT.0) THEN
	      OPS_AMT(JOKER_NUMBER) = OPS_AMT(JOKER_NUMBER) + JOKER_PRIZE
           ENDIF
	   OPS_AMT(GAMEON) = OPS_AMT(GAMEON) + GAME_PRIZE

C
C          WRITE ORDER TO FILE
C          -------------------
           WRITE(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error writing to Order file : Agent    = ', OPS_REC.AGENT, 
     *                               '                              Game     = '// OPS_REC.GAME, 0, 
     *                               '                              YearWeek = ' // OPS_REC.YEARWEEK, 0)
              CALL GSTOP (GEXIT_FATAL)
           ENDIF

300     CONTINUE    !INPUT FILE READ LOOP

C
C	CLOSE FILES
C	-----------
        DO GAM=1,MAXGAM
           IF (FDB_REC(GAM).FDB(1).NE.0) THEN
              CALL CLOSEFIL(FDB_REC(GAM).FDB)
           ENDIF
        ENDDO

        CLOSE(OOFWAG_LUN)  
        CLOSE(OPS_LUN)

        TYPE*,' '
        TYPE*,'==========================================================================='
        TYPE*,'>>> ORDENS GERADAS : ', OPS_CNT
        TYPE*,'>>> ORDENS ALTAS   : ', OPS_CNT_HI
        TYPE*,'==========================================================================='
        DO GAM=1,MAXGAM
           IF (OPS_AMT(GAM).GT.0) THEN
	      WRITE(MSG_LUN,55) (GLNAMES(K,GAM),K=1,4), CMONY(OPS_AMT(GAM),11,VALUNIT)
55	      FORMAT(1X,4X,'JOGO = ', 4A4,/,
     *               1X,4X,'VALOR EM ORDENS = ', A11,/,
     *               1X,75('-'))
           ENDIF
	ENDDO
	WRITE(MSG_LUN,79) CMONY(TOTAL,11,VALUNIT)
79      FORMAT(      1X,'>>> TOTAL             ', A11)
        TYPE*,'==========================================================================='
        TYPE*,' '
C
C       UPDATE LAST ORDER SEQUENTIAL NUMBER INTO SCF
C       --------------------------------------------
        CALL UPD_ORDER_NUMBER (ORDER_NUMBER, ST)
        IF (ST.NE.0) THEN  
	   CALL DISPERR (MSG_LUN, 'Error updating Order Number in Configuration File', 0, ' ', 0, ' ', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF
C
C       THERE WERE ERRORS DURING FILE PROCESSING
C       -----------------------------------------
        IF (ERROR_FOUND) THEN  
	   CALL DISPERR (MSG_LUN, 'There were errors during file processing', 0, 
     *                            'CORRECT the errors, RESTORE files OPS.FIL and  ', 0, 
     *                            'SCF.FIL and PROCESS the file again.', 0)
           CALL GSTOP (GEXIT_FATAL)
        ENDIF

        CALL GSTOP(GEXIT_SUCCESS)                          
        END   



C	****************************************
	SUBROUTINE CHECK_INPUT_FILE (STATUS)
C	****************************************
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4   STATUS

	INTEGER*4   MSG_LUN /6/

   	INTEGER*4   ST
   	INTEGER*4   ANSWER
  	INTEGER*4   AGENT 
        INTEGER*4   GAM
     
        INTEGER*4   TERM

	STATUS  = 0
C
C       READ FIRST RECORD HERE TO PROCESS THE HEADER
C       ********************************************
        CALL READ_OOFWAG (ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, OOFWAG_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
           STATUS = 1
           RETURN
        ENDIF

        TYPE*,IAM(),' '
        TYPE*,IAM(),'>> Date in Header = ', OOFWAG_REC.DATAHD
        CALL WIMG (5,'Is that correct (Y/N) ? ')                  
        CALL YESNO(ANSWER)                                                        
        TYPE*,IAM(),' '
        IF (ANSWER .NE. 1) THEN   
           STATUS = 1
           RETURN
        ENDIF
C
C      LOOP READING INPUT FILE
C      =======================
C  
       DO 300 WHILE (ABS(ST).NE.144)

       CALL READ_OOFWAG (ST)
       IF (ABS(ST).EQ.144 .OR. OOFWAG_REC.RECTYPE.EQ.'TL') GOTO 300
       IF (ST.NE.0) THEN
          CALL DISPERR (MSG_LUN, OOFWAG_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
          STATUS = 1
C          RETURN
       ENDIF
C
C     CHECK IF AGENT NUMBER EXISTS
C     ----------------------------
      AGENT = OOFWAG_REC.AGENT
      IF (AGENT.EQ.0) THEN
         CALL DISPERR (MSG_LUN, 'Agent number is equal to zero', 0, ' ', 0, ' ', 0) 
         STATUS = 1
C         RETURN
      ENDIF
      CALL GET_TERM(AGENT,TERM,ST)
      IF (ST.NE.0) THEN
         CALL DISPERR (MSG_LUN, 'Agent not found : ', AGENT, ' ', 0, ' ', 0) 
         STATUS = 1
C         RETURN
      ENDIF

      IF (OOFWAG_REC.WINS(1)+OOFWAG_REC.WINS(2)+OOFWAG_REC.WINS(3)+OOFWAG_REC.WINS(4)+OOFWAG_REC.WINS(5)+
     *    OOFWAG_REC.WINS(6)+OOFWAG_REC.JOKER_DIV .EQ. 0) THEN
         CALL DISPERR (MSG_LUN, 'No winner division', 0, 'Record = ', OOFWAG_REC.RECNUM, 'Game = ', OOFWAG_REC.ON_GAME) 
         STATUS = 1
      ENDIF
C
C     CHECK FOR GAME TYPE AND INDEX
C     -----------------------------
      GAM = OOFWAG_REC.ON_GAME
      IF (GNTTAB(GAMTYP,GAM).LE.0 .OR. GNTTAB(GAMIDX, GAM).LE.0) THEN
         CALL DISPERR (MSG_LUN, 'Invalid Game TYPE or INDEX' , 0,  
     *                          'Type  = ', GNTTAB(GAMTYP,GAM),
     *                          'Index = ', GNTTAB(GAMIDX,GAM)) 
         STATUS = 1
C         RETURN
      ENDIF

300   CONTINUE     !LOOP READING INPUT FILE

      IF (ST.EQ.-144) THEN
          CALL DISPERR (MSG_LUN, OOFWAG_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
          STATUS = 1
          RETURN                   
      ENDIF

      RETURN
      END

