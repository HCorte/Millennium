C
C GET_OFFPRIZES_12.FOR                                                                    
C
C V03 12-AUG-2011 RXK "Millennium" replaced with "ES Evolution"
C V02 27-DEC-2010 FRP Lotto2 Changes
C V01 13-MAR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C READ OLD 12 day PAYMENT ORDERS (semana 22 e 23) SENT BY OFFLINE SYSTEM AND CREATE 
C THEM ON OUR ORDERS FILE                                                                               
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
      PROGRAM GET_OFFPRIZES_ANT      
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
      INCLUDE 'INCLIB:DBNREC.DEF'
      INCLUDE 'INCLIB:DTGREC.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:WINCOM.DEF'
      INCLUDE 'INCLIB:INTERFACES_REC.DEF'
                                                                               
      INTEGER*4   OOFWAGANT_LUN /TMP1_LUN/
      INTEGER*4   MSG_LUN /6/                                                   
      INTEGER*4   DIV, GAMEON, YEAR, WEEK
      INTEGER*4   ST, I, SZ
      INTEGER*4   ORDER_NUMBER(MAXGAM)
      INTEGER*4   LAST_OP_NUM(MAXGAM)
      INTEGER*2   DATE(12)
	INTEGER*4 DIA, MES, CDC
	INTEGER*4 YESNO                  
      INTEGER*4   CIVILWEEK, CCCWEK
      CALL COPYRITE                                                             
  	                                                                         
      TYPE*,IAM(),' '
      TYPE *,'------------------------------------------------------------------------------'   
      TYPE *,'<<<<< TRANSFERENCIA DAS ORDENS OFF-LINE ANTIGAS PARA O SISTEMA ES EVOLUTION >>>>>'
      TYPE *,'------------------------------------------------------------------------------'
      TYPE*,IAM(),' '

C                                                                               
C     GET AND OPEN INPUT FILE
C     *********************** 
      CALL OPEN_OOFWAGANT (OOFWAGANT_LUN,ST)
      IF (ST.NE.0) THEN
         TYPE*,IAM(),'>> OOFWAGANT.ASC Open error,  status =',ST
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
    
C
C     VALIDATE INTEGRITY OF INPUT FILE 
C     ********************************
      CALL CHECK_INPUT_FILE (ST)
      IF (ST.NE.0) THEN
	 CALL DISPERR (MSG_LUN, 'There where errors Consisting OOFWAGANT.ASC', 0, ' ', 0, ' ', 0)
	 CALL PRMYESNO('Continua mesmo assim (Y/N) ? ', yesno)
	 IF(YESNO.NE.1) THEN
            CALL GSTOP (GEXIT_FATAL)
         ENDIF
      ENDIF
C
C     CLOSE AND GO TO BEGINNING OF INPUT FILE AGAIN
C
      CLOSE (OOFWAGANT_LUN)
C
C     SAVE LAST_OP_NUM
C
      DO I= 1,MAXGAM
        LAST_OP_NUM(I) = OOFWAGANT_REC.LAST_OP_NUM(I)
      ENDDO

      CALL OPEN_OOFWAGANT (OOFWAGANT_LUN, ST)
      IF (ST.NE.0) THEN
         TYPE*,IAM(),'>> OOFWAGANT.ASC Open error,  status =',ST
         CALL GSTOP(GEXIT_FATAL)
      ENDIF

C
C     GET LAST ORDER NUMBER FROM SCF
C     ******************************
         
      CALL GET_ORDER_NUMBER (ORDER_NUMBER, ST)
      IF (ST.NE.0) THEN
	 CALL DISPERR (MSG_LUN, 'Error getting Order Number from Configuration File', 0, ' ', 0, ' ', 0)
         CALL GSTOP (GEXIT_FATAL)
      ENDIF

      TYPE*,IAM(),'>>> OP numbers in SCF are:'
      DO I=1,MAXGAM
         TYPE*,IAM(),'    Game =', I, '     SCF OP = ', ORDER_NUMBER(I) 
      ENDDO
      TYPE*,IAM(),' '

C
C     OPEN ORDENS DE PAGAMENTO FILE
C     *****************************
      CALL OPEN_OPS('KEYED',ST)
      IF (ST.NE.0) THEN
	 CALL DISPERR (MSG_LUN, 'Error Opening Order file', 0, ' ', 0, ' ', 0)
         CALL GSTOP (GEXIT_FATAL)
      ENDIF             

C
C     READ FIRST RECORD HERE TO PROCESS THE HEADER
C     ********************************************
      CALL READ_OOFWAGANT (ST)
       IF (ST.NE.0) THEN
	  CALL DISPERR (MSG_LUN, OOFWAGANT_REC.ERRSTR, 0, ' ', 0, ' ', 0)
          CALL GSTOP(GEXIT_FATAL)
       ENDIF
C
C     LOOP READING INPUT FILE
C     ***********************                                                                              
C   
      DO 300 WHILE (ABS(ST).NE.144)
                                                                  
      CALL READ_OOFWAGANT (ST)
      IF (ABS(ST).EQ.144 .OR. OOFWAGANT_REC.RECTYPE.EQ.'TL') GOTO 300 
      IF (ST.NE.0) THEN
	  CALL DISPERR (MSG_LUN, OOFWAGANT_REC.ERRSTR, 0, ' ', 0, ' ', 0)
          CALL GSTOP(GEXIT_FATAL)
       ENDIF

       IF (MOD(OOFWAGANT_REC.RECNUM,5000).EQ.0) THEN
          TYPE*,IAM(), 'Records processed = ', OOFWAGANT_REC.RECNUM
       ENDIF
C
C     NOW UPDATE ORDERS FILE WITH PRIZE DATA 
C     **************************************	

      GAMEON = OOFWAGANT_REC.ON_GAME

      IF (OOFWAGANT_REC.NUM_OP.EQ.0) THEN
	  CALL DISPERR (MSG_LUN, 'Numero de ordem zerado', 0, 'Registro = ', OOFWAGANT_REC.RECNUM, ' ', 0)
      ELSE
         WRITE (OPS_REC.ORDER, FMT='(I6.6)') OOFWAGANT_REC.NUM_OP
      ENDIF

C
C     FILL OTHER FIELDS
C     *****************
C
      IF (OOFWAGANT_REC.RECLAMACAO) THEN
         OPS_REC.CLAIM = .TRUE.
      ELSE
         OPS_REC.CLAIM = .FALSE.
      ENDIF

      WRITE(OPS_REC.BANK,FMT='(I4.4)')   OOFWAGANT_REC.BANCO
      WRITE(OPS_REC.BRANCH,FMT='(I4.4)') OOFWAGANT_REC.BALCAO

      WRITE (OPS_REC.GAME,FMT='(I2.2)')   GAMEON

      YEAR = OOFWAGANT_REC.D_YEAR
      WEEK = OOFWAGANT_REC.D_WEEK
      WRITE (OPS_REC.YEARWEEK,FMT='(I4.4,I3.3)')  YEAR, WEEK

      WRITE (OPS_REC.AGENT, FMT='(I7.7)') OOFWAGANT_REC.AGENT   

      OPS_REC.JOKER_DIV = OOFWAGANT_REC.JOKER_DIV
      DO DIV=1,6
         OPS_REC.WINS(DIV) = OOFWAGANT_REC.WINS_GAME(DIV)
      ENDDO      

      OPS_REC.TOTAL_GAME  = OOFWAGANT_REC.GAME_VALUE
      OPS_REC.TOTAL_JOKER = OOFWAGANT_REC.JOKER_VALUE
      OPS_REC.HI_PRIZE = .TRUE.

      WRITE (OPS_REC.BILHETE,FMT='(I7.7,A7)') OOFWAGANT_REC.BILHETE, '       '
      OPS_REC.ONLINE_ORDER = .FALSE.

      OPS_REC.GENERATED      = .TRUE.
      OPS_REC.PRINTED_BY_OFF = .TRUE.


C
C        DATA PRESCRICAO PAGAMENTO
C
C        13 => SABADO (FECHAMENTO MUTUAS) DA PRIMEIRA SEMANA DO ANO 2001
C        CALCULO DESLOCAMENTO A PARTIR DAI PARA CHEGAR AO SABADO DA SEMANA DESEJADA E SOMO OS DIAS PARA PRESCRICAO 

         CIVILWEEK = WEEK
	 IF(GAMEON.EQ.6 .OR. GAMEON.EQ.7) CIVILWEEK = CCCWEK(YEAR,WEEK,GAMEON)

	 CALL SCML_DRAW_DATE (CIVILWEEK, YEAR, MES, DIA, CDC)
         OPS_REC.PAYABLE_CDC = CDC + PRGDAY(GAMEON)


      DATE(VYEAR) = CTOI(OOFWAGANT_REC.DATA_PAGAMENTO(1:4), SZ)
      DATE(VMON)  = CTOI(OOFWAGANT_REC.DATA_PAGAMENTO(5:6), SZ)
      DATE(VDAY)  = CTOI(OOFWAGANT_REC.DATA_PAGAMENTO(7:8), SZ)
      IF (DATE(VYEAR)+DATE(VMON)+DATE(VDAY).GT.0) THEN
         CALL BDATE(DATE)
         OPS_REC.PAID_CDC = DATE(VCDC) 
      ELSE
         OPS_REC.PAID_CDC = 0
      ENDIF
      
      IF (OPS_REC.PAID_CDC.NE.0) THEN
         OPS_REC.PAID_SENT_SAP = .TRUE.
      ELSE
         OPS_REC.PAID_SENT_SAP = .FALSE.
      ENDIF

      IF (OOFWAGANT_REC.LOTO2_VALUE .GT. 0) THEN
         OPS_REC.SPLITTED = .TRUE.
      ELSE
         OPS_REC.SPLITTED = .FALSE.
      ENDIF
C
C     WRITE TO ORDERS FILE
C
      IF (OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER .GT. 0 .OR. OPS_REC.SPLITTED) THEN
         WRITE(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
         IF (ST.NE.0) THEN
   	    CALL DISPERR (MSG_LUN, 'Error writing to Order file : Agent    = ' // OPS_REC.AGENT, 0, 
     *                             '                              Game     = ' // OPS_REC.GAME, 0, 
     *                             '                              YearWeek = ' // OPS_REC.YEARWEEK, 0,)
            CALL GSTOP (GEXIT_FATAL)
         ENDIF
      ENDIF

      IF (OOFWAGANT_REC.LOTO2_VALUE .GT. 0) THEN
C
C        Must create a separate OP for LOTO2
C
	 GAMEON = 4           !Loto 2 game number ***
         OPS_REC.GAME = '04'  !Loto 2 game number ***

         OPS_REC.JOKER_DIV = 0      !There is no Joker for loto2
         DO DIV=1,5
            OPS_REC.WINS(DIV) = OOFWAGANT_REC.WINS_LOTO2(DIV)
         ENDDO      

         OPS_REC.TOTAL_GAME   = OOFWAGANT_REC.LOTO2_VALUE
         OPS_REC.TOTAL_JOKER  = 0   !There is no Joker for loto2
C
C        WRITE TO ORDERS FILE
C
         WRITE(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
         IF (ST.NE.0) THEN
	    CALL DISPERR (MSG_LUN, 'Error writing to Order file : Agent    = ' // OPS_REC.AGENT, 0,
     *                             'Game = ' // OPS_REC.GAME // '     YearWeek = ' // OPS_REC.YEARWEEK, 0, 
     *                             'Record = ', OOFWAGANT_REC.RECNUM)
            CALL GSTOP (GEXIT_FATAL)
         ENDIF

      ENDIF

300   CONTINUE    !OOFWAGANT.ASC FILE READ LOOP

      CLOSE(OOFWAGANT_LUN)  
      CLOSE(OPS_LUN)

      TYPE*,IAM(),' '
      TYPE*,IAM(),'--------------------------------------------------------'
      TYPE*,IAM(),'>> TOTAL RECORDS PROCESSED : ', OOFWAGANT_REC.RECNUM-2
      TYPE*,IAM(),'--------------------------------------------------------'
      TYPE*,IAM(),' '

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
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4   MSG_LUN /6/
	INTEGER*4   TERM
   	INTEGER*4   ST, ANSWER, STATUS, GAM

C       READ FIRST RECORD HERE TO PROCESS THE HEADER
C       ********************************************
        CALL READ_OOFWAGANT (ST)
        IF (ST.NE.0) THEN
           CALL DISPERR (MSG_LUN, OOFWAGANT_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
           STATUS = 1
           RETURN
        ENDIF

        TYPE*,IAM(),' '
        TYPE*,IAM(),'>> Date in Header = ', OOFWAGANT_REC.DATAHD
        CALL WIMG (5,'Is that correct (Y/N) ? ')                  
        CALL YESNO(ANSWER)                                                        
        TYPE*,IAM(),' '
        IF (ANSWER .NE. 1) THEN   
           STATUS = 1
           RETURN
        ENDIF
C
C      LOOP READING INPUT FILE
C      ***********************
C  
       DO 300 WHILE (ABS(ST).NE.144)

       CALL READ_OOFWAGANT (ST)
       IF (ABS(ST).EQ.144 .OR. OOFWAGANT_REC.RECTYPE.EQ.'TL') GOTO 300
       IF (ST.NE.0) THEN
          CALL DISPERR (MSG_LUN, OOFWAGANT_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
          STATUS = 1
C          RETURN
       ENDIF
C
C      CHECK FOR AGENT
C
       CALL GET_TERM(OOFWAGANT_REC.AGENT,TERM,ST)
      IF (ST.NE.0) THEN
         CALL DISPERR (MSG_LUN, 'Agent not found : ', OOFWAGANT_REC.AGENT, ' RECORD = ', 
     *                 OOFWAGANT_REC.RECNUM, ' ', 0) 
         STATUS = 1
C         RETURN
      ENDIF

C
C     CHECK FOR GAME TYPE AND INDEX
C
      GAM = OOFWAGANT_REC.ON_GAME
      IF (GNTTAB(GAMTYP,GAM).LE.0 .OR. GNTTAB(GAMIDX, GAM).LE.0) THEN
         CALL DISPERR (MSG_LUN, 'Invalid Game TYPE or INDEX' , 0,  
     *                          'Type  = ', GNTTAB(GAMTYP,GAM),
     *                          'Index = ', GNTTAB(GAMIDX,GAM)) 
         STATUS = 1
C         RETURN
      ENDIF

      IF (OOFWAGANT_REC.LOTO2_VALUE .GT. 0 .AND. OOFWAGANT_REC.ON_GAME .NE. 2) THEN
         CALL DISPERR (MSG_LUN, 'Value for LOTO2 in game other than LOTO', 0,  
     *                          'Game  = ',OOFWAGANT_REC.ON_GAME ,
     *                          'Record = ', OOFWAGANT_REC.RECNUM) 
         STATUS = 1
C         RETURN
      ENDIF


300   CONTINUE     !LOOP READING INPUT FILE

      IF (ST.EQ.-144) THEN
          CALL DISPERR (MSG_LUN, OOFWAGANT_REC.ERRSTR, 0, ' ', 0, ' ', 0) 
          STATUS = 1
          RETURN                   
      ENDIF

      RETURN
      END
