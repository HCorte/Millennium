C
C GENERATE_OPS.FOR                                                                    
C
C
C V01 23-JAN-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C GENERATE OPS IN PRINT FORMAT
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
      OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM GENERATE_OPS      
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
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:WINCOM.DEF'
                                                                               
      INTEGER*4   IMP_LUN /TMP1_LUN/
      INTEGER*4   ST

      INTEGER*4   OFFLINE_GAME   
      INTEGER*4   WEEK
      INTEGER*4   YEAR
      INTEGER*4   AGENT 
      INTEGER*4   WINS(6)
      INTEGER*4   JOKER_DIV
      INTEGER*4   BILHETE

      INTEGER*4   ORDER_NUMBER
                           
      INTEGER*4   FILNAME(7)            
      CHARACTER   CFILNAME(28)            
      EQUIVALENCE (CFILNAME,FILNAME)

      INTEGER*4   ONLINE_GAME(MAXGAM) /1,2,3,4,6,7,0,0,0,0/

      INTEGER*4   GAM, GTYPE, DIV, GIND
      INTEGER*4   TOTAL_PRIZE, DRAW, CLOSE_DAY, GAMEON

      INTEGER*4   TERM
      LOGICAL     ENCONTRADO

      INTEGER*4   GETDRW   !FUNCTION

      CHARACTER*66 OP_TO_PRINT(17,6)      !FITS ALL THE OPS ON ONE PAGE
      INTEGER*4    AGT(0:3)               !ALL FOUR NECESSARY AGENTS TO DEFINE * POS => 0=ANTERIOR / 3=POSTERIOR
      INTEGER*4    POSASTERISCO
      INTEGER*4    NUMOPS_PAG, NUMOPS
      INTEGER*4    PAGT


      CALL COPYRITE                                                             
  	                                                                         
      TYPE*,IAM(),' '
      TYPE *,'-------------------------------------------------------------------'
      TYPE *,'<<<<< GERACAO DO ARQUIVO DE ORDENS PENDENTES (PARA IMPRESSAO) >>>>>' 
      TYPE *,'-------------------------------------------------------------------'
      TYPE*,IAM(),' '

C                                                                               
C     OPEN OUTPUT FORMATTED FILE (TO PRINT)
C     *************************************
120   CONTINUE              
      CALL WIMG(5,'Entre nome do Arquivo de Impressao de Ordens (VOLN:FILNAME)   ')
      READ(5,901) FILNAME
901   FORMAT(7A4)   
      IF (FILNAME(1).EQ.'    ') GOTO 120
      IF (CFILNAME(1).EQ.'E '.OR.CFILNAME(1).EQ.'e ') THEN
         CALL GSTOP(GEXIT_OPABORT)
      ENDIF
      OPEN (UNIT   = IMP_LUN,
     *      FILE   = FILNAME,
     *      IOSTAT = ST,
     *      STATUS = 'NEW') 
      IF (ST.NE.0) THEN
         TYPE*,IAM(),CFILNAME,' Open error,  status =',ST
         GOTO 120
      ENDIF
    
C
C     OPEN ORDENS DE PAGAMENTO 
C     ************************
      CALL OPEN_OPS('SEQUENTIAL',ST)
      IF (ST.NE.0) THEN
         TYPE*,IAM(),' '
         TYPE*,IAM(),'=================================================='
         TYPE*,IAM(),'>> ERRO DE ABERTURA NO ARQUIVO  ** FILE:OPS.FIL **'	
         TYPE*,IAM(),'=================================================='
         TYPE*,IAM(),' '
         CALL GSTOP (GEXIT_FATAL)
      ENDIF             

C
C     READ ORDERS FILE
C
      READ(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
      IF (ST.NE.0 .AND. ST.NE.-1) THEN
         TYPE*,IAM(),' '
         TYPE*,IAM(),'============================================'
         TYPE*,IAM(),'>> ERRO LENDO DO ARQUIVO  ** FILE:OPS.FIL **'	
         TYPE*,IAM(),'============================================'
         TYPE*,IAM(),' '
         CALL GSTOP (GEXIT_FATAL)
      ENDIF

C
C     READ LOOP
C     *********
C
      CALL CLEAN_OP_TO_PRINT(OP_TO_PRINT)
      NUMOPS     = 0
      NUMOPS_PAG = 0
      PAGT       = 0

      DO WHILE (ST.NE.-1)

         IF (OPS_REC.GENERATION_CDC .LE. DAYCDC .OR. OPS_REC.GENERATED_CDC .EQ. DAYCDC) THEN
C
C           THIS GOES TO PRINT
C

            NUMOPS    = NUMOPS + 1

            PAGT      = PAGT + 1

            AGT(PAGT) = OPS_REC.AGENT

            IF (PAGT.EQ.3) THEN
C
C              IT'S TIME TO DEFINE * FOR LAST 2 OP LINE 
C	    
               CALL WHERE_ASTERISC (AGT, POSASTERISCO)

C
C              SHIFT AGT TO DEAL WITH NEXT OP LINE 
C
	       AGT(0) = AGT(2)
               AGT(1) = AGT(3)
               AGT(2) = 0
               AGT(3) = 0

               PAGT = 1
C
C              INSERT * IN PRINT LINE
C
               IF (POSASTERISCO.NE.0) THEN
                  OP_TO_PRINT(POSASTERISCO,NUMOPS_PAG-1)(1:1) = '*'	       
               ENDIF

            ENDIF

            NUMOPS_PAG = NUMOPS_PAG + 1

	    IF (NUMOPS_PAG.EQ.7) THEN
               CALL PRINT_PAGE (IMP_LUN, OP_TO_PRINT)
               CALL CLEAN_OP_TO_PRINT(OP_TO_PRINT)
               NUMOPS_PAG = 1
            ENDIF

            OP_TO_PRINT( 1,NUMOPS_PAG) = '   111111111'
            OP_TO_PRINT( 2,NUMOPS_PAG) = '   222222222'
            OP_TO_PRINT( 3,NUMOPS_PAG) = '   333333333'
            OP_TO_PRINT( 4,NUMOPS_PAG) = '   ' // OPS_REC.YEARWEEK
            OP_TO_PRINT( 5,NUMOPS_PAG) = '  '
            OP_TO_PRINT( 6,NUMOPS_PAG) = '   ' // OPS_REC.GAME
            OP_TO_PRINT( 7,NUMOPS_PAG) = ' '
            OP_TO_PRINT( 8,NUMOPS_PAG) = '   ' // OPS_REC.ORDER
            OP_TO_PRINT( 9,NUMOPS_PAG) = ' '
            OP_TO_PRINT(10,NUMOPS_PAG) = ' '
            OP_TO_PRINT(11,NUMOPS_PAG) = ' '
            OP_TO_PRINT(12,NUMOPS_PAG) = ' '
            OP_TO_PRINT(13,NUMOPS_PAG) = ' '
            OP_TO_PRINT(14,NUMOPS_PAG) = ' '
            OP_TO_PRINT(15,NUMOPS_PAG) = ' '
            OP_TO_PRINT(16,NUMOPS_PAG) = '           16'
            OP_TO_PRINT(17,NUMOPS_PAG) = '           17'

         ENDIF
C
C        READ ORDERS FILE
C
         READ(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC
         IF (ST.NE.0 .AND. ST.NE.-1) THEN
            TYPE*,IAM(),' '
            TYPE*,IAM(),'============================================'
            TYPE*,IAM(),'>> ERRO LENDO DO ARQUIVO  ** FILE:OPS.FIL **'	
            TYPE*,IAM(),'============================================'
            TYPE*,IAM(),' '
            CALL GSTOP (GEXIT_FATAL)
         ENDIF

      ENDDO

      CALL WHERE_ASTERISC (AGT, POSASTERISCO)

      IF (POSASTERISCO.NE.0) THEN
         IF (MOD(NUMOPS_PAG,2).EQ.0) THEN
            OP_TO_PRINT(POSASTERISCO,NUMOPS_PAG-1)(1:1) = '*'	       
         ELSE
            OP_TO_PRINT(POSASTERISCO,NUMOPS_PAG)(1:1) = '*'	       
         ENDIF
      ENDIF

      CALL PRINT_PAGE (IMP_LUN, OP_TO_PRINT)

      CLOSE(IMP_LUN)  
      CLOSE(OPS_LUN)

      TYPE*,IAM(),' '
      TYPE*,IAM(),'----------------------------------------------'
      TYPE*,IAM(),'>> NUMERO OPS IMPRESSAS = ', NUMOPS
      TYPE*,IAM(),'----------------------------------------------'
      TYPE*,IAM(),' '

      CALL GSTOP(GEXIT_SUCCESS)                          

      END   




C	****************************************
	SUBROUTINE PRINT_PAGE (LUN, OP_TO_PRINT)
C       ****************************************
	IMPLICIT NONE

        CHARACTER*66 OP_TO_PRINT(17,6)      !FITS ALL THE 6 OPS ON ONE PAGE
	INTEGER*4    LUN
        INTEGER*4    OP, LIN

        DO OP = 1,6,2
	   DO LIN = 1,17
              WRITE(LUN, FMT='(A66,A66)') OP_TO_PRINT(LIN,OP), OP_TO_PRINT(LIN,OP+1)
           ENDDO
           WRITE(LUN, FMT='(A1)') ' '
        ENDDO

        CALL CLEAN_OP_TO_PRINT(OP_TO_PRINT)

	RETURN
	END



C	*********************************************
	SUBROUTINE WHERE_ASTERISC (AGT, POSASTERISCO)
C       *********************************************
	IMPLICIT NONE

	INTEGER*4 AGT(0:3), POSASTERISCO

        POSASTERISCO = 0

	IF (AGT(1).EQ.AGT(2)) THEN
           IF (AGT(1).EQ.AGT(0)) THEN
              IF (AGT(2).EQ.AGT(3)) THEN
                 POSASTERISCO = 0 !???????????
              ELSE
                 POSASTERISCO = 0 !???????????
              ENDIF
           ELSE
              IF (AGT(2).EQ.AGT(3)) THEN
                 POSASTERISCO = 1 
              ELSE
                 POSASTERISCO = 8
              ENDIF
           ENDIF
        ELSE
           IF (AGT(1).EQ.AGT(0)) THEN
              IF (AGT(2).EQ.AGT(3)) THEN
                 POSASTERISCO = 0 !???????????
              ELSE
                 POSASTERISCO = 17 
              ENDIF
           ELSE
              IF (AGT(2).EQ.AGT(3)) THEN
                 POSASTERISCO = 12 
              ELSE
                 POSASTERISCO = 17
              ENDIF
           ENDIF
        ENDIF

  	RETURN
 	END



C	******************************************
	SUBROUTINE CLEAN_OP_TO_PRINT (OP_TO_PRINT)
C       ******************************************
	IMPLICIT NONE

        CHARACTER*66 OP_TO_PRINT(17,6)      !FITS ALL THE 6 OPS ON ONE PAGE
        INTEGER*4    OP, LIN

        DO OP = 1,6
	   DO LIN = 1,17
              WRITE (OP_TO_PRINT(LIN,OP),15)
15            FORMAT(63(' '))
           ENDDO
        ENDDO

	RETURN
	END

