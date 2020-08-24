C
C LOAD_BANKS.FOR                                                                    
C
C V01 27-MAR-2001 EPH INITIAL VERSION FOR SCML
C                                                                               
C LOAD BANK TABLE FROM SCML TO BANK.FIL VMS INDEXED FILE
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
C Copyright 2001 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM LOAD_BANKS
        IMPLICIT NONE                                                  
	                                                
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BANK_REC.DEF'
                                                                               
        INTEGER*4      MSG_LUN /6/
        INTEGER*4      BNKASC_LUN /TMP1_LUN/
        INTEGER*4      ST
	CHARACTER*200  SCMLREC
        INTEGER*4      BANK
        INTEGER*4      TOTRECS

	TYPE*, '============================================================='
	TYPE*, '>>> RECRIANDO BANK.FIL A PARTIR DO FICHEIRO ASCII DA SCML <<<'
	TYPE*, '============================================================='

C
C	OPEN SCML ASCII FILE
C	--------------------                                                                   
	OPEN (BNKASC_LUN, FILE='FILE:BNKMIL.ASC', STATUS='OLD',IOSTAT=ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening SCML ASCII BANK file', 0, 'Loading Aborted', 0, 'Status = ', ST)
           STOP
        ENDIF 

C
C	OPEN BANK FILE
C	--------------
	CALL OPEN_BANK ('WRITE',ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening BANK file', 0, 'Loading Aborted', 0, 'Status = ', ST)
           STOP
        ENDIF 

C
C	LOOP READING ASCII BANK FILE 
C	============================

	TOTRECS = 0

500	CONTINUE

	   READ(BNKASC_LUN, FMT='(A200)',IOSTAT=ST, END=200) SCMLREC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading SCML ASCII BANK file', 0, 'Loading Aborted', 0, 'Status = ', ST)
              STOP
           ENDIF 

	   IF (SCMLREC(1:2).NE.'01') GOTO 500   !ONLY DETAIL RECORDS

	   READ (SCMLREC(3:6),FMT='(I4)') BANK
  	   WRITE (BANK_REC.BANK,FMT='(I4.4)') BANK

	   BANK_REC.SHORT_NAME = SCMLREC(7:18)
	   BANK_REC.LONG_NAME  = SCMLREC(19:51)

	   BANK_REC.NIB_MUTUAS   = SCMLREC(56:72)
	   BANK_REC.NIB_PASSIVE  = SCMLREC(77:93)
	   BANK_REC.NIB_INSTANTS = SCMLREC(98:114)          !AAAACCCCCCCCCCCCC = AGENCIA/CONTA(COM DV)

	   BANK_REC.CONTA_PREMIO = SCMLREC(119:133)         !AAAACCCCCCCCCCC = AGENCIA/CONTA(SEM DV)
C
C	   WRITE TO BANK FILE
C	   ------------------
	   WRITE(BNK_LUN,IOSTAT=ST) BANK_REC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Writing to BANK file', 0, 'Loading Aborted', 0, 'Status = ', ST)
              STOP
           ENDIF 

	   TOTRECS = TOTRECS + 1

        GOTO 500
	
C
C	CLOSE FILES
C
200	CONTINUE

	TYPE*, '===================================================='
	TYPE*, '>>> TOTAL DE REGISTROS TRANSFERIDOS = ', TOTRECS
	TYPE*, '===================================================='

	CLOSE(BNK_LUN)
	CLOSE(BNKASC_LUN)

	STOP
	END
