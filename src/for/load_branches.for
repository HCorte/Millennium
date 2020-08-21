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
	INCLUDE 'INCLIB:BRANCH_REC.DEF'
                                                                               
        INTEGER*4      MSG_LUN /6/
        INTEGER*4      BRHASC_LUN /TMP1_LUN/
        INTEGER*4      ST
	CHARACTER*200  SCMLREC
        INTEGER*4      BANK, BRANCH
        INTEGER*4      TOTRECSINC, TOTRECSUPD
	CHARACTER*1    RESP
	CHARACTER*8    CHAVE


	TYPE*, '================================================================='
	TYPE*, '>>> ATUALIZANDO BRANCH.FIL A PARTIR DO FICHEIRO ASCII DA SCML <<<'
	TYPE*, '================================================================='

	RESP = 'N'
	TYPE  *, 'CONFIRMA A ATUALIZACAO DO FICHEIRO DE AGENCIAS (S/N) ? '
	READ(5,FMT='(A1)') RESP
	IF (RESP.NE.'S' .AND. RESP.NE.'s') STOP
C
C	OPEN BRANCH FILE
C	----------------
	CALL OPEN_BRANCH (ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening BRANCH file', 0, 'Loading Aborted', 0, 'Status = ', ST)
           STOP
        ENDIF 
C
C	OPEN SCML ASCII FILE
C	--------------------                                                                   
	OPEN (BRHASC_LUN, FILE='FILE:BRHMIL.ASC', STATUS='OLD',IOSTAT=ST)
	IF (ST.NE.0) THEN
	   CALL DISPERR (MSG_LUN, 'Error opening SCML ASCII BRANCH file', 0, 'Loading Aborted', 0, 'Status = ', ST)
           STOP
        ENDIF 

C
C	LOOP READING ASCII BANK FILE 
C	============================

	TOTRECSINC = 0
	TOTRECSUPD = 0

500	CONTINUE

	   READ(BRHASC_LUN, FMT='(A200)',IOSTAT=ST, END=200) SCMLREC
           IF (ST.NE.0) THEN
	      CALL DISPERR (MSG_LUN, 'Error Reading SCML ASCII BRANCH file', 0, 'Loading Aborted', 0, 'Status = ', ST)
              STOP
           ENDIF 

	   IF (SCMLREC(1:2).NE.'01') GOTO 500   !ONLY DETAIL RECORDS

	   READ (SCMLREC(3:6),FMT='(I4)') BANK
	   READ (SCMLREC(7:10),FMT='(I4)') BRANCH
  	   WRITE (CHAVE, FMT='(I4.4,I4.4)') BANK, BRANCH
C
C	   UPDATE BRANCH FILE
C	   ------------------
	   READ (BRH_LUN, KEYID=0, KEYEQ=CHAVE, IOSTAT=ST) BRANCH_REC
	   IF (ST.NE.0) THEN
C	      
C	      NOT FOUND (WILL BE INCLUDED)
C
  	      WRITE (BRANCH_REC.BANK,FMT='(I4.4)') BANK
  	      WRITE (BRANCH_REC.BRANCH,FMT='(I4.4)') BRANCH
	      BRANCH_REC.LONG_NAME = SCMLREC(11:43)
	      WRITE(BRH_LUN,IOSTAT=ST) BRANCH_REC
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error Writing to BRANCH file', 0, 'Loading Aborted', 0, 'Status = ', ST)
                 STOP
              ENDIF 

	      TOTRECSINC = TOTRECSINC + 1

	   ELSE
C	      
C	      FOUND (WILL BE UPDATED)
C
	      BRANCH_REC.LONG_NAME = SCMLREC(11:43)
	      REWRITE(BRH_LUN, IOSTAT=ST) BRANCH_REC
              IF (ST.NE.0) THEN
	         CALL DISPERR (MSG_LUN, 'Error REWriting to BRANCH file', 0, 'Loading Aborted', 0, 'Status = ', ST)
                 STOP
              ENDIF 

	      TOTRECSUPD = TOTRECSUPD + 1

	   ENDIF

        GOTO 500
	
C
C	CLOSE FILES
C
200	CONTINUE

	TYPE*, '===================================================='
	TYPE*, '>>> TOTAL DE REGISTROS INCLUIDOS   = ', TOTRECSINC
	TYPE*, '>>> TOTAL DE REGISTROS ATUALIZADOS = ', TOTRECSUPD
	TYPE*, '===================================================='

	CLOSE(BRH_LUN)
	CLOSE(BRHASC_LUN)

	STOP
	END
