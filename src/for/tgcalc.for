C TGCALC.FOR
C  
C TOTOGOLO SHARECLC                                                         
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
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND                              
        SUBROUTINE TGCALC(GNUM, GIND, DRAW)                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  GIND            ! game index
        INTEGER*4  DRAW            ! draw number

        ! variables
        INTEGER*4  FDB(7)          ! file description block
        INTEGER*4  ST              ! status
        INTEGER*4  I               ! counter variable
        INTEGER*4  TMPPOL(TGGDIV)
        INTEGER*4  TMPMOV
	INTEGER*4  TMPASH(TGGDIV)
        INTEGER*4  ACTPER(TGGDIV)
	INTEGER*4  FINAL               ! FINAL PROGNOSIS 0 = NO, 1 = YES

        COMMON SCFREC
        COMMON /DTGCOM/ DTGREC
                                                           
	CALL FASTSET (0,TMPPOL,TGGDIV)
	TMPMOV=0
	CALL FASTSET (0,TMPASH,TGGDIV)
	CALL FASTSET (0,ACTPER,TGGDIV)

        ! open game file
        CALL OPENW(4,SCFGFN(1,GNUM),4,0,0,ST)                      
        CALL IOINIT(FDB,4,DTGSEC*256)                                  
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)             

        ! read record for CURRENT draw
        CALL READW(FDB,DRAW,DTGREC,ST)                             
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)          
C                                                                               
C       
        ! check game status                                        
        IF(DTGSTS .NE. GAMENV) THEN                                
            WRITE(5,900) IAM(),GTNAMES(TTGL),GIND,DRAW,DTGSTS            
            CALL GPAUSE                                            
        ENDIF                                                      
        WRITE(5,901) IAM(),GTNAMES(TTGL),GIND,DRAW                       
C                                                                               
C 
	CALL PRMYESNO('Is this the final prognosis? ',FINAL)

        DO I = 1, DTGDIV
           DTGSHV(I) = 0
        ENDDO
C
C ASK TO USER FOR EXTRAORDINARY DRAW ( ROLLOVED AMOUNT FROM EXTRA GAMES ) 
C
        CALL TGROLEXTDRW(GNUM, DRAW, DTGREC)
C
C DO SHARE CALCULATION PROCEDURE
C
	CALL TGDOCALC(GNUM, DRAW, DTGREC, TMPPOL, .TRUE.)

        DO I = 1, DTGDIV
           DTGTSR(I) = DTGSHR(I)
        ENDDO

	IF (FINAL.EQ.1) DTGSTS=GAMDON

        CALL WRITEW(FDB,DRAW,DTGREC,ST)                                 
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)               
C
C WRITE MOVING AMOUNT TO NEXT DRAW RECORD
C
         CALL READW(FDB,DRAW+1,DTGREC,ST)                             
         IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)          

	 CALL FASTMOV(TMPPOL,DTGPOL,DTGDIV)
	 DTGAPL   = TMPMOV
	 CALL FASTMOV(TMPASH,DTGASH,DTGDIV)

         CALL WRITEW(FDB,DRAW+1,DTGREC,ST)
         IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
         CALL CLOSEFIL(FDB)                                              

        RETURN

900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)       
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)                                    
	END               
