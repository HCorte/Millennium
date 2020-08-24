C VAKCALC.FOR
C  
C V11 12-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V10 31-JUL-2000 OXK GIND 1 treated as other indexes
C V09 06-MAR-2000 OXK Unit # fixed
C V08 25-FEB-2000 OXK Vakio changes
C V07 10-JAN-2000 PXO Changed DSPWRF(6) to DSPAPL
C V06 26-JUL-1999 PXO Added question about final prognosis
C V05 28-APR-1999 RXK Stopsys optimization (some IAM()s added).
C V04 21-MAR-1995 HXK Minor changes
C V03 01-FEB-1995 HXK Changes for customer
C V02 16-DEC-1994 JXP SCREEN DISPLAY MODIFICATION
C V01 28-NOV-1994 JXP Initial revision.
C  
C SPORTS SHARECLC                                                         
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
        SUBROUTINE SPCALC(GNUM, GIND, DRAW)                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'

        ! arguments
        INTEGER*4  GNUM            ! game number
        INTEGER*4  GIND            ! game index
        INTEGER*4  DRAW            ! draw number

        ! variables
        INTEGER*4  FDB(7)          ! file description block
        INTEGER*4  ST              ! status
        INTEGER*4  I               ! counter variable
        INTEGER*4  TMPPOL(SPGDIV)
        INTEGER*4  TMPMOV,TMPPRN
	INTEGER*4  TMPASH(SPGDIV)
        INTEGER*4  ACTPER(SPGDIV)
	INTEGER*4  FINAL               ! FINAL PROGNOSIS 0 = NO, 1 = YES

        COMMON SCFREC
        COMMON /DSPCOM/ DSPREC
                                                           
	CALL FASTSET (0,TMPPOL,SPGDIV)
	TMPMOV=0
	CALL FASTSET (0,TMPASH,SPGDIV)
	CALL FASTSET (0,ACTPER,SPGDIV)

        ! open game file
        CALL OPENW(4,SCFGFN(1,GNUM),4,0,0,ST)                      
        CALL IOINIT(FDB,4,DSPSEC*256)                                  
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)             

        ! read record for CURRENT draw
        CALL READW(FDB,DRAW,DSPREC,ST)                             
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)          
C                                                                               
C       
        ! check game status                                        
        IF(DSPSTS .NE. GAMENV) THEN                                
            WRITE(5,900) IAM(),GTNAMES(TSPT),GIND,DRAW,DSPSTS            
            CALL GPAUSE                                            
        ENDIF                                                      
        WRITE(5,901) IAM(),GTNAMES(TSPT),GIND,DRAW                       
C                                                                               
C 
	CALL PRMYESNO('Is this the final prognosis? ',FINAL)

        DO I = 1, DSPDIV
           DSPSHV(I) = 0
        ENDDO
C
C ASK TO USER FOR EXTRAORDINARY DRAW ( ROLLOVED AMOUNT FROM EXTRA GAMES )
C
C NOTE: - WHEN THE DRAW HAS BEEN CANCELLED IS NOT NECESSARY TO ASK TO THE USER BY THE
C         JACKPOT ROOLOVED BECAUSE THE JACKPOT AMOUNT WILL BE NOT USED IN THE SHARE
C         CALCUALTION
C
        IF(DSPDCD .EQ. 0) THEN                   ! THE DRAW HAS NOT BEEN CANCELLED
           CALL SPROLEXTDRW(GNUM, DRAW, DSPREC)
        ENDIF
C
C DO SHARE CALCULATION PROCEDURE
C
	CALL SPDOCALC(GNUM, DRAW, DSPREC, TMPPOL, .TRUE.)

        DO I = 1, DSPDIV
           DSPTSR(I) = DSPSHR(I)
        ENDDO

	IF (FINAL.EQ.1) DSPSTS=GAMDON

        CALL WRITEW(FDB,DRAW,DSPREC,ST)                                 
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)               

	IF(DSPEXT .EQ. 0) TMPPRN=DSPPRN  !for normal game, keep current
C                                        !value to save it in next draw
C WRITE MOVING AMOUNT TO NEXT DRAW RECORD IF REGULAR
C UPDATE WRF-TABLE IF NON REGULAR
C
	IF (SCFSTP(GIND).EQ.1) THEN
           CALL READW(FDB,DRAW+1,DSPREC,ST)                             
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW+1)          

	   CALL FASTMOV(TMPPOL,DSPPOL,DSPDIV)
	   DSPAPL   = TMPMOV
	   CALL FASTMOV(TMPASH,DSPASH,DSPDIV)
	   IF(DSPEXT .EQ. 0) DSPPRN = TMPPRN

           CALL WRITEW(FDB,DRAW+1,DSPREC,ST)
           IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW+1)
           CALL CLOSEFIL(FDB)                                             
	ELSE
	    DO I=1,DSPDIV
		TMPPOL(I) = TMPPOL(I)/DYN_BETUNIT
	    ENDDO
	    CALL UPDRDFSPT(GIND,DRAW,TMPPOL)
	ENDIF

        RETURN

900     FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)       
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)                                    
	END               
