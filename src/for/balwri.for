C BALWRI.FTN                                                                    
C 
C V07 24-JAN-2011 FJG ut of bound issue
C V06 13-MAY-10 RXK BALWRI2 added for treatment of commissions. 
C V05 16-OCT-97 UXN BALWRI USES LOGICAL UNIT 99 !!!!
C V04 MARCH-93 PP CHANGE: ALL LIABILITYREPORTS HAVE REP.CODE 8                  
C                   HBNKWN'S RAPCODE = 14                                       
C V03 FEB-93 PP  ADDED RAPCODE FOR VILIABLE                                     
C V02 14-OCT-92 PP UPDATED FOR SPEDE'S GAMES                                    
C V01 07-MAY-91 PP INITIAL RELEASE FOR FINLAND                                  
C                                                                               
C SUBPROGRAM TO WRITE GAME TOTALS AND TOTAL SALES TO BALANS.FIL                 
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
C Copyright 1991 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                   
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
        IMPLICIT NONE
C                    
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
                                                
        INCLUDE 'INCLIB:GLOBAL.DEF'                                           
        INCLUDE 'INCLIB:CONCOM.DEF'                                           
        INCLUDE 'INCLIB:BALANS.DEF'
C	
        INTEGER*4  RAPCODE                        !
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT) !
        INTEGER*4  TOTSUMS(NO_BALSUMS)            !

        REAL*8     TOTREAL                        !

        INTEGER*4  FDB(7)                         !
        INTEGER*4  NOCHECK0                       !
        INTEGER*4  ST,I,J,K,GNUM,GTYP,GIND
	INTEGER*4  LUN/99/
                                                                               
        COMMON /NOCHECK0/ NOCHECK0
        COMMON BALREC
C
C START
C
        NOCHECK0 = -1
C
        CALL OPENW(LUN,SFNAMES(1,BAL),4,0,0,ST)
        CALL IOINIT(FDB,LUN,BALSEC*256)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),1,ST,0)
        ENDIF      
C
        CALL READW(FDB,1,BALREC,ST)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),2,ST,1)
        ENDIF
C
C       ================== Main Processing Loop ===============
C
        IF(BALCDC .NE. DAYCDC) THEN
            CALL FASTSET(0, BALRAPC, 17)
            CALL FASTSET(0, BALGSUMS, NO_BALREPS*MAXGAM*NUMFIN*NUMTOT)
            CALL FASTSET(0, BALCSUMS, 3*(MAXGAM+1))
            CALL FASTSET(0, BALTSUMS, NO_BALREPS*NO_BALSUMS)
            CALL FASTSET(0, BALDUE,  NO_BALREPS*2)
            CALL FASTSET(0, BALCOMS, NO_BALREPS*MAXGAM)
            CALL FASTSET(0, BALTCOM, NO_BALREPS) 
            BALCDC = DAYCDC
            DO I = 1, MAXGAM
                BALLSUMS(I) = 0.0D0
            END DO
        ENDIF
C                                                                               
C MOVE THE REPORT SUMS PASSED TO THE SUBPROGRAM VIA PARAMETERS TO               
C THE BALANS FILE RECORD                                                        
C                                                                               
        IF (RAPCODE.GE.80) THEN			  !V04
            BALRAPC(8) = 8
        ELSE
            BALRAPC(RAPCODE) = RAPCODE
        ENDIF
C
        IF (RAPCODE.LE.7 .AND. RAPCODE.NE.6) THEN
            DO I = 1, MAXGAM
                DO J = 1, NUMFIN
                    DO K = 1, NUMTOT
                        BALGSUMS(RAPCODE,I,J,K) = GAMESUMS(I,J,K)
                    END DO
                END DO
            END DO
            DO I = 1, NO_BALSUMS
                BALTSUMS(RAPCODE,I) = TOTSUMS(I)
            END DO
        ELSE IF (RAPCODE.EQ.6) THEN		  ! CSHREP
            DO I = 1, MAXGAM
               GTYP = GNTTAB(GAMTYP,I)
               IF(GTYP.NE.TPAS) THEN
                  BALCSUMS(1,I) = GAMESUMS(I,3,2)
               ENDIF       
            END DO
            BALCSUMS(1,MAXGAM+1) = TOTSUMS(6)
        ELSE IF (RAPCODE.EQ.9) THEN		  ! CSHPAS
            DO GIND = 1, NUMPAS
               GNUM = GTNTAB(TPAS,GIND)                
               BALCSUMS(3,GNUM) = GAMESUMS(GNUM,TVAL,DOLAMT)
            END DO
            BALCSUMS(3,MAXGAM+1) = TOTSUMS(2*TVAL) 
        ENDIF
C						  ! LIABS
        IF (RAPCODE.GE.81 .AND. RAPCODE.LE. 80+MAXGAM) THEN       !V04
            I = RAPCODE-80
            BALLSUMS(I) = TOTREAL
        END IF
C
C       WRITE RECORD TO BALANS FILE
C
        CALL WRITEW(FDB,1,BALREC,ST)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),3,ST,1)
        ENDIF
        CALL CLOSEFIL(FDB)
C
        RETURN
        END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                   
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BALWRI2(RAPCODE,GAMECOMS,TOTCOM,TOTDUE)
        IMPLICIT NONE
C                    
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
                                                
        INCLUDE 'INCLIB:GLOBAL.DEF'                                           
        INCLUDE 'INCLIB:CONCOM.DEF'                                           
        INCLUDE 'INCLIB:BALANS.DEF'
C	
        INTEGER*4  RAPCODE, GAMECOMS(MAXGAM), TOTCOM, TOTDUE(2)

        INTEGER*4  FDB(7)                         !
        INTEGER*4  NOCHECK0                       !
        INTEGER*4  ST
	INTEGER*4  LUN/99/
        INTEGER*4  I
                                                                               
        COMMON /NOCHECK0/ NOCHECK0
        COMMON BALREC
C
C START
C
        NOCHECK0 = -1
C
        CALL OPENW(LUN,SFNAMES(1,BAL),4,0,0,ST)
        CALL IOINIT(FDB,LUN,BALSEC*256)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),1,ST,0)
        ENDIF      
C
        CALL READW(FDB,1,BALREC,ST)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),2,ST,1)
        ENDIF

        DO I = 1, MAXGAM
           BALCOMS(RAPCODE,I) = GAMECOMS(I)            
        ENDDO
        BALTCOM(RAPCODE) = TOTCOM
        BALDUE(RAPCODE,1) = TOTDUE(1)   
        BALDUE(RAPCODE,2) = TOTDUE(2)   
C
C       WRITE RECORD TO BALANS FILE
C
        CALL WRITEW(FDB,1,BALREC,ST)
        IF(ST.NE.0) THEN
            CALL FILERR(SFNAMES(1,BAL),3,ST,1)
        ENDIF
        CALL CLOSEFIL(FDB)
C
        RETURN
        END
