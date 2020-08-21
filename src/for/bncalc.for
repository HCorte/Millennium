C BNCALC.FOR
C
C 
C V04 28-APR-99 RXK Stopsys optimization (some IAM()s added).
C V03 05-FEB-96 RXK Rfss 242. For share values is now used JIDINT 
C                   (instead of idnint)   
C V02 07-FEB095 HXK Fix for dividing Bingo div 1 by 5
C V01 07-JAN-95 HXK Initial revision.
C
C SUBROUTINE TO CALCULATE BINGO SHARE VALUES                                   
C                                                                               
C NB ONLY BINGO AB DIV 1 IS CALCULATED, ALL OTHER VALUES ARE SET
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
C Copyright 1999 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C               
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                    
      SUBROUTINE BNCALC(GNUM,GIND,DRAW)                                         
      IMPLICIT NONE
                                 
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'                
      INCLUDE 'INCLIB:GLOBAL.DEF'                                                          
      INCLUDE 'INCLIB:RECSCF.DEF'                                                          
      INCLUDE 'INCLIB:GTNAMES.DEF'                                                          
      INCLUDE 'INCLIB:DBNREC.DEF'                                                           

      COMMON SCFREC                                                             
      INTEGER*4 FDB(7),GROSS
      INTEGER*4 GNUM,ST,DRAW,GIND,ROLPOL,I

                                                                               
      CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                                     
      CALL IOINIT(FDB,3,DBNSEC*256)          
      IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                            
      CALL READW(FDB,DRAW,DBNREC,ST)                                            
      IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)                         
C                                                                               
C                                                                               
      IF(DBNSTS.NE.GAMENV) THEN                                                 
        WRITE(5,900) IAM(),GTNAMES(TBNG),GIND,DRAW,DBNSTS          
        CALL GPAUSE            
      ENDIF                                                                     
      WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,DRAW      
C                                                                               
C                                                                               
5     CONTINUE                                                                  
      DBNTAX=0                                                                  
      ROLPOL=0                                                                  
      DBNRES(1)=0                                                               
      DBNRES(2)=0                                                               
      GROSS = 0
      DO I=1,DBNDIV(BGOBAB)      
         DBNTSR(I,BGOBAB)=DBNSHR(I,BGOBAB)
      ENDDO                                                                  
      DO I=1,DBNDIV(BGOFHS)                                                    
         DBNTSR(I,BGOFHS)=DBNSHR(I,BGOFHS)
      ENDDO
C
C VERIFY BINGO A,B DIV 1 TOTAL AMOUNT IS CORRECT
C IT IS NOT NEEDED AFTER THE BINGO CHANGES IN OCT 97
C10      CONTINUE
C	 WRITE(5,902) IAM(),CMONY(DBNSHV(1,BGOBAB),12,VALUNIT)
C        CALL WIMG(5,'Is this value correct [Y/N] ')
C        CALL YESNO(FLAG)
C        IF(FLAG.NE.1) THEN
C           CALL INPMONY('Enter correct amount ',DBNSHV(1,BGOBAB),VALUNIT,EXT)
C           IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C           GOTO 10
C        ENDIF
C
C CALCULATE GROSS SHARE VALUE                                                   
C
      IF(DBNSHR(1,BGOBAB).EQ.0) THEN        
        !DO NOTHING
      ELSE                                                                      
        GROSS = JIDINT(DFLOAT(DBNSHV(1,BGOBAB)*DYN_VALUNIT) / 
     *                 DFLOAT(DBNSHR(1,BGOBAB)))    
        CALL GET_PENNIES_BRK(GROSS,DBNSHV(1,BGOBAB),DBNBRK(1,BGOBAB))         
        IF(DBNSHV(1,BGOBAB).LT.(1000/DYN_VALUNIT)) THEN  
           DBNSHV(1,BGOBAB)=(1000/DYN_VALUNIT)
           TYPE*,IAM(),'Division 1 prize less than 10.00 mks'                      
           TYPE*,IAM(),'Setting prize to 10.00 mks using reserve fund '
           GROSS = DBNSHV(1,BGOBAB)*DYN_BETUNIT
           CALL GET_PENNIES_BRK(GROSS,DBNSHV(1,BGOBAB),DBNBRK(1,BGOBAB)) 
        ENDIF                                                                   
        DBNRES(1)=DBNRES(1)+DBNBRK(1,BGOBAB)*DBNSHR(1,BGOBAB)
      ENDIF

      DBNWRF(PENAMT)=DBNRES(1)   !BREAKAGE                         
      DBNSTS=GAMDON                                                             

      CALL WRITEW(FDB,DRAW,DBNREC,ST)                                           
      IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)                         

      CALL CLOSEFIL(FDB)                                                        
      RETURN                                                                    
                                                                               
900   FORMAT(1X,A,1X,A8,I1,' event ',I4,' invalid game status> ',I4)
901   FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902   FORMAT(1X,A,1X,'Bingo A,B total amount for Super Bingo:',A12)
      END                                                                       
