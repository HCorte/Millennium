C VIS_VKSTSNP.FOR                                                                  
C
C V10 30-MAR-2015 MTK Modified Super 14 game
C V09 04-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V08 02-FEB-2001 JHR Changes To Portugal Project
C V07 12-DEC-2000 EPH Price with up to 4 decimals
C V06 31-MAY-2000 PXO Subroutine name from VAKSTSNP -> VKSTSNP
C V05 14-FEB-2000 OXK Removed hardcoded GIND=1 (Vakio changes)
C V04 13 Dec 1994 PXB Change page 2 to have two type of percentages displayed.
C V03 29 Sep 1993 HXK Initial revision.
C V02 24-MAR-1993 HJK ADDED PERCENTAGES PAGE           
C V01 20-DEC-1992 HJK INITIAL RELEASE FOR FINLAND         
C                                                                               
C SPORTS STATISTICS SNAPSHOT                                                     
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      SUBROUTINE SPSTSNP(TABLE,GIND)
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
                                              
      INCLUDE 'INCLIB:GLOBAL.DEF'                      
      INCLUDE 'INCLIB:CONCOM.DEF'                      

      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:VISCOM.DEF'                      
      INCLUDE 'INCLIB:SPTCOM.DEF'                      
      INCLUDE 'INCLIB:STACOM.DEF'
C                                             
      REAL*8 VAKPER1(SPGNBR,7)                                                  
C
C      REAL*8 VAKPER2(SPGNBR,3)
C      REAL*8 VAKPER3(SPGNBR,3)                                                  
C
C---- Added by Paul Brannigan. - 12-Dec-1994.

      REAL*8 VAKPER4(SPGNBR,7)                                                  
C                                                                               
      INTEGER*4 GIND,DRAW,TOTSAL,TOTBDS,GNUM,TOTTAB,I,J,TABLE
C
C     INTEGER*4 HH,MM   
C
      INTEGER*4 TEMPTOT2(SPGNBR)
      INTEGER*4 BCNT


      IF (GIND.LE.0.OR.GIND.GT.NUMSPT) GIND=1
      DRAW=0                                                                    
      TOTSAL=0                                                                  
      TOTBDS=0                                                                  
      GNUM=GTNTAB(TSPT,GIND)                                                    
      IF(GNUM.LT.1) THEN                                                        
        WRITE(CLIN23,3001)                                                      
        RETURN                                                                  
      ENDIF                                                                     
C                                                                               
      IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)                                           
      IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)                                           
C                                                                               
      DO 5 I=1,SPGENT                                                           
        TOTSAL=TOTSAL+SPTSAL(I,GIND)                                            
5     CONTINUE                                                                  
C     
      IF(SPTPRC(GIND).EQ.0) THEN
	TOTBDS = 0
      ELSE
        TOTBDS = IDNINT (DFLOAT(TOTSAL) / (SPTPRC(GIND)/P(PRFACTOR)))
      ENDIF
C                                                                               
      WRITE(CLIN1,901) GIND,DRAW                                                
      WRITE(CLIN3,903) STASPT_CUP(GIND),TOTBDS,CMONY(TOTSAL,11,BETUNIT)  
C                                                                               
      DO 90 I=4,22                                                              
         WRITE(XNEW( I),900)                                                   
90    CONTINUE                                                                  
C                                                    
      DO I = 1,SPGNBR                                                      
        TEMPTOT2(I) = 0
      END DO
                           
      IF(TABLE.EQ.2) THEN                                                       
        DO 91 J=1,7                                                             
          TOTTAB=0                                                              
          DO 92 I=1,SPGNBR                                                      
            TOTTAB=TOTTAB+STASPT_TAB1(I,J,GIND)     
            TEMPTOT2(I) = TEMPTOT2(I) + STASPT_TAB1(I,J,GIND)     
92        CONTINUE                                                              
          DO 93 I=1,SPGNBR                                                      
            IF(TOTTAB.GT.0) THEN                                                
              VAKPER1(I,J)=DFLOAT(STASPT_TAB1(I,J,GIND))*1.0D2/DFLOAT(TOTTAB)   
              IF (VAKPER1(I,J) .GE. 100.0) VAKPER1(I,J) = 99.9
            ELSE                                                                
              VAKPER1(I,J)=0.0D0                                                
            ENDIF                                                               
93        CONTINUE                                                              
91      CONTINUE                                                                

C---- Added by Paul Brannigan. - 12-Dec-1994.
C---- Work out percentages accross to table.

        DO J = 1,7
          DO I = 1,SPGNBR
            IF (TEMPTOT2(I) .GT. 0) THEN
              VAKPER4(I,J)=DFLOAT(STASPT_TAB1(I,J,GIND)) * 
     *                     1.0D2/DFLOAT(TEMPTOT2(I))
              IF (VAKPER4(I,J) .GE. 100.0) VAKPER4(I,J) = 99.9
            ELSE                                                                
              VAKPER4(I,J)=0.0D0                                                
            END IF
          END DO
        END DO
       ENDIF
C
C V08 REMOVED FOR PORTUGAL PROJECT ( SANTA CASA DA MISSERICORDIA DON'T NEED )
C 
C      ELSEIF(TABLE.EQ.3.OR.TABLE.EQ.4) THEN                                                   
C        DO 94 J=1,3                                                             
C          TOTTAB=0                                                              
C          DO 95 I=1,SPGNBR                                                      
C            TOTTAB=TOTTAB+STASPT_TAB2(I,J,GIND)
C95        CONTINUE                                                              
C          DO 96 I=1,SPGNBR                                                      
C            IF(TOTTAB.GT.0) THEN                                                
C              VAKPER2(I,J)=DFLOAT(STASPT_TAB2(I,J,GIND))*1.0D2/DFLOAT(TOTTAB)
C            ELSE                                                                
C              VAKPER2(I,J)=0.0D0                                                
C            ENDIF                                                               
C96        CONTINUE                                                              
C94      CONTINUE                                                                
C        DO 97 I=1,SPGNBR                                                        
C          TOTTAB=0                                                              
C          DO 98 J=1,3                                                           
C            TOTTAB=TOTTAB+STASPT_TAB2(I,J,GIND)
C98        CONTINUE                                                              
C          DO 99 J=1,3                                                           
C            IF(TOTTAB.GT.0) THEN                                                
C              VAKPER3(I,J)=DFLOAT(STASPT_TAB2(I,J,GIND))*1.0D2/DFLOAT(TOTTAB)  
C            ELSE                                                                
C              VAKPER3(I,J)=0.0D0                                                
C            ENDIF                                                               
C99        CONTINUE                                                              
C97      CONTINUE                                                                
C      ENDIF                                                                     
C                                                                               
C     IF(TABLE.LE.1.OR.TABLE.GT.4) THEN                                        
C
      BCNT = 0
      IF(SPTFRG(GIND).GT.0) BCNT = 1

      IF(TABLE.LE.1.OR.TABLE.GT.2) THEN                                        
        WRITE(CLIN5,9050)                                                       
        WRITE(CLIN6,9060)                                                       
        DO 100 I=1,SPTMAX(GIND)-BCNT
          WRITE(XNEW( I+7),9080) I,(STASPT_TAB1(I,J,GIND),J=1,7)
100     CONTINUE                                                                
C
C
C V08 REMOVED FOR PORTUGAL PROJECT ( SANTA CASA DA MISSERICORDIA DON'T NEED )
C
C      ELSEIF(TABLE.EQ.3) THEN                                                   
C        WRITE(CLIN5,9051)                                                       
C        WRITE(CLIN6,9061)                                                       
C        DO 1100 I=1,SPTMAX(GIND)                                                      
C          WRITE(XNEW( I+7),9081) I,                                            
C     *           STASPT_TAB2(I,1,GIND),VAKPER2(I,1),VAKPER3(I,1),    
C     *           STASPT_TAB2(I,2,GIND),VAKPER2(I,2),VAKPER3(I,2),     
C     *           STASPT_TAB2(I,3,GIND),VAKPER2(I,3),VAKPER3(I,3)    
C1100    CONTINUE                                                                
C     
       ELSEIF(TABLE.EQ.2) THEN                                             
        WRITE(CLIN5,9052)                                                       
        WRITE(CLIN6,9062)           

        BCNT = 0
        IF(SPTFRG(GIND).GT.0) BCNT = 1
                                            
        DO 2100 I=1,SPTMAX(GIND)-BCNT                                                      
          WRITE(XNEW( I+7),9082) I, VAKPER1(I,1),VAKPER4(I,1),
     *                              VAKPER1(I,2),VAKPER4(I,2),
     *                              VAKPER1(I,3),VAKPER4(I,3),
     *                              VAKPER1(I,4),VAKPER4(I,4),
     *                              VAKPER1(I,5),VAKPER4(I,5),
     *                              VAKPER1(I,6),VAKPER4(I,6),
     *                              VAKPER1(I,7),VAKPER4(I,7)
2100    CONTINUE                                                                

C
C
C V08 REMOVED FOR PORTUGAL PROJECT ( SANTA CASA DA MISSERICORDIA DON'T NEED )
C
C      ELSEIF(TABLE.EQ.4) THEN       
C        IF(SPSUPT(GIND).GT.0) THEN           
C           HH=SPSUPT(GIND)/3600
C           MM=(SPSUPT(GIND)-3600*HH)/60
C           WRITE(CLIN5,9070) SPSVER(GIND),HH,MM
C        ELSE
C           WRITE(CLIN5,9071) SPSVER(GIND)
C        ENDIF
C        WRITE(CLIN7,9072)                                                       
C        DO I=1,SPTMAX(GIND)
C           WRITE(XNEW(I+8),9073) I,
C     *           DFLOAT(SPSROP(I,1,GIND))/1.D2,
C     *           VAKPER3(I,1),
C    *           DABS(DFLOAT(SPSROP(I,1,GIND))/1.D2-VAKPER3(I,1)),
C     *           DFLOAT(SPSROP(I,2,GIND))/1.D2,
C     *           VAKPER3(I,2),
C     *           DABS(DFLOAT(SPSROP(I,2,GIND))/1.D2-VAKPER3(I,2)),
C     *           DFLOAT(SPSROP(I,3,GIND))/1.D2,
C     *           VAKPER3(I,3),
C     *           DABS(DFLOAT(SPSROP(I,3,GIND))/1.D2-VAKPER3(I,3))
C        ENDDO                                                       
      ENDIF                                                                     
C                                                                               
      RETURN                                                                    
C                                                                               
C                                                                               


900   FORMAT(80(' '))

901   FORMAT(1X,'Sport ',I1,'  Mark Statistics     Draw ',I4)

903   FORMAT(1X,'Total coupons ',I8,    
     *       6X,'Total boards  ',I8,                          
     *       6X,'Sales ',A11)

9050  FORMAT(1X,'Mark combinations')                                            

C
C9051  FORMAT(1X,'Mark popularity')                                              
C

9052  FORMAT(1X,'Mark combinations %')

9060  FORMAT(1X,'Match   ',                                                     
     *       3X,'(1--)',                                                        
     *       5X,'(-X-)',                                                        
     *       5X,'(--2)',                                                        
     *       5X,'(1X-)',                                                        
     *       5X,'(1-2)',                                                        
     *       5X,'(-X2)',                                                        
     *       5X,'(1X2)')                                                        

C
C9061  FORMAT(15X,'1   M %    1 %',                                              
C     *       10X,'X   M %    X %',                                              
C     *       10X,'2   M %    2 %')                                              
C

9062  FORMAT(7X,'M  (1--)  ',
     *          'M  (-X-)  ',
     *          'M  (--2)  ',
     *          'M  (1X-)  ',
     *          'M  (1-2)  ',
     *          'M  (-X2)  ',
     *          'M  (1X2)  ')

C
C9070  FORMAT(1X,'WQP percent table.   Revision',I8,
C     *           ', updated ',I2.2,':',I2.2)
C9071  FORMAT(1X,'WQP percent table of revision',I8,', created during runsys')
C9072  FORMAT(6X,3(3X,'WQP%      %   Diff',3X))                                              
C9073  FORMAT(1X,I2,3(3X,3(1X,F6.2)))                                      
C

9080  FORMAT(1X,I2,4X,7(2X,I8))

C
C9081  FORMAT(1X,I2,3X,3(1X,I9,2(1X,F6.2)))                                      
C

9082  FORMAT(1X,I2,2X,7(2(1X,F4.1)))
                                                                               
3001  FORMAT('Sport game not active')                                           
                                                                               
      END                                                                       
