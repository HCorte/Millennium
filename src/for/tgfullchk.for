* TGFULLCHK.FOR                         
* -----------
C                                                                               
C V01 03-DEC-2000 UXN Initial release.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
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
C=======OPTIONS /CHECK=NOOVERFLOW/CHECK=NOUNDER
      SUBROUTINE TGFULLCHK(SYSBET, WIN, NUMROWS, HISHR, SHARES)
      IMPLICIT NONE                                                   
                                                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'   
C                                                                               
      INTEGER*4 FASTBIN                                                         
      INTEGER*4 CMB_MISS                                                        
      INTEGER*4 MISS                                                            
      INTEGER*4 NUM_MISS                                                        
      INTEGER*4 BETCNT                                                          
      INTEGER*4 BET1,BET2
      INTEGER*4 OFF,I,J
      INTEGER*4 NUMROWS                                                         
C                                                                               
      INTEGER*4 SYSBET(2,*)        ! TABLE OF SYSTEM BETS                       
      INTEGER*4 WIN(2,*)           ! TABLE OF WINS                              
      INTEGER*4 SHARES(0:NUMROWS)  ! SHARES TABLE TO BE UPDATED                 
      INTEGER*4 HISHR              ! HIGHEST SHARE (TO BE UPDATED)              
      INTEGER*4 ROW_MATCH(0:16)
      INTEGER*4 ROW_NON_MATCH(0:16) 
C                                                                               
C     OFFSETS:    0 = ALL, 1 = BET 1, 2 = BET 2, 3 = BET 3                      
C                 4 = MATCH 4                                                   
C                                                                               
C     Number of bits set (marks) in a nibble digit (9=0111b, 3)                 
C                                                                               
      INTEGER*4 CNTMARK(0:15) /0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4/                 
C                                                                               
C     Array of powers
C                                                                               
      INTEGER*4 POWERS(16,0:16)
      SAVE POWERS
C                                                                               
C     Counters of number of misses by marks in nibble                           
C                                                                               
      INTEGER*4 ROW_MISS_2                                                      
      INTEGER*4 ROW_MISS_3                                                      
      INTEGER*4 ROW_MISS_4                                                      
      INTEGER*4 ROW_MISS_6
      INTEGER*4 ROW_MISS_8
      INTEGER*4 ROW_MISS_9
      INTEGER*4 ROW_MISS_12
      INTEGER*4 ROW_MISS_16
C                                                                               
      INTEGER*4 MISS_2 /0/                                                      
      INTEGER*4 MISS_3 /0/                                                      
      INTEGER*4 MISS_4 /0/ 
      INTEGER*4 MISS_6,MISS_8, MISS_9, MISS_12, MISS_16
C                                                                             
      LOGICAL*4 FIRST/.TRUE./

      IF(FIRST) THEN
         FIRST = .FALSE.
	 CALL FASTSET(0,POWERS,17*16)
	 DO 10 I=1,16
	    DO J=0,16
	       POWERS(I,J) = I**J
	       IF(POWERS(I,J).GT.'07FFFFFF'X) GOTO 10 ! TOO BIG ALREADY
            ENDDO
10	 CONTINUE
      ENDIF
C                                                                               
C     INIT MATCHES AND NON MATCHES OF SINGLES, DUBLES AND TRIPLES               
C                                                                               
      CALL FASTSET(0,ROW_MATCH,17)
      CALL FASTSET(0,ROW_NON_MATCH,17)
C                                                                               
C     Find number of four bit bets with matches and three bit bets with matches 
C                                                                               
      DO 5, OFF=1,NUMROWS                                                      
C                                                                               
C        Get next row                                                           
C                                                                               
         BET1 = SYSBET(1,OFF)
         BET2 = SYSBET(2,OFF)
C                                                                               
C        Get number of bits set (Marks) in row                                  
C                                                                               
         BETCNT = CNTMARK(BET1)*CNTMARK(BET2) 
C                                                                               
C        Check if this row is match with winning bitmap                         
C                                                                               
         IF (BTEST(BET1,WIN(1,OFF)) .AND. BTEST(BET2,WIN(2,OFF))) THEN
C                                                                               
C           Row is a match, increment match by number/bit counter               
C                                                                               
            ROW_MATCH(BETCNT) = ROW_MATCH(BETCNT) + 1                           
         ELSE                                                                   
C                                                                               
C           Row is not a match, increment non-match by number/bit counter       
C                                                                               
            ROW_NON_MATCH(BETCNT) = ROW_NON_MATCH(BETCNT) + 1                   
         END IF                                                                 
5     CONTINUE                                                                  
C                                                                               
C     Get total number of rows with matches                                     
C        
      NUM_MISS = 1
      DO I=1,16
         ROW_MATCH(0) = ROW_MATCH(0) + ROW_MATCH(I)
         ROW_NON_MATCH(0) = ROW_NON_MATCH(0) + ROW_NON_MATCH(I)
	 NUM_MISS = NUM_MISS * POWERS(I,ROW_NON_MATCH(I))
      ENDDO
C                                                                               
C     Set highest share won                                                     
C                                                                               
      IF (ROW_MATCH(0).GT.HISHR) THEN                                           
         HISHR = ROW_MATCH(0)                                                   
      END IF                                                                    
C                                                                               
C     Get number of combinations generated from non-matching rows               
C                                                                               
C                                                                               
C     Get number of rows with no matches                                        
C                                                                               
C                                                                               
C                                                                               
C     GENERAL FORMULA TO FIND SMALLER SHARES WINNERS (1X2 TYPE):                
C                                                                               
C***       EXTRA=(2**0)*BINOMIAL(MATCH(2),MISS)*BINOMIAL(MATCH(3),0)+           
C***       +(2**1)*BINOMIAL(MATCH(2),MISS-1)*BINOMIAL(MATCH(3),1)+              
C***       +(2**2)*BINOMIAL(MATCH(2),MISS-2)*BINOMIAL(MATCH(3),2)+              
C***       +(2**3)*BINOMIAL(MATCH(2),MISS-3)*BINOMIAL(MATCH(3),3)+              
C***       + + +                                                                
C***       +(2**MISS)*BINOMIAL(MATCH(2),0)*BINOMIAL(MATCH(3),MISS)              
C***       EXTRA=EXTRA*(2**MATCH0(2))                                           
                                                                                
C                                                                               
C     Update all shares by number of winning combinations                       
C                                                                               
C     Two mark rows are used as the outside loop as these are likely to be      
C     least used.                                                               
C                                                                               
C                                                                               
      DO 70 ROW_MISS_2 = 0, ROW_MATCH(2)                                        
        MISS_2 = FASTBIN(ROW_MATCH(2),ROW_MISS_2)

        DO 60 ROW_MISS_3 = 0, ROW_MATCH(3)                                      
          MISS_3 = POWERS(2,ROW_MISS_3) * FASTBIN(ROW_MATCH(3),ROW_MISS_3)

          DO 50 ROW_MISS_4 = 0, ROW_MATCH(4)                              
            MISS_4 = POWERS(3,ROW_MISS_4) * FASTBIN(ROW_MATCH(4),ROW_MISS_4)

            DO 40 ROW_MISS_6 = 0, ROW_MATCH(6) 
               MISS_6 = POWERS(5,ROW_MISS_6) * FASTBIN(ROW_MATCH(6),ROW_MISS_6)

               DO 35 ROW_MISS_8 = 0, ROW_MATCH(8) 
                  MISS_8 = POWERS(7,ROW_MISS_8) * 
     *                     FASTBIN(ROW_MATCH(8),ROW_MISS_8)

                  DO 30 ROW_MISS_9 = 0, ROW_MATCH(9) 
                     MISS_9 = POWERS(8,ROW_MISS_9) * 
     *                        FASTBIN(ROW_MATCH(9),ROW_MISS_9)

                     DO 25 ROW_MISS_12 = 0, ROW_MATCH(12) 
                        MISS_12 = POWERS(11,ROW_MISS_12) * 
     *                            FASTBIN(ROW_MATCH(12),ROW_MISS_12)

                        DO 20 ROW_MISS_16 = 0, ROW_MATCH(16) 
                           MISS_16 = POWERS(15,ROW_MISS_16) * 
     *                               FASTBIN(ROW_MATCH(16),ROW_MISS_16)
                                                                                
C                                                                               
C           Get the total number of combinations generated by missed rows       
C                                                                               
            CMB_MISS = MISS_2 * MISS_3 * MISS_4 * MISS_6 * MISS_8 * MISS_9 *
     *                 MISS_12 * MISS_16 
C                                                                               
C           Factor in the combinations generated by completely missed           
C           rows                                                                
C                                                                               
            CMB_MISS = CMB_MISS * NUM_MISS                                      
C                                                                               
C           Add the number of missed rows for current calculation               
C                                                                               
            MISS = ROW_MISS_2 + ROW_MISS_3 + ROW_MISS_4 + ROW_MISS_6 + 
     *             ROW_MISS_8 + ROW_MISS_9 + ROW_MISS_12 + ROW_MISS_16
C                                                                               
C           Update shares for rows matched in current calculation,              
C           shares are updated by the number of combinations paid               
C           for on ticket                                                       
C                                                                               
            SHARES(ROW_MATCH(0)-MISS) = SHARES(ROW_MATCH(0)-MISS) +             
     *                                  CMB_MISS                                
20	CONTINUE
25	CONTINUE
30	CONTINUE
35	CONTINUE
37      CONTINUE
40	CONTINUE
50      CONTINUE                                                              
60      CONTINUE                                                                
70    CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
