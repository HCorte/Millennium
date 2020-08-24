C
C SUBROUTINE DECRYPTV
C $Log:   GXAFXT:[GOLS]DECRYPTV.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:50:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:03:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - encryptv.for **
C
C DECRYPTV.FTN                                                                  
C                                                                               
C V01 15-SEP-90 WOL INITIAL RELEASE FOR MARYLAND.                                
C                                                                               
C This subroutine will decrypt the buffer using encryption key                  
C                                                                               
C Calling sequence:                                                             
C                                                                               
C     CALL DECRYPTV(BUFFER, CODEKEY, HALFCODEKEY)                               
C                                                                               
C Input parameters:                                                             
C                                                                               
C     BUFFER      Int*2(LENBUF)   Buffer to be decrypted                        
C     CODEKEY     Int*4(KEYLEN)   Encryption key                                
C                                 CODEKEY is 8 bytes long                       
C     HALFCODEKEY Int*4(KEYLEN)   Half encryption key.                          
C                                 This key has been obtained from encryption                    
C                                 key by shifting to the left by 7, bit pattern                         
C                                 derived from CODEKEY (taking every second 1               
C                                 bit beginning from the rigthmost one).             
C                                                                               
C Output parameters:                                                            
C                                                                               
C     BUFFER      Int*2(LENBUF)   Decrypted buffer                              
C                                                                               
C                                                                               
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE DECRYPTV(BUFFER, CODEKEY, HALFCODEKEY)                         
      IMPLICIT NONE                                                     
C                                                                               
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PROCOM.DEF'                                                             
      INTEGER*4   BUFFLEN, BUFFOFF, STARTOFF                                    
      BYTE        BUFFER(*)
      INTEGER*4   CODEKEY(*)                                                    
      INTEGER*4   HALFCODEKEY(*)                                                
      INTEGER*4   ONESCOUNT(8)                                                  
      INTEGER*4   ONESNUM(0:255)                                                
      INTEGER*4   TWOI4(2)                                               
      INTEGER*4   I, K, MYBYT1, MYBYT2, MYBYT3, MYBYT4                          
      LOGICAL     BEG_END                                                       
      INTEGER*2   I2TEMP
      BYTE        I1TEMP(2)
      BYTE        BYT1(4), BYT2(4)
      INTEGER*4   MASK/Z000000FF/
      INTEGER*4   TEMP,CNT
      INTEGER*4   M1, M2, M3, SUM
      BYTE        BTEMP(4), BSUM(4)
      EQUIVALENCE(TEMP,BTEMP)
      EQUIVALENCE(SUM,BSUM)
      EQUIVALENCE(I2TEMP,I1TEMP)
      EQUIVALENCE(MYBYT1,BYT1)
      EQUIVALENCE(MYBYT2,BYT2)
      DATA ONESNUM   /0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,                          
     *                1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,                          
     *                1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,                          
     *                1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,                          
     *                2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,                          
     *                3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,                          
     *                3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,                          
     *                4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8 /                         
      BEG_END = .FALSE.                                                         
      STARTOFF = (INPTAB*2 )*2 - 1                                              
      I1TEMP(1)=BUFFER(OUTLEN*2-1)
      I1TEMP(2)=BUFFER(OUTLEN*2-2)
      BUFFLEN = I2TEMP - 2
      IF (BUFFLEN .LT. 8) BUFFLEN = 8                                           
      IF (MOD(BUFFLEN,8) .EQ. 0) THEN                                           
        BEG_END = .TRUE.                                                        
        BUFFOFF = STARTOFF                                                      
      ELSE                                                                      
        BUFFOFF = STARTOFF + BUFFLEN - 8                                        
      ENDIF                                                                     
      MYBYT1=CODEKEY(1)
      MYBYT2=CODEKEY(2)
      DO 2210 K = 1, 4                                                          
        ONESCOUNT(K) = -ONESNUM(ZEXT(BYT1(K)))                                      
        ONESCOUNT(K + 4) = -ONESNUM(ZEXT(BYT2(K)))                                      
2210  CONTINUE                                                                  
2230  CONTINUE                                                                  

      CALL LIB$MOVC3(8,BUFFER(BUFFOFF),TWOI4(1))
C
C	REVERSE BYTE ORDER IN I4
C
           MYBYT1=TWOI4(1)
           BYT2(1)=BYT1(4)
           BYT2(2)=BYT1(3)
           BYT2(3)=BYT1(2)
           BYT2(4)=BYT1(1)
           TWOI4(1)=MYBYT2
           MYBYT1=TWOI4(2)
           BYT2(1)=BYT1(4)
           BYT2(2)=BYT1(3)
           BYT2(3)=BYT1(2)
           BYT2(4)=BYT1(1)
           TWOI4(2)=MYBYT2
C                                                                               
C EQUIVALENCE OF THE TWOI4 AND THE ENCRYPTION KEY                               
C                                                                               
      MYBYT2 = IAND(TWOI4(1), CODEKEY(1))                                     
      MYBYT3 = NOT(TWOI4(1))                                                  
      MYBYT4 = NOT(CODEKEY(1))                                                
      MYBYT1 = IAND(MYBYT3, MYBYT4)                                           
      TWOI4(1) = IOR(MYBYT1, MYBYT2)                                          
      MYBYT2 = IAND(TWOI4(2), CODEKEY(2))                                     
      MYBYT3 = NOT(TWOI4(2))                                                  
      MYBYT4 = NOT(CODEKEY(2))                                                
      MYBYT1 = IAND(MYBYT3, MYBYT4)                                           
      TWOI4(2) = IOR(MYBYT1, MYBYT2)                                          
C                                                                               
C SWAP THE BITS OF THE TWOI4(1) WITH TWOI4(2)  ( 6-10 FOR 21-25, 5 BITS)                             
C                                              (13-18 FOR  5-10, 6 BITS)
C                                              (22-25 FOR 14-17, 4 BITS)
C                                    
      MYBYT1 = TWOI4(1)
      MYBYT2 = TWOI4(2)
      CALL MVBITS(MYBYT1,6,5,TWOI4(2),21)
      CALL MVBITS(MYBYT2,21,5,TWOI4(1),6)
      CALL MVBITS(MYBYT1,13,6,TWOI4(2),5)
      CALL MVBITS(MYBYT2,5,6,TWOI4(1),13)
      CALL MVBITS(MYBYT1,22,4,TWOI4(2),14)
      CALL MVBITS(MYBYT2,14,4,TWOI4(1),22)
C                                                                               
C EXCLUSIVE OR OF THE TWOI4 AND ENCRYPTION KEY WORDS                            
C                                                                               
      TWOI4(1) = IEOR(TWOI4(1), CODEKEY(1))                                     
      TWOI4(2) = IEOR(TWOI4(2), CODEKEY(2))                                     
C                                                                               
C ROTATE THE BYTES OF TWOI4 TO THE RIGHT                                        
C                                                                               
      TEMP=TWOI4(1)      
C
C	ROTATE FIRST 4 BYTES
C
      DO 2260 K = 4, 1, -1
        CNT=ONESCOUNT(K)
        IF(CNT .EQ. 0) GO TO 2260
        IF(-CNT .EQ. 8) GO TO 2240
C	
C	SHIFTS CYCLICALLY LAST BYTE
C
        M1=ZEXT(BTEMP(1))
        M2=ISHFT(M1,CNT)
        M3=ISHFT(M1,CNT+8)
        M3=IAND(M3,MASK)
        SUM=IOR(M2,M3)
        BTEMP(1)=BSUM(1)
C	
C	SHIFTS CYCLICALLY FOUR BYTES
C	
2240    CONTINUE
        M1=ISHFT(TEMP,CNT)
        M2=ISHFT(TEMP,32+CNT)
        TEMP=M1+M2
2260  CONTINUE

      TWOI4(1)=TEMP      
      TEMP=TWOI4(2)      
C
C	ROTATE SECOND 4 BYTES
C

      DO 2360 K = 4, 1, -1
        CNT=ONESCOUNT(4+K)
        IF(CNT .EQ. 0) GO TO 2360
        IF(-CNT .EQ. 8) GO TO 2340
C	
C	SHIFTS CYCLICALLY LAST BYTE
C
        M1=ZEXT(BTEMP(1))
        M2=ISHFT(M1,CNT)
        M3=ISHFT(M1,CNT+8)
        M3=IAND(M3,MASK)
        SUM=IOR(M2,M3)
        BTEMP(1)=BSUM(1)
C	
C	SHIFTS CYCLICALLY FOUR BYTES
C	
2340    CONTINUE
        M1=ISHFT(TEMP,CNT)
        M2=ISHFT(TEMP,32+CNT)
        TEMP=M1+M2
2360  CONTINUE
      TWOI4(2)=TEMP      
C                                                                               
C COMPLEMENT THE CORRESPONDING BITS OF TWOI4                                    
C                                                                               
      TWOI4(1) = IEOR(TWOI4(1),HALFCODEKEY(1))                                  
      TWOI4(2) = IEOR(TWOI4(2),HALFCODEKEY(2))                                  
C
C	REVERSE BYTE ORDER IN I4
C
           MYBYT1=TWOI4(1)
           BYT2(1)=BYT1(4)
           BYT2(2)=BYT1(3)
           BYT2(3)=BYT1(2)
           BYT2(4)=BYT1(1)
           TWOI4(1)=MYBYT2
           MYBYT1=TWOI4(2)
           BYT2(1)=BYT1(4)
           BYT2(2)=BYT1(3)
           BYT2(3)=BYT1(2)
           BYT2(4)=BYT1(1)
           TWOI4(2)=MYBYT2

      CALL LIB$MOVC3(8,TWOI4(1),BUFFER(BUFFOFF))

      IF (BEG_END) THEN                                                         
        BUFFOFF = BUFFOFF + 8                                                   
        IF (BUFFOFF + 8 .GT. STARTOFF + BUFFLEN) GO TO 2500                     
      ELSE                                                                      
        BUFFOFF = STARTOFF                                                      
        BEG_END = .TRUE.                                                        
      ENDIF                                                                     
      GO TO 2230                                                                
2500  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
