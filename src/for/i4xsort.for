C
C SUBROUTINE I4XSORT.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]I4XSORT.FOV                                  $
C  $Date::   17 Apr 1996 13:34:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C I4XSORT.FTN                                                                   
C                                                                               
C V03 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V02 09-DEC-91 RRB VALIDATE RANGE OF SORT ELEMENTS.
C V01 31-AUG-90 WOL INITIAL RELEASE.                                            
C                                                                               
C This subroutine will sort a multi-dimension Int*4 array                       
C on up to three indicies, returning the sorted array in                        
C the input array. First index value must be greater then 0.                    
C If index2 value equals 0, index3 value must be 0.                             
C All values are treated as absolute (hex) values. This routine                 
C will interpret negative values as high positive values!!!!                    
C                                                                               
C                                                                               
C Calling sequence:                                                             
C                                                                               
C     CALL I4XSORT(I4ARRAY,LEN,CNT,INDX1,INDX2,INDX3)                           
C                                                                               
C Input parameters:                                                             
C                                                                               
C     I4ARRAY     Int*4(LEN,CNT)  Array to be sorted                            
C     LEN         Int*4           Dimension of I4ARRAY                          
C     CNT         Int*4           Number of elements to sort                    
C     INDX1-INDX3 Int*4           Indices to sort on                            
C                                                                               
C Output parameters:                                                            
C                                                                               
C     I4ARRAY     Int*4(LEN,CNT)  Sorted array                                  
C                                                                               
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
C Copyright 1990 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
      SUBROUTINE I4XSORT(I4ARRAY,LEN,CNT,INDX1,INDX2,INDX3)                     
      IMPLICIT NONE                                                             
C                                                                               
      INTEGER*4   LEN                     !Dimension of I4ARRAY                 
      INTEGER*4   CNT                     !Number of elements to sort           
      INTEGER*4   I4ARRAY(LEN,CNT)        !Array to sort                        
      INTEGER*4   INDX1,INDX2,INDX3       !Indices to sort on                   
      INTEGER*4   I4TEMP(100)             !Work array                           
      INTEGER*4   I,L,IR,II,JJ            !Work variables                       
C                                                                               
      IF (INDX1 .LE. 0 ) GO TO 3000                                             
      IF (CNT .LT. 2) GOTO 4000
C                                                                               
C                                                                               
C INITIALIZE SORT VARIABLES.                                                    
C                                                                               
      L = CNT/2 + 1                                                             
      IR = CNT                                                                  
2110  CONTINUE                                                                  
        IF (L .GT. 1) THEN                                                      
          L = L - 1                                                             
          DO 2120 I = 1, LEN                                                    
            I4TEMP(I) = I4ARRAY(I,L)                                            
2120      CONTINUE                                                              
        ELSE                                                                    
          DO 2130 I = 1, LEN                                                    
            I4TEMP(I) = I4ARRAY(I,IR)                                           
            I4ARRAY(I,IR) = I4ARRAY(I,1)                                        
2130      CONTINUE                                                              
          IR = IR - 1                                                           
          IF (IR .EQ. 1) THEN                                                   
            DO 2140 I = 1, LEN                                                  
              I4ARRAY(I,1) = I4TEMP(I)                                          
2140        CONTINUE                                                            
            RETURN                                                              
          ENDIF                                                                 
        ENDIF                                                                   
        II = L                                                                  
        JJ = L + L                                                              
2150    CONTINUE                                                                
        IF (JJ .LE. IR) THEN                                                    
          IF (JJ .LT. IR) THEN                                                  
            IF(I4ARRAY(INDX1,JJ) .LT. 0 .AND.                                   
     *         I4ARRAY(INDX1,JJ+1) .GT. 0) GO TO 2154                           
            IF(I4ARRAY(INDX1,JJ) .GT. 0 .AND.                                   
     *         I4ARRAY(INDX1,JJ+1) .LT. 0) GO TO 2152                           
            IF(I4ARRAY(INDX1,JJ) .GT. I4ARRAY(INDX1,JJ+1)) GO TO 2154           
            IF(I4ARRAY(INDX1,JJ) .LT. I4ARRAY(INDX1,JJ+1)) GO TO 2152           
            IF (INDX2 .LE. 0) GO TO 2154                                        
            IF(I4ARRAY(INDX2,JJ) .LT. 0 .AND.                                   
     *         I4ARRAY(INDX2,JJ+1) .GT. 0) GO TO 2154                           
            IF(I4ARRAY(INDX2,JJ) .GT. 0 .AND.                                   
     *         I4ARRAY(INDX2,JJ+1) .LT. 0) GO TO 2152                           
            IF(I4ARRAY(INDX2,JJ) .GT. I4ARRAY(INDX2,JJ+1)) GO TO 2154           
            IF(I4ARRAY(INDX2,JJ) .LT. I4ARRAY(INDX2,JJ+1)) GO TO 2152           
            IF (INDX3 .LE. 0) GO TO 2154                                        
            IF(I4ARRAY(INDX3,JJ) .LT. 0 .AND.                                   
     *         I4ARRAY(INDX3,JJ+1) .GT. 0) GO TO 2154                           
            IF(I4ARRAY(INDX3,JJ) .GT. 0 .AND.                                   
     *         I4ARRAY(INDX3,JJ+1) .LT. 0) GO TO 2152                           
            IF(I4ARRAY(INDX3,JJ) .GE. I4ARRAY(INDX3,JJ+1)) GO TO 2154           
2152          CONTINUE                                                          
              JJ = JJ + 1                                                       
2154          CONTINUE                                                          
          ENDIF                                                                 
            IF(I4TEMP(INDX1) .LT. 0 .AND.                                       
     *         I4ARRAY(INDX1,JJ) .GT. 0) GO TO 2165                             
            IF(I4TEMP(INDX1) .GT. 0 .AND.                                       
     *         I4ARRAY(INDX1,JJ) .LT. 0) GO TO 2157                             
          IF (I4TEMP(INDX1) .GT. I4ARRAY(INDX1,JJ)) GO TO 2165                  
          IF (I4TEMP(INDX1) .LT. I4ARRAY(INDX1,JJ)) GO TO 2157                  
          IF (INDX2 .LE. 0) GO TO 2165                                          
            IF(I4TEMP(INDX2) .LT. 0 .AND.                                       
     *         I4ARRAY(INDX2,JJ) .GT. 0) GO TO 2165                             
            IF(I4TEMP(INDX2) .GT. 0 .AND.                                       
     *         I4ARRAY(INDX2,JJ) .LT. 0) GO TO 2157                             
          IF (I4TEMP(INDX2) .GT. I4ARRAY(INDX2,JJ)) GO TO 2165                  
          IF (I4TEMP(INDX2) .LT. I4ARRAY(INDX2,JJ)) GO TO 2157                  
          IF (INDX3 .LE. 0) GO TO 2165                                          
            IF(I4TEMP(INDX3) .LT. 0 .AND.                                       
     *         I4ARRAY(INDX3,JJ) .GT. 0) GO TO 2165                             
            IF(I4TEMP(INDX3) .GT. 0 .AND.                                       
     *         I4ARRAY(INDX3,JJ) .LT. 0) GO TO 2157                             
          IF (I4TEMP(INDX3) .GE. I4ARRAY(INDX3,JJ)) GO TO 2165                  
2157        CONTINUE                                                            
CV03            DO 2160 I = 1, LEN
CV03              I4ARRAY(I,II) = I4ARRAY(I,JJ)
CV032160        CONTINUE
	    CALL FASTMOV(I4ARRAY(1,JJ),I4ARRAY(1,II),LEN)	!V03
            II = JJ                                                             
            JJ = JJ + JJ                                                        
            GO TO 2150                                                          
2165        CONTINUE                                                            
            JJ = IR + 1                                                         
            GO TO 2150                                                          
        ENDIF                                                                   
        DO 2170 I = 1, LEN                                                      
          I4ARRAY(I,II) = I4TEMP(I)                                             
2170    CONTINUE                                                                
        GO TO 2110                                                              
3000  CONTINUE                                                                  
      TYPE *, ' I4XSORT: index1 value is less or equal 0 '                      
4000  CONTINUE
      RETURN                                                                    
      END                                                                       
