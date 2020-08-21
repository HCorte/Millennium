C 
C INVMIS.FOR                                                                    
C                                                                               
C $Log:   GXAFXT:[GOLS]INVMIS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:39:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   07 Aug 1993 18:38:40   HXK
C  declared hh,mm,ss
C  
C     Rev 1.3   07 Aug 1993 18:30:42   HXK
C  CHANGED TIME TO HOURS, MINUTES, SECONDS
C  
C     Rev 1.2   06 Aug 1993  9:52:32   HXK
C  BYTE ORDERING FIX FOR MESSAGE FROM TERMINAL
C  
C     Rev 1.1   28 Jun 1993 16:29:06   HXK
C  changed err message length from 5 to 6
C  brought chksum up to baseline
C  
C     Rev 1.0   27 Jun 1993 16:38:12   HXK
C  Initial revision.
C
C V03 27-JUN-93 HJK    VAX CONVERSION
C V02 09-MAY-90 MGM    GOT RID OFF DUPLICATE CODE         
C V01 04-OCT-89 LOU R. INITIAL RELEASE FINLAND.                             
C                                                                               
C SUBROUTINE TO PROCESS AGENTS MISCELLANEOUS ITEM SALES.                        
C                                                                               
C CALLING SEQUENCE:                                                             
C      CALL INVMIS(TRABUF,MESTAB,OUTLEN)                                        
C INPUT                                                                         
C     TRABUF - INTERNAL TRANSACTION FORMAT                                      
C     MESTAB - TERMINAL INPUT MESSAGE                                           
C                                                                               
C OUTPUT                                                                        
C     MESTAB - TERMINAL OUTPUT MESSAGE                                          
C     OUTLEN - OUTPUT MESSAGE LENGTH                                            
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
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW
      SUBROUTINE INVMIS(TRABUF,MESTAB,OUTLEN)                                   
      IMPLICIT NONE                                                  

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'           
      INCLUDE 'INCLIB:DESTRA.DEF'                
      INCLUDE 'INCLIB:CHKSUMCM.DEF'               
C                                                                               
C                                                                               
      INTEGER*4 TBUF(3),MYCHKSUM,ERRTYP,CHKLEN,I4TEMP                   
      INTEGER*4 XCOUNT,IND
      INTEGER*4 HH,MM,SS  !hours,minutes,seconds
      INTEGER*2 OUTLEN                                                          
      BYTE      MESTAB(*),I1TEMP(4)

      EQUIVALENCE(I4TEMP,I1TEMP)

      DATA ERRTYP/Z90/                                                          
C                                                                               
C CHECK FOR INVALID TRANS                                                       
C                                                                               
      IF(TRABUF(TERR).NE.NOER) THEN                                             
        TRABUF(TSTAT)=REJT                                                      
        GOTO 1000                                                               
      ENDIF                                                                     
C                                                                               
C GET ITEM NUMBER AND AMOUNT                                                    
C                                                                               
      XCOUNT=0
      IND=7                    
                                              
      I4TEMP=0
      I1TEMP(1)=MESTAB(IND+0)   !#7
      TRABUF(TSDT1)=I4TEMP                !ITEM NUMBER                  
      IND=IND+1

      I4TEMP=0
      I1TEMP(4)=MESTAB(IND+0)   !#8
      I1TEMP(3)=MESTAB(IND+1)   !#9
      I1TEMP(2)=MESTAB(IND+2)   !#10
      I1TEMP(1)=MESTAB(IND+3)   !#11
      TRABUF(TSDT2)=I4TEMP                !AMT IN MARKS                 
      IND=IND+4

      I4TEMP=0
      I1TEMP(2)=MESTAB(IND+0)   !#12
      I1TEMP(1)=MESTAB(IND+1)   !#13
      XCOUNT=I4TEMP                       !NUMBER OF ITEMS
      TRABUF(TSDT3)=MOD(XCOUNT,32768)                                           
      IF(IAND(XCOUNT,32768).NE.0) TRABUF(TSDT3)=-TRABUF(TSDT3)                  
      IND=IND+2
                                                                               
      IF(TRABUF(TSDT1).LT.1.OR.TRABUF(TSDT1).GT.10) THEN                        
        TRABUF(TSTAT)=REJT                                                      
        TRABUF(TERR)=INVL                                                       
        GOTO 1000                                                               
      ENDIF                                                                     
C                                                                               
C CHECK QUANTITY                                                                
C                                                                               
      IF(TRABUF(TSDT3).EQ.0) THEN                                               
        TRABUF(TSTAT)=REJT                                                      
        TRABUF(TERR)=INVL                                                       
        GOTO 1000                                                               
      ENDIF                                                                     
C                                                                               
      CALL UPDMIS(TRABUF)                                                       
C                                                                               
C BUILD OUTPUT MESSAGE BACK TO TERMINAL                                         
C                                                                               
      IND=5                                                                     
      I4TEMP = TRABUF(TTIM)
      HH=I4TEMP/3600
      MM=(I4TEMP-HH*3600)/60
      SS=I4TEMP-(HH*3600+MM*60)
      MESTAB(IND+0) = HH
      MESTAB(IND+1) = MM
      MESTAB(IND+2) = SS
      IND=IND+3
      OUTLEN = IND-1
      GOTO 9000
C                                                                               
C RETURN ERROR                                                                  
C                                                                               
1000  CONTINUE                                                                  
      MESTAB(2) = ERRTYP                                          
      MESTAB(5) = TRABUF(TERR)                                    
      OUTLEN=6 
C                                                                               
C CALCULATE CHECKSUM AND RETURN                                                 
C                                                                               
9000  CONTINUE                                                                  
      I4CCITT = TRABUF(TCHK)
      MESTAB(3) = I1CCITT(2)
      MESTAB(4) = I1CCITT(1)
      CHKLEN = OUTLEN - 1
      CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
      I4CCITT = MYCHKSUM
      MESTAB(3) = I1CCITT(2)
      MESTAB(4) = I1CCITT(1)
      RETURN                                                                    
      END                                                                       
