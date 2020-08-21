C SUBROUTINE TO DEFINE MISCELLANEOUS PARAMETERS.                                
C               
c V03 18-MAY-93 HUGH   CONVERTED FOR VAX                                                                
C V02 23-JUL-91 HUGH   CHANGE SOME THINGS                                       
C V01 08-FEB-90 LOU R. INITIAL RELEASE FOR FINLAND.                             
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
C Copyright 1996 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                        
C=======OPTIONS /CHECK=NOOVERFLOW/EXT       
      SUBROUTINE BLDMIS                                                         
      IMPLICIT NONE
C                     
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'                          
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:DESPAR.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
C                                  
C     ! Variables
C                       
      INTEGER*4 OID
      INTEGER*4 BCDC
      INTEGER*4 ECDC             
      INTEGER*4 VALUE         
      INTEGER*4 I, J, K, X, Y, SEL, EXT, ST
      INTEGER*2 BEGDAT(LDATE_LEN,PRM_NUMOPN)
      INTEGER*2 ENDDAT(LDATE_LEN,PRM_NUMOPN)
      INTEGER*2 WORK_BEGDAT(LDATE_LEN)
      INTEGER*2 WORK_ENDDAT(LDATE_LEN)
      INTEGER*4 FLAG,SFLAG,CFLAG
      INTEGER*4 COPY_INSNAM(5)
      INTEGER*4 NOPS
      CHARACTER*20 INSNAM
      CHARACTER*3 YN(2)
C
      EQUIVALENCE(INSNAM,COPY_INSNAM)
C
      DATA YN/' no','yes'/
C
      COMMON SCFREC                                                             
C                                                                               
      NOPS=PRM_NUMOPN

20    CONTINUE
C
      DO 30 I=1,NOPS                                                   
      WORK_BEGDAT(5)=SCF_OPNDATE(1,I)                                  
      WORK_ENDDAT(5)=SCF_OPNDATE(2,I)                                  
      CALL LCDATE(WORK_BEGDAT)                                          
      CALL LCDATE(WORK_ENDDAT)
      DO 33 J=1,LDATE_LEN
        BEGDAT(J,I)=WORK_BEGDAT(J)
        ENDDAT(J,I)=WORK_ENDDAT(J)
33    CONTINUE
30    CONTINUE   
C                                                     
      CALL CLRSCR(5)                                                            
      WRITE(5,9000)
      DO 35 I=1,NOPS
        WRITE(5,9010) I,I,SCF_OPNID(I),(BEGDAT(X,I),X=7,13),
     *                (ENDDAT(X,I),X=7,13)
35    CONTINUE
CRXK      WRITE(5,9015) NOPS+1,CMONY(SCFPAR(REDIMN),11,BETUNIT)
CRXK      WRITE(5,9020) CMONY(SCFPAR(REDDEF),11,BETUNIT)
CRXK      WRITE(5,9040)
      CALL INPNUM('Enter option ',SEL,1,NOPS,EXT)                    
      IF(EXT.LT.0) RETURN 
C                                                                               
C GET OPINION POLL INFORMATION                                                  
C                                                                               
      IF(SEL.GE.1.AND.SEL.LE.NOPS) THEN    
        TYPE*,'Enter Data for Opinion Poll ',SEL,':'                   
        CALL INPNUM('Enter OPINION POLL ID number ',OID,0,99,      
     *               EXT)                                                       
        IF(EXT.LT.0) GOTO 20            
        DO 40 I=1,NOPS
          IF(OID.EQ.SCF_OPNID(I).AND.I.NE.SEL.AND.OID.NE.0) THEN
            TYPE*,'ID number already assigned '
            CALL XWAIT(3,2,ST)
            GOTO 20
          ENDIF
40      CONTINUE         
        TYPE*,'Enter Beginning date of poll '                                   
        CALL INPDAT(BCDC,EXT)                                                   
        IF(EXT.LT.0) GOTO 20                                                    
        TYPE*,'Enter Ending date of poll '                                      
        CALL INPDAT(ECDC,EXT)                                                   
        IF(EXT.LT.0) GOTO 20                                                    
        SCF_OPNID(SEL)=OID                                             
        SCF_OPNDATE(1,SEL)=BCDC                                        
        SCF_OPNDATE(2,SEL)=ECDC                                        
        GOTO 20                                                                 
      ENDIF                          
      RETURN                                                                    
C                                                                               
9000  FORMAT(' Miscellaneous system features ')                              
9010  FORMAT(' ',I2,' - OpnPoll ',I1,' ID:',I3,
     *       ' beg:',7A2,' end:',7A2)                           
9015  FORMAT(' ',I2,' - Instant Validation Minimum    ',A11)
9020  FORMAT('     (Instant Validation Maximum is   ',A11,')')
9030  FORMAT(' ',I2,' - ',5A4,' Ins prc:',A11,
     *       ' sal enab:',A3,' csh enab:',A3)                       
9040  FORMAT('  E - Exit ')
9050  FORMAT(A20)                                                  
      END                                                                       
