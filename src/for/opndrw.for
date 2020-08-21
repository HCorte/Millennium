C
C $LOG :  $
C OPNDRW.FOR                                                                   
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
C SUBROUTINE TO OPEN DRAW FILES FOR LTOST48, LTOST39, SPTSTA
C =======================================================                       
C       
C=======OPTIONS/CHECK=NOOVERFLOW/EXT                                                                  
        SUBROUTINE OPNDRW(CDC,UNT,DRWFILE)                                           
        IMPLICIT NONE
C                                                                               
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'


        ! arguments
        INTEGER*4  CDC          ! CDC date
        INTEGER*4  UNT          ! logical unit
        INTEGER*4  DRWFILE(5)   ! draw file name                       


        ! variables
        INTEGER*4  ST           ! status
        INTEGER*4  INLEN                       
        INTEGER*4  VOLN

        CHARACTER*4  CVOLN

        EQUIVALENCE(VOLN,CVOLN)

C                                                                               
        CALL BINASC(DRWFILE,10,4,CDC)                                                
10      CONTINUE                                                                  
        CLOSE(UNIT=UNT)                                                           
        CALL OPENW(UNT,DRWFILE,5,0,0,ST)                                             
        IF(ST.EQ.0) THEN                                                          
            WRITE(5,930) IAM(),DRWFILE                                                       
            RETURN                                                                  
        ENDIF                                                                     
C                                                                               
        WRITE(5,900) IAM(),DRWFILE,ST                                                      
        CALL PRMTEXT('Enter new draw pack volume name: ',CVOLN,INLEN)                          
        DRWFILE(1)=VOLN
        WRITE(5,920) IAM(),DRWFILE(1)                                                      
        CALL GPAUSE                                                                     
        GOTO 10                                                                   
C                                                                               
C       ================== Format Statements ===============                      
C                                                                               
900     FORMAT(1X,A,5A4,' open error> ',I4)                                         
910     FORMAT(A4)                                                                
920     FORMAT(1X,A,' Mount ',A4,' pack and continue process')                       
930     FORMAT(1X,A,' Scanning file ',5A4)                                           
C                                                                               
        END                                                                       
