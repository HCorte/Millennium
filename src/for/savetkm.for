C
C $Log:   GXAFIP:[GOLS]SAVETKM.FOV  
C  
C     Rev 1.2   19 Feb 1997 15:09:20   HXK
C  Don not use TKMAFL
C  
C     Rev 1.1   13 Jan 1997 17:06:42   RXK
C  CDU, phase 2 
C  
C     Rev 1.0   17 Apr 1996 14:50:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.4   07 Oct 1993 21:15:20   GXA
C  Changed NTSBIT to TSBIT inorder to check the right bit....
C  
C     Rev 1.3   27 Sep 1993 14:12:16   GXA
C  Save Ticket Message Revision #'s.
C
C SAVETKM.FCC
C                                                                               
C SUBROUTINE TO SAVE TICKET MARKETING MESSAGES                              
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
C Copyright 1997 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                       
        SUBROUTINE SAVETKM                                                        
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
                                                           
        INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'                                                           
        INCLUDE 'INCLIB:RECTKM.DEF'                                                           

        ! variables
        INTEGER*4  FDB(7)               !
        INTEGER*4  ST                   !
C                                                                               
C                                                                               
        TYPE*,IAM(),'Saving ticket messages'                                            
        CALL OPENW(3,SFNAMES(1,TKTM),4,0,0,ST)                                     
        CALL IOINIT(FDB,3,TKMSEC*256)                                                 
        IF(ST.NE.0) THEN                                                          
            CALL FILERR(SFNAMES(1,TKTM),1,ST,0)                                      
            RETURN                                                                  
        ENDIF                                                                     
C                                                                               
C                                                                               
        CALL FASTMOV(TKTMES(1,1,1),TKMMES(1,1,1),
     *       TICKET_LENGTH*TICKET_ROWS*(MAXGAM+PRM_NUMOPN+NUMCDU))                             

	CALL FASTMOV(TKTMLN(1),TKMMLN(1),MAXGAM+PRM_NUMOPN+NUMCDU)
	CALL FASTMOV(TKTMRV(1),TKMMRV(1),MAXGAM+PRM_NUMOPN+NUMCDU)
        TKMCDU = TKTCDU
C
C                                                                               
        CALL WRITEW(FDB,1,TKMREC,ST)                                              
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,TKTM),3,ST,1)                            
        CALL USRCLOS1(3)                                                             

        RETURN                                                                    

        END                                                                       
