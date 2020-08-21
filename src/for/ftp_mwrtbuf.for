C  GXSRC:FTP_MWRTBUF.FOR
C
C V02 12-JUN-2000 UXN FTP_TEST removed.
C V01 28-NOV-1996 WXW Initial revision.
C  
C FTP_MWRTBUF.FTN
C
C SUBROUTINE TO WRITE A BUFFER TO FTP FILE                          
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
C=======OPTIONS/CHECK/EXT
      SUBROUTINE FTP_MWRTBUF(UNIT)
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'                                          
      INCLUDE 'INCLIB:SYSEXTRN.DEF'                                          
      INCLUDE 'INCLIB:GLOBAL.DEF'                                            
      INCLUDE 'INCLIB:FTP.DEF'

      INTEGER*4 TSTREC   /0/
      INTEGER*4 UNIT

C WRITE TO FILE                                                 

      WRITE(UNIT,9000) IBMBUF
      TSTREC=TSTREC+1                                                        

C CLEAR BUFFER AND RESET INDEX
C ----------------------------
      CALL FASTSET(0,IBUF,BYTLEN/4)
      RETURN                                                                    
9000  FORMAT(A100)
      END                                                                       
