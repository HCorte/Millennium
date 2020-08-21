C  GXSRC:ENC_DESSTOP.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENC_DESSTOP.FOV  $
C  
C     Rev 1.2   11 Feb 1998 18:47:32   NXA
C  Disconnect comms with DES board and set ASTSTOP to True [RFC 2066]
C  
C     Rev 1.1   11 Feb 1998 18:45:26   NXA
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    11 Feb 1998 18:45:06   NXA
C  Initial revision.
C  
C V01 13-MAR-03 GPW DESENCR TAKEN FROM UK
C
C This routine will reset comms to the DES board
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE DESSTOP
      IMPLICIT NONE
C
      INCLUDE   'INCLIB:SYSPARAM.DEF'
      INCLUDE   'INCLIB:SYSEXTRN.DEF'
      INCLUDE   'INCLIB:GLOBAL.DEF'
      INCLUDE   'INCLIB:PROCOM.DEF'
      INCLUDE   'INCLIB:DESPARAMS.DEF'          ! General parameters
      INCLUDE   'INCLIB:DESCOM.DEF'             ! Control common
C
      INTEGER*4 STATUS                          ! VMS return status
C
C Kill the AST
C
      ASTSTOP = .TRUE.
C
C Tell DES to stop processing
C
      CALL DESEND ( STATUS )
      IF ( .NOT. STATUS ) THEN
        TYPE *, IAM(), 'Error from DESEND = ', STATUS
        CALL LIB$SIGNAL ( %VAL(STATUS) )
      ENDIF
C
C Deassign the driver and mailbox channels
C
      CALL DESDASSGN ( STATUS )
      IF ( .NOT. STATUS ) THEN
        TYPE *, IAM(), 'Error from DESASSIGN = ', STATUS
        CALL LIB$SIGNAL ( %VAL(STATUS) )
      ENDIF
C
      RETURN
      END
