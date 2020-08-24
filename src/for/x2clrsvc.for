C
C SUBROUTINE X2CLRSVC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLRSVC.FOV                                 $
C  $Date::   17 Apr 1996 16:14:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C X2CLRSVC.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2CLRSVC(PROBUF)
C
C     PURPOSE:
C        BUILD FRONT END MESSAGE TO CLEAR SVC
C
C     INPUT:
C       PROBUF       -     BUFFER NUMBER
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE X2CLRSVC(PROBUF)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:X2FEMES.DEF'
C
        INTEGER*4 PROBUF
C
        HPRO(MSGNUM,PROBUF) = 0
        HPRO(INPLEN,PROBUF) = 0        
        HPRO(X2X_DEST,PROBUF) = X2DEST_FE+X2DEST_STATION
     *                                   +X2DEST_TRANSPORT
        HPRO(X2X_CONNCTL_OVR,PROBUF) =
     *                          ( X2FEMES_UNCDDISC )*256
C
        RETURN
        END
