C
C SUBROUTINE TRBET
C
C V02 31-MAY-1999 UXN MAXTRPRW added.
C V01 12-JAN-1998 RXK Initial release.
C
C Subroutine to build bet image for Todays Triple transaction.
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

	SUBROUTINE TRBET(TRABUF,BIMAGE)

	IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local Variables.

        CHARACTER*56 BIMAGE(12)

	INTEGER*4   I
	INTEGER*4   OFF

C------------------------- Start of Code --------------------------------

        OFF=0
        WRITE(BIMAGE(1),900) 'A:',
     *       (TRABUF(TWTTBET+OFF+I),I=0,TRABUF(TWTTMA)-1)
        OFF=OFF+TRABUF(TWTTMA)
        WRITE(BIMAGE(2),900) 'B:',
     *       (TRABUF(TWTTBET+OFF+I),I=0,TRABUF(TWTTMB)-1)
        OFF=OFF+TRABUF(TWTTMB)
        WRITE(BIMAGE(3),900) 'C:',
     *       (TRABUF(TWTTBET+OFF+I),I=0,TRABUF(TWTTMC)-1)

	RETURN

C--------------------------- Format statements. -------------------------

900	FORMAT(A2,<MAXTRPRW>(I3.2))

	END

