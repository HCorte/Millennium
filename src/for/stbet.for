C
C SUBROUTINE STBET
C
C V01 21-MAY-1999 UXN Initial release.
C
C Subroutine to build bet image for Super Triple transaction.
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

	SUBROUTINE STBET(TRABUF,BIMAGE)

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
        WRITE(BIMAGE(1),900) '1st:',
     *       (TRABUF(TWSTBET+OFF+I),I=0,TRABUF(TWSTM1)-1)
        OFF=OFF+TRABUF(TWSTM1)
        WRITE(BIMAGE(2),900) '2nd:',
     *       (TRABUF(TWSTBET+OFF+I),I=0,TRABUF(TWSTM2)-1)
        OFF=OFF+TRABUF(TWSTM2)
        WRITE(BIMAGE(3),900) '3rd:',
     *       (TRABUF(TWSTBET+OFF+I),I=0,TRABUF(TWSTM3)-1)

	RETURN

C--------------------------- Format statements. -------------------------

900	FORMAT(A4,16(I3.2))

	END

