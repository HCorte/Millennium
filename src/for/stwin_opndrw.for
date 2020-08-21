C
C SUBROUTINE STWIN_OPNDRW
C  
C V01 21-MAY-1999 UXN Initial revision.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  
C SUBROUTINE TO OPEN DRAW FILES FOR WINSEL.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE STWIN_OPNDRW(FILE,UNT)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 FILE(5)
	INTEGER*4 UNT, ST
        INTEGER*4   VOL, INPLEN
        CHARACTER*4 CVOL
        EQUIVALENCE (VOL,CVOL)

10	CONTINUE
	CALL USRCLOS1(     UNT)
	CALL OPENW(UNT,FILE,5,0,0,ST)
	IF (ST .EQ. 0) RETURN

	WRITE(5,900) IAM(),FILE,ST
        CALL PRMTEXT('Enter new draw pack volume name: ',CVOL,INPLEN)
        FILE(1) = VOL
	WRITE(5,920) IAM(),FILE(1)
	CALL GPAUSE

	GOTO 10
C
900	FORMAT(1X,A,1X,5A4,' open error> ',I4)
910	FORMAT(A4)
920	FORMAT(1X,A,' Mount ',A4,' pack and continue STWINSEL')

	END
