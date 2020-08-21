C
C SUBROUTINE STR_CANROW
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE STR_CANROW(ROW,N)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local variables.

	INTEGER*4 ROW, K, FLAG
	INTEGER*4 N		! N=1 IF STRENT IS CALLING ROUTINE
				! N=2 IF STRVER IS CALLING ROUTINE
			        ! NOTE THAT THIS IS ALSO USED AS AN INDEX
				! INTO THE STROWSTS ARRAY


C----------------------- Start of code ------------------------------

C---- Chack to see if event is cancelled.

	IF (N .EQ. 1) THEN
	  IF (STROWSTS(ROW,N) .EQ. GAMCAN .OR. 
     *	      STROWSTS(ROW,N) .EQ. GAMREF) THEN
	    WRITE(5,901) IAM(),ROW,(DSTNMS(K,ROW),K=1,3)
	    RETURN
	  END IF
	ELSE IF (N .EQ. 2) THEN
	  IF (STROWSTS(ROW,N) .EQ. GAMCAN) THEN
	    WRITE(5,901) IAM(),ROW,(DSTNMS(K,ROW),K=1,3)
	    RETURN
	  END IF
	END IF

C---- Check if row active.

	IF(STROWSTS(ROW,N).EQ.GAMNUL) THEN
	  TYPE*,IAM(),' Row ',ROW,' not active for this event'
	  RETURN
	ENDIF

C---- Cancel row.

	WRITE(5,902) IAM(),ROW,(DSTNMS(K,ROW),K=1,3)

	CALL WIMG(5,'Is this correct (Y/N) ')

	CALL YESNO(FLAG)

	IF(FLAG.NE.1) RETURN

	STROWSTS(ROW,N)=GAMCAN

C------------------------------ Format statements ------------------------

901	FORMAT(1X,A,' Row ',I2,1X,3A4,' Already cancelled')

902	FORMAT(1X,A,' Cancellation of row ',I4,1X,3A4)

	RETURN
	END
