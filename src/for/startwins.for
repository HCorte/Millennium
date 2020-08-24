C 
C
C V02 28-DEC-1999 OXK NBRTCW added.
C V01 20-DEC-1999 OXK Initial release
C
C   Clear VLW- & TCW-files & run WINSELs when OK
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
      SUBROUTINE STARTWINS

      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:MULNAM.DEF'

      INTEGER*4    I,K,ST,NTSK,NTCW
      CHARACTER*20 FILNAM

      INTEGER*4 NBRTCW
      EXTERNAL  NBRTCW

10    CONTINUE
      DO 20 I=1,WINCNT
          NTSK=WINWTSK(I)
          IF(FCNT(NTSK).EQ.0) GOTO 20
          WRITE(*,1000) IAM(),WINNAM(NTSK)
C
C Check VLW-file status, clear if possible & check result
C
          IF (VLWSTS(NTSK).NE.WCLR) THEN
            FILNAM(1:20)=' '
            WRITE(FILNAM,1100) (VLWNAM(K,NTSK),K=1,5)
            CALL SUBCLRFIL(1,FILNAM,ST)
            IF(ST.NE.0) THEN
               WRITE(6,1110) IAM(),(VLWNAM(K,NTSK),K=1,5)
               CALL GPAUSE
               GOTO 10
            ELSE
               VLWSTS(NTSK)=WCLR
            ENDIF
          ENDIF
C
C If necessary check TCW-file status, clear if possible & check result
C
          IF (MRGTYP(NTSK).EQ.MRGALL) THEN
	      NTCW=NBRTCW(I)
	      IF (NTCW.NE.0) THEN
      	        IF (TCWSTS(NTCW).NE.WCLR) THEN
      	  	  FILNAM(1:20)=' '
      		  WRITE(FILNAM,1100) (TCWNAM(K,NTCW),K=1,5)
      		  CALL SUBCLRFIL(1,FILNAM,ST)
      		  IF(ST.NE.0) THEN
      		     WRITE(6,1110) IAM(),(TCWNAM(K,NTCW),K=1,5)
      		     CALL GPAUSE
      		     GOTO 10
      		  ELSE
      		     TCWSTS(NTCW)=WCLR
      		  ENDIF
                ENDIF
	      ENDIF
	  ENDIF
C
C Start WINSEL
C
          CALL NRUNTSK(WINNAM(NTSK))
20	CONTINUE

	RETURN
1000  FORMAT(1X,A,A8,'  started.')
1100  FORMAT(5A4)
1110  FORMAT(1X,A,' Error while clearing ',5A4)
	END
