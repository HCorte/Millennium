C SUBROUTINE TGLVER
C
C V01 01-DEC-2000 UXN INITIAL RELEASE.
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF TOTOGOLO RESULTS
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TGLVER(GNUM,GIND,DRAW,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C

        ! arguments
	INTEGER*4  GNUM                    !
	INTEGER*4  GIND                    !
	INTEGER*4  DRAW                    !
	INTEGER*4  ST                      !

        ! variables
	INTEGER*4  Y                       !
	INTEGER*4  EXT                     !
	INTEGER*4  I,J,K
	CHARACTER*50 STRING
	CHARACTER*1 VAL(0:3)/'0','1','2','M'/
C
C ENTER WINNING RESULTS
C
20	CONTINUE
        DO I=1,DTGMAX
            WRITE(6,902) IAM(),I,(DTGNMS(K,1,I),K=1,TGNMS_LEN / 4),
     *                           (DTGNMS(K,2,I),K=1,TGNMS_LEN / 4)
            DO J=1,2
               WRITE(STRING,903) (DTGNMS(K,J,I),K=1,TGNMS_LEN / 4)
               CALL INPNUM(STRING,DTGHLD(J,I),0,2,EXT)
               IF(EXT.EQ.-3) THEN
                  DTGHLD(J,I) = 3
               ELSEIF(EXT.LT.0) THEN
                  GOTO 20
               ENDIF
            ENDDO
        END DO

        WRITE(6,901) (GLNAMES(K,GNUM),K=1,4)
        DO I=1,DTGMAX
            WRITE(6,904) I,(DTGNMS(K,1,I),K=1,TGNMS_LEN / 4),
     *                     (DTGNMS(K,2,I),K=1,TGNMS_LEN / 4),
     *                      VAL(DTGHLD(1,I)),
     *                      VAL(DTGHLD(2,I))
        ENDDO
        CALL INPYESNO('Are the results entered ok [Y/N] ?',Y)
        IF(Y.NE.1) GOTO 20
C
C VERIFY AGAINST OPERATOR ENTRY
C
	DO I=1,DTGMAX
	  DO J=1,2
	    IF(DTGWIN(J,I).NE.DTGHLD(J,I)) THEN
	        TYPE*,'Verification error, please re-enter'
	        OPDONE=0
	        DTGSTS=GAMBFD
	        ST=-1
	        RETURN
	    ENDIF
          ENDDO
        END DO
C
C
	ST=0
	DTGSTS=GAMENV

	RETURN
901     FORMAT(1X,4A4,' results entered:')
902     FORMAT(1X,A,I1,'. match ',<TGNMS_LEN/4>A4,' - ',<TGNMS_LEN/4>A4)
903     FORMAT('Enter score for ',<TGNMS_LEN/4>A4,' [0/1/2/M]')
904     FORMAT(1X,I1,'. match ',<TGNMS_LEN/4>A4,' - ', <TGNMS_LEN/4>A4,
     *         10X,A1,' : ',A1)

	END
