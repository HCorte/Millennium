C SUBROUTINE TGLENT
C
C V01 01-DEC-2000 UXN Initial release.
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF TotoGolo RESULTS
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
	SUBROUTINE TGLENT(GNUM,GIND,DRAW)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
	INTEGER*4  GNUM              !
	INTEGER*4  GIND              !
	INTEGER*4  DRAW              !

        ! variables
	INTEGER*4  FDB(7)            !
	INTEGER*4  Y                 !
	INTEGER*4  EXT               !
	INTEGER*4  I,J,K,TEMP
	INTEGER*4  ST                !
	INTEGER*4  OFF               !
	INTEGER*4  CBUF(CDLEN)       !
	CHARACTER*1 VAL(0:3)/'0','1','2','M'/
	CHARACTER*50 STRING
C
C
	IF(ONLINE) THEN
	    CALL GAMLOG(TTGL,GIND,DTGREC,TGLBLK)
	ELSE
	    CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DTGSEC*256)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

	    CALL READW(FDB,DRAW,DTGREC,ST)
	    IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	ENDIF
C
C
	IF(DTGSTS.NE.GAMBFD) THEN
	    WRITE(6,900) GTNAMES(TTGL),GIND,DTGSTS
	    CALL GPAUSE
	ENDIF
C
C ENTER WINNING RESULTS
C
20	CONTINUE
	DO I=1,DTGMAX
	    WRITE(6,902) IAM(),I,(DTGNMS(K,1,I),K=1,TGNMS_LEN / 4),
     *                           (DTGNMS(K,2,I),K=1,TGNMS_LEN / 4)
	    DO J=1,2
	       WRITE(STRING,903) (DTGNMS(K,J,I),K=1,TGNMS_LEN / 4)
	       CALL INPNUM(STRING,DTGWIN(J,I),0,2,EXT)
	       IF(EXT.EQ.-3) THEN
	          DTGWIN(J,I) = 3
	       ELSEIF(EXT.LT.0) THEN
		  GOTO 20
	       ENDIF
            ENDDO
        END DO

	WRITE(6,901) (GLNAMES(K,GNUM),K=1,4)
	DO I=1,DTGMAX
	    WRITE(6,904) I,(DTGNMS(K,1,I),K=1,TGNMS_LEN / 4),
     *                     (DTGNMS(K,2,I),K=1,TGNMS_LEN / 4),
     *                      VAL(DTGWIN(1,I)),
     *                      VAL(DTGWIN(2,I))
        ENDDO
	CALL INPYESNO('Are the results entered ok [Y/N] ?',Y)
	IF(Y.NE.1) GOTO 20
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL
C
	DTGSTS=GAMEN1
	OPDONE=1
40	CONTINUE
	TYPE*,' Waiting for verification from remote terminal'
	IF(DTGSTS.EQ.GAMENV) THEN
	    IF(ONLINE) THEN
                CALL FASTSET(0,CBUF,CDLEN)
                CBUF(1)=2
                CBUF(2)=0
                CBUF(3)=TCTGL
                CBUF(6)='SYS '
                CBUF(8)=GIND
                OFF = 0
                DO I=1,DTGMAX
		    TEMP = IOR(ISHFT(DTGWIN(1,I),4),DTGWIN(2,I))
                    CALL ISBYTE(TEMP,CBUF(9),OFF)
                    OFF=OFF+1
                END DO
                CALL RESCMD(CBUF)

               CALL FASTSET(0,CBUF,CDLEN)
               CBUF(1)=1
               CBUF(2)=GAMENV
               CBUF(3)=TCTGL
               CBUF(6)='SYS '
               CBUF(8)=GIND
               CALL RESCMD(CBUF)
	    ELSE
	       CALL WRITEW(FDB,DRAW,DTGREC,ST)
	       IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	       CALL CLOSEFIL(FDB)
	       CALL TGRESULT(GIND,DRAW)
	    ENDIF

	    RETURN
	ENDIF

	CALL XWAIT(5,2,ST)
	IF(DTGSTS.EQ.GAMBFD) THEN
	    TYPE*,'Verification error, please re-enter '
	    GOTO 20
	ENDIF
	GOTO 40
C
900	FORMAT(1X,A8,I1,' invalid game status> ',I4)
901	FORMAT(1X,4A4,' results entered:')
902     FORMAT(1X,A,I1,'. match ',<TGNMS_LEN/4>A4,' - ',<TGNMS_LEN/4>A4)
903     FORMAT('Enter score for ',<TGNMS_LEN/4>A4,' [0/1/2/M]')
904     FORMAT(1X,I1,'. match ',<TGNMS_LEN/4>A4,' - ', <TGNMS_LEN/4>A4,
     *         10X,A1,' : ',A1)
	END
