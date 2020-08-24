C MULSUBS.FOR
C
C V13 01-MAR-2000 UXN READGFL, CHKWINS, CHKSTS, VLTCNAM, VLTCSET
C                     are now in separate module.
C V12 08-FEB-2000 OXK Reduced hardcoding of gamenums from TSKGAM (Vakio changes)
C V11 16-JAN-2000 UXN SORT_WINSEL added, DISPWIN changed.
C V10 05-JAN-2000 OXK WINYES set as status instead of WINPRV
C V09 27-DEC-1999 OXK NBRTCW added.
C V08 15-DEC-1999 OXK CHKSTS fixed.
C V07 13-DEC-1999 OXK MULTIWIN changes.
C V06 13-OCT-1999 RXK World Tour added.
C V05 27-AUG-1999 RXK Postponed winsels displayed in proper section.
C V04 30-JUN-1999 UXN Fix for CHKWIN. Also Super Triple added.
C V03 25-MAY-1999 RXK Every wintsk has its own VLWnn, run wintsks for cancelled
C                     events as well,fix for postponed draw number(oddset games)
C V02 30-APR-1999 RXK Fix to pick up Pitka draw. Display draw # in DISPWIN.
C                     Get the draw # of an added game in CHKWINS. 
C V01 11-JAN-1999 GPW INITIAL RELEASE FOR FINLAND 
C
C  SUBROUTINES FOR STOPSYS OPTIMISATION
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE STORFIL(NTSK,FILES,FTYPE,FILCNT,MODE,CODE)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER * 4 I
      CHARACTER * (*) FILES(*)
      INTEGER * 4 NTSK,PTR,LGT,LC,FILCNT,FTYPE(*),MODE,CODE
C
      PTR=(NTSK-1)*MAXF1+1
      LC=LEN(FILES(1))
C
      IF(MODE.EQ.1) THEN                    ! COPY TO BUFFER
         IF(FILCNT.EQ.0) RETURN
         FCNT(NTSK)=FILCNT
         LGT=LC*FILCNT
         DO I=1,FILCNT
            CBUFF(PTR+I-1)=FILES(I)
         ENDDO
         IF(CODE.EQ.0) RETURN
         DO I=1,FILCNT
            BUFTYP(PTR+I-1)=FTYPE(I)
         ENDDO
         RETURN
      ENDIF
C         
      IF(MODE.EQ.2) THEN                    ! COPY FROM BUFFER
         FILCNT=FCNT(NTSK)
         IF(FILCNT.EQ.0) RETURN
         LGT=LC*FILCNT
         DO I=1,FILCNT
            FILES(I)=CBUFF(PTR+I-1)
         ENDDO
         IF(CODE.EQ.0) RETURN
         DO I=1,FILCNT
            FTYPE(I)=BUFTYP(PTR+I-1)
         ENDDO
         RETURN
      ENDIF

      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE DISPWIN(MODE)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER*4 MODE
      INTEGER*4 I,J,FLG,WIN, FOUND
C
      IF(MODE.EQ.2) TYPE*,IAM(),'Checking WINSEL statuses'
C
      FOUND=0
      FLG=0
      DO I=1,MAXGAM
	 DO WIN=1,MAX_WINSEL
	    IF(DRWSTS(WIN,I).EQ.WINYES) THEN
              IF(FLG.EQ.0) THEN
                WRITE(*,1000) IAM()
                FLG=1
              ENDIF
              WRITE(*,1001) IAM(),(GLNAMES(J,I),J=1,4),I,DRWGAM(WIN,I)
            ENDIF
	 ENDDO
      ENDDO
      FOUND=FOUND+FLG
C
      IF(MODE.EQ.1) RETURN
C
      FLG=0
      DO I=1,MAXGAM
	 DO WIN=1,MAX_WINSEL
	    IF(DRWSTS(WIN,I).EQ.WINPRV) THEN
              IF(FLG.EQ.0) THEN
                WRITE(*,1002) IAM()
                FLG=1
              ENDIF
              WRITE(*,1001) IAM(),(GLNAMES(J,I),J=1,4),I,DRWGAM(WIN,I)
            ENDIF
	 ENDDO
      ENDDO
      FOUND=FOUND+FLG
C
      FLG=0
      DO I=1,MAXGAM
	 DO WIN=1,MAX_WINSEL
	    IF(DRWSTS(WIN,I).EQ.RESNOT) THEN
              IF(FLG.EQ.0) THEN
                WRITE(*,1003) IAM()
                FLG=1
              ENDIF
              WRITE(*,1001) IAM(),(GLNAMES(J,I),J=1,4),I,DRWGAM(WIN,I)
            ENDIF
	 ENDDO
      ENDDO
      FOUND=FOUND+FLG
C
      FLG=0
      DO I=1,MAXGAM
	 DO WIN=1,MAX_WINSEL
	    IF(DRWSTS(WIN,I).EQ.WINCAN) THEN
              IF(FLG.EQ.0) THEN
                WRITE(*,1004) IAM()
                FLG=1
              ENDIF
              WRITE(*,1001) IAM(),(GLNAMES(J,I),J=1,4),I,DRWGAM(WIN,I)
            ENDIF
	 ENDDO
      ENDDO
      FOUND=FOUND+FLG
C
      IF(MODE.EQ.2) THEN
	 IF(FOUND.EQ.0) THEN
             TYPE*,IAM(),'WINSEL statuses OK'
         ELSE
	     TYPE*,IAM()
	     TYPE*,IAM()
	     TYPE*,IAM(),'Winner selections were postponed or cancelled'
             TYPE*,IAM(),'for the games listed above'
	     TYPE*,IAM()
	     TYPE*,IAM()
	     CALL PRMYESNO('Are you sure you want to continue [Y/N]', FLG)
	     IF(FLG.NE.1) THEN
		TYPE*,IAM()
	        TYPE*,IAM(),'Enter results and/or run MULTIWIN.'
                TYPE*,IAM(),'After MULTIWIN subrun VLFTSK again!'
		TYPE*,IAM()
		CALL GSTOP(GEXIT_OPABORT)
             ENDIF
	 ENDIF
      ENDIF 
C
      RETURN
 1000 FORMAT(/1X,A,' REGULAR WINSEL FOR GAMES :'/)
 1001 FORMAT( 1X,A,2X,4A4,3X,'GNUM=',I3,'  DRAW=',I5)
 1002 FORMAT(/1X,A,' PREVIOUSLY POSTPONED WINSEL FOR GAMES :'/)
 1003 FORMAT(/1X,A,' RESULTS NOT IN - WINSEL WILL BE POSTPONED ',
     *       'OR RUN LATER TODAY FOR GAMES :'/)
 1004 FORMAT(/1X,A,' WINSEL REMOVED OR POSTPONED FOR GAMES:'/)
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      INTEGER * 4 FUNCTION INDTSK(NAME)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:PRMSTOP.DEF'
      INCLUDE 'INCLIB:MULNAM.DEF'
C
      CHARACTER * (*) NAME
      DO 1 INDTSK=1,MAXWTSK
         IF(NAME.EQ.TSKWNAM(INDTSK)) RETURN
    1 CONTINUE
      INDTSK=0
C
      RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      INTEGER*4 FUNCTION NBRTSK(GAMNBR)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER*4 GAMNBR,J,CNT
      NBRTSK=0
      IF (GAMNBR.LT.1.OR.GAMNBR.GT.MAXGAM) RETURN
C
      DO 2 NBRTSK=1,MAXWTSK
         CNT=TSKCNT(NBRTSK)
         IF(CNT.EQ.0) GOTO 2
         DO 1 J=1,CNT
            IF(TSKGAM(J,NBRTSK).EQ.GAMNBR) RETURN
    1    CONTINUE
    2 CONTINUE
C
      RETURN
      END    
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WAITLOCK(POS,BITARY)
      IMPLICIT NONE
C
      LOGICAL * 4 LIB$BBSSI
      INTEGER * 4 POS,BITARY(*)
      INTEGER * 4 TIMDEL,ST
C
      DATA TIMDEL/20/      ! MSEC
C
   10 CONTINUE
      IF(.NOT.LIB$BBSSI(POS,BITARY)) GOTO 20
      CALL XWAIT(TIMDEL,1,ST)   
      IF(ST.EQ.1) CALL FREELOCK(POS,BITARY)
      GOTO 10
   20 CONTINUE
C
      RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE FREELOCK(POS,BITARY)
      IMPLICIT NONE
C
      LOGICAL * 4 LIB$BBCCI,ST
      INTEGER * 4 POS,BITARY
C
      ST=LIB$BBCCI(POS,BITARY)
C
      RETURN
      END
C
C
C++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SETWTSK
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER*4 GAMNUM, I
      INTEGER*4 NBRTSK,NTSK,KTCF
C
      DO I=1,MAXWTSK
           STSTSK(I)=NOWINS
           VLWNBR(I)=0
           TCWNBR(I)=0
      ENDDO
C
      KTCF=0
      WINCNT=0
      DO GAMNUM=1,MAXGAM
         IF(DRWSTS(MLWININD,GAMNUM).EQ.WINYES.OR.
     *	    DRWSTS(MLWININD,GAMNUM).EQ.WINPRV)    THEN
      	      NTSK=NBRTSK(GAMNUM)
      	      IF(STSTSK(NTSK).EQ.NOWINS) THEN
      		   WINCNT=WINCNT+1
      		   STSTSK(NTSK)=TOBWIN
      		   WINWTSK(WINCNT)=NTSK
      		   VLWNBR(NTSK)=NTSK
      		   IF(MRGTYP(NTSK).EQ.MRGALL) THEN
      		       KTCF=KTCF+1
      		       TCWNBR(NTSK)=KTCF
      		   ENDIF
      	      ENDIF
         ENDIF
      ENDDO
C
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE CHKVLTC(ST)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER * 4 ST,I
C
      ST=0
C
      DO 1 I=1,WINCNT
         IF(VLWSTS(I).NE.WCLR) THEN
             ST=I
             RETURN
         ENDIF
    1 CONTINUE
C
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WINGETW(TSKNAME,VLWNAME,TCWNAME)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      CHARACTER * (*) TSKNAME
      INTEGER * 4 VLWNAME(5),TCWNAME(5)
      INTEGER * 4 I,NTSK,NVLW,NTCF
      INTEGER * 4 INDTSK                     !FUNCTION
C

      IF(STOPMOD.EQ.WINMANUAL) THEN
         DO 4 I=1,5
            VLWNAME(I)=SFNAMES(I,VLW)
            TCWNAME(I)=SFNAMES(I,TCW)
    4    CONTINUE
      ELSE
C                                        ! MULTIWINSEL 
         DO 1 I=1,5
            VLWNAME(I)='    '
            TCWNAME(I)='    '
    1    CONTINUE
C
         NTSK=INDTSK(TSKNAME)
         NVLW=NTSK
C
         DO 2 I=1,5
            VLWNAME(I)=VLWNAM(I,NVLW)
    2    CONTINUE
C
         IF(MRGTYP(NTSK).NE.MRGALL) GOTO 5
C
         NTCF=TCWNBR(NTSK)
C
         DO 3 I=1,5
            TCWNAME(I)=TCWNAM(I,NTCF)
    3    CONTINUE
C
C
      ENDIF
C
    5 CONTINUE
C
      RETURN
C
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE WINSEND
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:MULNAM.DEF'
C
      INTEGER * 4 DUM,ST(MAXMVLF),I,NTSK,COUNT
      LOGICAL RUNNING
C
      COUNT=0
      CALL FASTSET(0,ST,MAXMVLF)

   11 CONTINUE
      RUNNING=.FALSE.
      DO 1 I=1,WINCNT
         IF(ST(I).NE.4) THEN
            NTSK=WINWTSK(I)
            CALL STTSK(WINNAM(NTSK),DUM,ST(I))
            IF(ST(I).NE.4) THEN 
               RUNNING=.TRUE.
               IF(MOD(COUNT,100).EQ.0) 
     *            TYPE*,IAM(),CWINNAM(NTSK),' running'
            ELSE
               STSTSK(WINWTSK(I))=WINDON
               IF(MOD(COUNT,100).EQ.0)
     *            TYPE*,IAM(),CWINNAM(NTSK),'  done  '
            ENDIF
         ELSE
            STSTSK(WINWTSK(I))=WINDON
            IF(MOD(COUNT,100).EQ.0)
     *         TYPE*,IAM(),CWINNAM(WINWTSK(I)),'  done  '
         ENDIF
    1 CONTINUE
C
      IF(.NOT.RUNNING) RETURN

      CALL XWAIT(2,2,ST)
      COUNT=COUNT+1
      GOTO 11
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE PRTTEST                  !%%% FOR TEST ONLY
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:MULNAM.DEF'
C
      CHARACTER * 20 FILES(200)
      INTEGER * 4 FTYPE(200)
      INTEGER * 4 I,J,M,CNT,K,KTCF
C
      TYPE*,' '
      TYPE*,'+++++++++++ P R T T E S T +++++++++++++++'
      TYPE*,' '
      TYPE*,' Wintasks for today :'
      TYPE*,' '
      KTCF=0
      DO 1 I=1,WINCNT
          TYPE*,' '
          M=WINWTSK(I)
          WRITE(*,2000) I,TSKWNAM(M),M
          WRITE(*,1000) (VLWNAM(J,M),J=1,5)
          KTCF=TCWNBR(M)
          IF(KTCF.EQ.0) GOTO 11
          WRITE(*,1000) (TCWNAM(J,KTCF),J=1,5)
   11     CONTINUE
C
          TYPE*,' '
          CALL STORFIL(M,FILES,FTYPE,CNT,2,1)
          IF(CNT.EQ.0) GOTO 1
          DO 2 K=1,CNT
               WRITE(*,3000) FILES(K)
    2     CONTINUE
C
    1 CONTINUE
      TYPE*,' '
      TYPE*,' '
      TYPE*,'+++++++++++ P R T  E N D  +++++++++++++++'
      TYPE*,' '
      RETURN
 1000 FORMAT(1X,20X,5A4)
 2000 FORMAT(1X,I3,3X,A8,' # ',I2.2)
 3000 FORMAT(1X,20X,A20)
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      INTEGER*4 FUNCTION NBRTCW(WINS)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER*4 WINS,NTSK

      NTSK=WINWTSK(WINS)
      NBRTCW=TCWNBR(NTSK)

      RETURN
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE SORT_WINSEL
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
C
      INTEGER*4 I, J, TEMP
C
      DO I=1, MAXGAM
        DO J=1, MAX_WINSEL-1
	  IF(DRWSTS(J+1,I).EQ.WINYES .AND. DRWSTS(J,I).NE.WINYES) THEN
	       TEMP = DRWSTS(J,I)
	       DRWSTS(J,I) = DRWSTS(J+1,I)
	       DRWSTS(J+1,I) = TEMP

	       TEMP = DRWGAM(J,I)
	       DRWGAM(J,I) = DRWGAM(J+1,I)
	       DRWGAM(J+1,I) = TEMP
	  ENDIF
        ENDDO
      ENDDO 
      END
