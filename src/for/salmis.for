C
C SUBROUTINE SALMIS
C
C SALMIS.FOR
C
C V01 15-JAN-93 HJK INITIAL RELEASE FOR FINLAND
C
C
C MISCELLANEOUS TERMINAL REPORT SUBROUTINE
C
C CALLING SEQUENCE:
C     CALL SALMIS(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C INPUT
C     SUBCLASS - REPORT SUBCLASS
C     MANAGER  - MANAGER FLAG
C
C OUTPUT
C     SALES    - SALES INFO FOR REPORT
C     SALOFF   - TOTAL WORDS USED
C     CDC      - CDC OF REPORT
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SALMIS(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'


        ! arguments
	INTEGER*4   SALES(*)       !
	INTEGER*4   SUBCLASS       !
	INTEGER*4   SALOFF         !
	INTEGER*4   CDC            !
	INTEGER*4   RTER           !

	LOGICAL	    MANAGER        !

        ! variables
	INTEGER*4   CLERK          !
	INTEGER*4   I              !
	INTEGER*4   J              !
	INTEGER*4   OFF            !
	INTEGER*4   TMPCNT	   ! TEMP. TOTAL COUNT, UNTIL INDEX COMPUTED
	INTEGER*4   TMPAMT(2)	   ! TEMP. TOTAL AMOUNT UNTIL INDEX COMPUTED
        INTEGER*4   SALIND         !


C
C SET / CLEAR VARIABLES
C
	TMPCNT = 0
	TMPAMT(1) = 0
	TMPAMT(2) = 0
        OFF=PRM_NUMINS*2   !OFFSET INTO AGTMIS TABLE FOR MISC ITEMS
	

C
C PROCESS ONLY MISCELLANEOUS REPORT
C
	IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN  !TODAY OR W. T. D.
            SALIND=1
	    DO 200 I=OFF+1,OFF+PRM_NUMISAL
                SALES(SALIND)=SALES(SALIND)+AGTMIS(I,1,RTER)
	        TMPCNT=TMPCNT+AGTMIS(I,1,RTER)
	        SALES(SALIND+1)=SALES(SALIND+1)+
     *                          AGTMIS(I,2,RTER)
                CALL ADDI8I4(TMPAMT,AGTMIS(I,2,RTER),BETUNIT)
                SALIND=SALIND+2
200	    CONTINUE
	    CDC=DAYCDC
	ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
	IF(MANAGER)  THEN   !GET CLERKS ACCOUNTS ALSO
	    DO 220 CLERK=2,8
	        IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 220
	        SALIND=1
                DO 210 I=OFF+1,OFF+PRM_NUMISAL
                    SALES(SALIND)=SALES(SALIND)+
     *                            CLRKMIS(I,1,CLERK)
	            TMPCNT=TMPCNT+CLRKMIS(I,1,CLERK)
                    SALES(SALIND+1)=SALES(SALIND+1)+
     *                              CLRKMIS(I,2,CLERK)
                    CALL ADDI8I4(TMPAMT,
     *                           CLRKMIS(I,2,CLERK),
     *                           BETUNIT)
	            SALIND=SALIND+2
210	        CONTINUE
220	    CONTINUE
	ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
	IF(SUBCLASS.EQ.8) THEN
	    DO 240 I= 1,9
	        IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 240
                SALIND=1
	        DO 230 J=OFF+1,OFF+PRM_NUMISAL
	            SALES(SALIND)=SALES(SALIND)+
     *                            ASFMIS(J,1,I)
	            TMPCNT=TMPCNT+ASFMIS(J,1,I)
	            SALES(SALIND+1)=SALES(SALIND+1)+
     *                              ASFMIS(J,2,I)
                    CALL ADDI8I4(TMPAMT,ASFMIS(J,2,I),BETUNIT)
                    SALIND=SALIND+2
230	        CONTINUE
240	    CONTINUE
	    CDC=DAYCDC
	ENDIF
C
C PROCESS FOR DAY REQUESTED
C
	IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
	   DO I= 1,9
	      IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 260
           END DO
	   TRABUF(TERR)=INVL
	   GOTO 8000
260	   CONTINUE
           SALIND=1
	   DO 270 J=OFF+1,OFF+PRM_NUMISAL
	       SALES(SALIND)=SALES(SALIND)+
     *                       ASFMIS(J,1,I)
	       SALES(SALIND+1)=SALES(SALIND+1)+
     *                         ASFMIS(J,2,I)
	       TMPCNT=TMPCNT+ASFMIS(J,1,I)
               CALL ADDI8I4(TMPAMT,ASFMIS(J,2,I),BETUNIT)
               SALIND=SALIND+2
270	   CONTINUE
	   CDC=ASFDAT(ASFCDC,I)
        ENDIF


C
C BUILD REPORT BACK TO TERMINAL
C 
	SALES(SALIND)   = TMPCNT
	SALES(SALIND+1) = TMPAMT(1)
	SALES(SALIND+2) = TMPAMT(2)

	SALIND = SALIND + 2
	SALOFF = SALIND
8000	CONTINUE

	RETURN

	END
