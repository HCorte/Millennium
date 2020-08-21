C
C SUBROUTINE SALCOM
C
C SALCOM.FOR
C
C V18 27-MAR-2017 HXK Omit Joker details if Joker removed > 90 days
C V17 01-JUL-2015 SCML Correction of SALCOM for Portugal
C V16 02-JAN-2011 HXK ADDED LOTTO3 AND LOTTO4 AS ONE GAME (TOTOLOTO)
C V15 21-MAY-2010 RXK Changes for ePassive.
C V14 14-MAY-1999 UXN Super Triple added. Code cleaned up...
C V13 23-NOV-1995 PXB Couple and Double games added
C V12 05-MAY-1995 HXK V5 entered into database again!!!!
C V11 02-MAR-1995 HXK Changed commission calculation for V5
C V10 22-JAN-1994 HXK Corrected totals calculation.
C V09 22-JAN-1994 HXK INITIALISE WORK_COM.
C V08 22-JAN-1994 HXK SUMMED TSCR,TWIT GAMES.
C V07 17-AUG-1993 SXH Fixed bug with TOTAL
C V06 16-AUG-1993 SXH Debugged
C V05 21-JUN-1993 HXK CHANGED FOR FINLAND VAX CONVERSION
C V04 10-JUN-1993 HXK sorted agtinf, agtcom.
C V03 10-FEB-1993 EBD Changed dimension of sales array to *, so calling 
C                     routine dimensions the array
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 18-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C CALLING SEQUENCE:
C     CALL SALCOM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C----+------------------------------------------------------------------
C V17| Correction of SALCOM for Portugal
C----+------------------------------------------------------------------
C	SUBROUTINE SALCOM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        SUBROUTINE SALCOM_OLD(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
C----+------------------------------------------------------------------
C V17| Correction of SALCOM for Portugal
C----+------------------------------------------------------------------
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C

        ! arguments
	INTEGER*4  SALES(*)         !
        INTEGER*4  SUBCLASS         !
        INTEGER*4  SALOFF           !
        INTEGER*4  CDC              !
        INTEGER*4  RTER             !

	LOGICAL    MANAGER          !

        ! variables                 
	INTEGER*4  SALIND           !
        INTEGER*4  GNUM             !
        INTEGER*4  GTYP             !
        INTEGER*4  COMAMT(2)        !
	INTEGER*4  CLERK            !
        INTEGER*4  I                !
	INTEGER*4  TOTAMT(2)/0,0/   !
        INTEGER*4  GIND             !
        INTEGER*4  DUMMY            !
        INTEGER*4  SALIDX           ! TEMPORAL SALES INDEX
	INTEGER*4  TMP_GNUM         ! used to add Lotto4 cnt/amt to Lotto4
C
        INTEGER*4   WORK_COM(2)          !WORK VARIABLE FOR COMMISSIONS
C
	INTEGER*4  IDX(MAXGAM)
C
        ! functions
        LOGICAL    NO_JOK_DET
        EXTERNAL   NO_JOK_DET
C
C SET / CLEAR VARIABLES
C
	TOTAMT(1) = 0
	TOTAMT(2) = 0

        SALIND        = 1

C
C GET COMMISSION REPORTS
C
	IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN
	    CALL FASTSET(0,IDX,MAXGAM)
 	    DO 800 GNUM=1,MAXGAM
	        GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 800
		IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 800                
                IF(GTYP.EQ.TINS) GOTO 800
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 800

	        IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
		    IDX(GNUM) = SALIND
		    SALIND = SALIND + 2
		  ENDIF
	          TMP_GNUM = GNUM
	        ELSE
	          TMP_GNUM = 6
	        ENDIF

                WORK_COM(1)=0
                WORK_COM(2)=0
                DUMMY=0                
	        CALL GETCOM(AGTGAM(GSAMT,GNUM,RTER)-
     *                      AGTGAM(GCAMT,GNUM,RTER),
     *                      TWAG,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      GTYP,
     *                      GIND,
     *                      RTER)
	        CALL GETCOM(AGTGAM(GVAMT,GNUM,RTER),
     *                      TVAL,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
	        IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
	            WORK_COM(1)=0
	            WORK_COM(2)=0
	        ENDIF
                CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
	        CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
800	    CONTINUE
	    CDC=DAYCDC
	ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
	IF(MANAGER.AND.P(CLRKACT).EQ.0) THEN
	    DO 820 CLERK=2,8
	        IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 820
	        SALIND = 1
	        CALL FASTSET(0,IDX,MAXGAM)
	        DO 810 GNUM=1,MAXGAM
	            GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)

	            IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 810
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 810
                    IF(GTYP.EQ.TINS) GOTO 810
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 810

	            IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
		        IDX(GNUM) = SALIND
		        SALIND = SALIND + 2
		      ENDIF
	              TMP_GNUM = GNUM
	            ELSE
	              TMP_GNUM = 6
	            ENDIF

                    WORK_COM(1)=0
                    WORK_COM(2)=0
                    DUMMY=0
	            CALL GETCOM(CLRKDAY(GSAMT,GNUM,CLERK)-
     *                          CLRKDAY(GCAMT,GNUM,CLERK),
     *                          TWAG,
     *                          GNUM,
     *                          COMAMT,
     *                          WORK_COM,
     *                          GTYP,GIND,RTER)
	            CALL GETCOM(CLRKDAY(GVAMT,GNUM,CLERK),
     *                          TVAL,
     *                          GNUM,
     *                          COMAMT,
     *                          WORK_COM,
     *                          DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
	            IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
	                WORK_COM(1)=0
	                WORK_COM(2)=0
	            ENDIF

                    CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
	            CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
810	        CONTINUE
820	    CONTINUE
	ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
	IF(SUBCLASS.EQ.8) THEN
	    DO 840 I=1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 840
	        SALIND = 1
	        CALL FASTSET(0,IDX,MAXGAM)
	        DO 830 GNUM=1,MAXGAM
	            GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)
	            IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 830
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 830
                    IF(GTYP.EQ.TINS) GOTO 830
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 830

	            IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
		        IDX(GNUM) = SALIND
		        SALIND = SALIND + 2
		      ENDIF
	              TMP_GNUM = GNUM
	            ELSE
	              TMP_GNUM = 6
	            ENDIF

                    WORK_COM(1)=0
                    WORK_COM(2)=0
                    DUMMY=0
	            CALL GETCOM(ASFDAY(GSAMT,GNUM,I)-
     *                      ASFDAY(GCAMT,GNUM,I),
     *                      TWAG,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      GTYP,GIND,RTER)
	            CALL GETCOM(ASFDAY(GVAMT,GNUM,I),
     *                      TVAL,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
	            IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
	                WORK_COM(1)=0
	                WORK_COM(2)=0
	            ENDIF

                    CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
	            CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
830	        CONTINUE
840	    CONTINUE
	    CDC=DAYCDC
	ENDIF


C
C PROCESS FOR DAY REQUESTED
C
	IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
	    DO I=1,9
	      IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 860
            END DO
	    TRABUF(TERR)=INVL
	    GOTO 8000
860	    CONTINUE
	    SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
	    DO 870 GNUM=1,MAXGAM
	        GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
	        IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 870   !IF GAME NON EXISTENT
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 870
                IF(GTYP.EQ.TINS) GOTO 870
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 870

	        IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
		    IDX(GNUM) = SALIND
		    SALIND = SALIND + 2
		  ENDIF
	          TMP_GNUM= GNUM
	        ELSE
	          TMP_GNUM = 6
	        ENDIF

                WORK_COM(1)=0
                WORK_COM(2)=0
                DUMMY=0
	        CALL GETCOM(ASFDAY(GSAMT,GNUM,I)-
     *                      ASFDAY(GCAMT,GNUM,I),
     *                      TWAG,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      GTYP,GIND,RTER)
	        CALL GETCOM(ASFDAY(GVAMT,GNUM,I),
     *                      TVAL,
     *                      GNUM,
     *                      COMAMT,
     *                      WORK_COM,
     *                      DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
	        IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM)) THEN	
	            WORK_COM(1)=0
	            WORK_COM(2)=0
	        ENDIF

                CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
	        CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
870	    CONTINUE
	    CDC=ASFDAT(ASFCDC,I)
	ENDIF
C
C COPY TOTALS TO SALES BUFFER
C
        CALL FASTMOV(TOTAMT, SALES(SALIND), 2)
	SALOFF = SALIND + 1
C
C WE HAVE TO SEND SALES IN LITLE ENDIAN
C
        DO  SALIDX = 1, SALOFF, 2
          I = SALES(SALIDX)
          SALES(SALIDX) = SALES(SALIDX + 1) 
          SALES(SALIDX + 1) = I 
        ENDDO        
C
C BUILD REPORT BACK TO TERMINAL
C
C	SALES(SALIND) = TOTAMT(2)
C	SALES(SALIND+1) = TOTAMT(1)
8000	CONTINUE

	RETURN

	END

C----+------------------------------------------------------------------
C V17| Correction of SALCOM for Portugal
C----+------------------------------------------------------------------
        SUBROUTINE I8CAL_COMMISSION(I8RESULT,I4AMOUNT,I4COMMISSION)
        IMPLICIT NONE
        
        INTEGER*8 I8RESULT
        INTEGER*4 I4AMOUNT, I4COMMISSION
        
        INTEGER*8 I8AMOUNT, I8COMMISSION

        INTEGER*8 I8ZERO
        PARAMETER(I8ZERO = KZEXT(0))
        
        INTEGER*8 I8DIV_UNIT
        PARAMETER(I8DIV_UNIT = KZEXT(100000))
        
        INTEGER*8 I8ROUND_UNIT
        PARAMETER(I8ROUND_UNIT = I8DIV_UNIT/KZEXT(2))
        
        CALL I4TOI8(I4AMOUNT,I8AMOUNT)
        CALL I4TOI8(I4COMMISSION,I8COMMISSION)
        
        I8RESULT = I8AMOUNT * I8COMMISSION
        
        IF(I8RESULT .LT. I8ZERO) THEN
            I8RESULT = (I8RESULT + I8ROUND_UNIT) / I8DIV_UNIT
        ELSE
            I8RESULT = (I8RESULT + I8ROUND_UNIT) / I8DIV_UNIT
        ENDIF
        
        RETURN
        END


        SUBROUTINE I8GETCOM(AMOUNT,TYPE,GAME,COMAMT,TOTCOM,GTYP,GIND,TER)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'

        ! Parameters
        INTEGER*4 AMOUNT, TYPE   , GAME
        INTEGER*8 COMAMT, TOTCOM
        INTEGER*4 GTYP  , GIND   , TER
        ! Auxiliary variables
        INTEGER*8 I8AMOUNT, I8COMGAM, I8COMTYP, I8HVCRAT, I8TEMP
        INTEGER*4 TEMP, UNIT
        
        COMAMT   = KZEXT(0)
        I8AMOUNT = KZEXT(0)
        I8COMGAM = KZEXT(0)
        I8COMTYP = KZEXT(0)
        I8HVCRAT = KZEXT(0)
        I8TEMP   = KZEXT(0)
C
C SALES COMMISSION (BY GAME)
C
        IF(TYPE .EQ. TWAG) THEN
          IF(GTYP .NE. 0 .AND. GIND .NE. 0 .AND. TER .NE. 0) THEN
C            TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_BETUNIT)*
C    *                      CALPER(COMGAM(GAME)))
             ! call to CALPER simply divides a number by 100000, so this
             ! is the equivalent in integer arithmetics
             ! call to IDNINT simply converts a floating point number into
             ! the nearest integer
C            COMAMT(1)=TEMP/DYN_BETUNIT
C            COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
             CALL I8CAL_COMMISSION(COMAMT, AMOUNT, COMGAM(GAME))

C            TOTCOM(1)=TOTCOM(1)+COMAMT(1)
C            TOTCOM(2)=TOTCOM(2)+COMAMT(2)
             TOTCOM = TOTCOM + COMAMT
             
             ! this next step is not necessary with integer arithmetics:
             ! calculus of carry over 
C            TEMP=TOTCOM(2)/DYN_BETUNIT
C            TOTCOM(1)=TOTCOM(1)+TEMP
C            TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
             RETURN
          ELSE
C            TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_BETUNIT)*
C    *                   CALPER(COMGAM(GAME)))
             ! call to CALPER simply divides a number by 100000, so this
             ! is the equivalent in integer arithmetics
             ! call to IDNINT simply converts a floating point number into
             ! the nearest integer
                
C            COMAMT(1)=TEMP/DYN_BETUNIT
C            COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
             CALL I8CAL_COMMISSION(COMAMT, AMOUNT, COMGAM(GAME))

C            TOTCOM(1)=TOTCOM(1)+COMAMT(1)
C            TOTCOM(2)=TOTCOM(2)+COMAMT(2)
             TOTCOM = TOTCOM + COMAMT
             
             ! this next step is not necessary with integer arithmetics:
             ! calculus of carry over 
C            TEMP=TOTCOM(2)/DYN_BETUNIT
C            TOTCOM(1)=TOTCOM(1)+TEMP
C            TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)

             RETURN
           ENDIF
        ENDIF
C
C VALIDATION/REFUND COMMISSION
C
C
        IF(TYPE .EQ. TVAL .OR. TYPE .EQ. TREF) THEN
C         UNIT=DYN_VALUNIT
C         IF(TYPE.EQ.TREF) UNIT=DYN_BETUNIT
C         TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(UNIT)*
C    *                CALPER(COMTYP(TVAL)))

          ! as of 2015.07.02, in Portugal, DYN_VALUNIT and DYN_BETUNIT
          ! have the same value : 1 (check GLOBAL.DEF)
          ! call to CALPER simply divides a number by 100000, so this
          ! is the equivalent in integer arithmetics
          ! call to IDNINT simply converts a floating point number into
          ! the nearest integer

C         COMAMT(1)=TEMP/DYN_BETUNIT
C         COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
          CALL I8CAL_COMMISSION(COMAMT, AMOUNT, COMTYP(GAME))
          
C         TOTCOM(1)=TOTCOM(1)+COMAMT(1)
C         TOTCOM(2)=TOTCOM(2)+COMAMT(2)
          TOTCOM = TOTCOM + COMAMT
          
          ! this next step is not necessary with integer arithmetics:
          ! calculus of carry over 
C         TEMP=TOTCOM(2)/DYN_BETUNIT
C         TOTCOM(1)=TOTCOM(1)+TEMP
C         TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
          RETURN
        ENDIF
C
C CLAIM COMMISSION
C
        IF(TYPE .EQ. TRET) THEN
C         COMAMT(1)=AMOUNT*COMTYP(TRET)
C         COMAMT(2)=0
          CALL I4TOI8(AMOUNT,I8AMOUNT)
          CALL I4TOI8(COMTYP(TRET),I8COMTYP)
          COMAMT = I8AMOUNT * I8COMTYP
          
C         TOTCOM(1)=TOTCOM(1)+COMAMT(1)
          TOTCOM = TOTCOM + COMAMT
          RETURN
        ENDIF
C
C HIGH TIER WINNERS COMMISSION
C
        IF(TYPE .EQ. -1) THEN
C         TEMP=IDNINT(DFLOAT(AMOUNT)*DFLOAT(DYN_VALUNIT)*
C    *                CALPER(HVCRAT))
          ! call to CALPER simply divides a number by 100000, so this
          ! is the equivalent in integer arithmetics
          ! call to IDNINT simply converts a floating point number into
          ! the nearest integer
          
C         COMAMT(1)=TEMP/DYN_BETUNIT
C         COMAMT(2)=MOD(TEMP,DYN_BETUNIT)
          CALL I8CAL_COMMISSION(COMAMT, AMOUNT, HVCRAT)
          
C         TOTCOM(1)=TOTCOM(1)+COMAMT(1)
C         TOTCOM(2)=TOTCOM(2)+COMAMT(2)
          TOTCOM = TOTCOM + COMAMT

          ! this next step is not necessary with integer arithmetics:
          ! calculus of carry over 
C         TEMP=TOTCOM(2)/DYN_BETUNIT
C         TOTCOM(1)=TOTCOM(1)+TEMP
C         TOTCOM(2)=MOD(TOTCOM(2),DYN_BETUNIT)
          RETURN
        ENDIF
        RETURN
        END


        SUBROUTINE SALCOM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C

        ! arguments
        INTEGER*4  SALES(*)         !
        INTEGER*4  SUBCLASS         !
        INTEGER*4  SALOFF           !
        INTEGER*4  CDC              !
        INTEGER*4  RTER             !

        LOGICAL    MANAGER          !

        ! variables                 
        INTEGER*4  SALIND           !
        INTEGER*4  GNUM             !
        INTEGER*4  GTYP             !
C       INTEGER*4  COMAMT(2)        !
        INTEGER*4  CLERK            !
        INTEGER*4  I                !
C       INTEGER*4  TOTAMT(2)/0,0/   !
        INTEGER*4  GIND             !
        INTEGER*4  DUMMY            !
        INTEGER*4  SALIDX           ! TEMPORAL SALES INDEX
        INTEGER*4  TMP_GNUM         ! used to add Lotto4 cnt/amt to Lotto4
C
C       INTEGER*4   WORK_COM(2)          !WORK VARIABLE FOR COMMISSIONS
C
        INTEGER*4  IDX(MAXGAM)
        
        ! Auxiliary variables for 8-byte integer
        INTEGER*8 I8TMPAMT
        INTEGER*8 I8TOTAMT
        INTEGER*8 I8WORK_COM
        INTEGER*8 I8COMAMT
        INTEGER*4 I4TMPCNT

        ! functions
        LOGICAL   NO_JOK_DET
        EXTERNAL  NO_JOK_DET
        
        I8TMPAMT   = KZEXT(0)
        I8TOTAMT   = KZEXT(0)
        I8WORK_COM = KZEXT(0)
        I8COMAMT   = KZEXT(0)
        I4TMPCNT = 0


C
C SET / CLEAR VARIABLES
C
C        TOTAMT(1) = 0
C        TOTAMT(2) = 0

        SALIND        = 1

C
C GET COMMISSION REPORTS
C
        IF(SUBCLASS.EQ.0.OR.SUBCLASS.EQ.8) THEN
            CALL FASTSET(0,IDX,MAXGAM)
            DO 800 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 800
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 800                
                IF(GTYP.EQ.TINS) GOTO 800
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 800

                IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
                  TMP_GNUM = GNUM
                ELSE
                  TMP_GNUM = 6
                ENDIF

C                WORK_COM(1)=0
C                WORK_COM(2)=0
                DUMMY=0                
                CALL I8GETCOM(AGTGAM(GSAMT,GNUM,RTER)-
     *                        AGTGAM(GCAMT,GNUM,RTER),
     *                        TWAG,
     *                        GNUM,
     *                        I8COMAMT,
     *                        I8WORK_COM,
     *                        GTYP,
     *                        GIND,
     *                        RTER)
                CALL I8GETCOM(AGTGAM(GVAMT,GNUM,RTER),
     *                        TVAL,
     *                        GNUM,
     *                        I8COMAMT,
     *                        I8WORK_COM,
     *                        DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
                IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
C                   WORK_COM(1)=0
C                   WORK_COM(2)=0
                    I8WORK_COM = KZEXT(0)
                ENDIF
C               CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
C               CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
                CALL I8ADDI8I8(SALES(IDX(TMP_GNUM))
     *                       , SALES(IDX(TMP_GNUM)), I8WORK_COM)
                CALL I8ADDI8I8(I8TOTAMT
     *                       , I8TOTAMT, I8WORK_COM)
                I8WORK_COM = KZEXT(0)
800         CONTINUE
            CDC=DAYCDC
        ENDIF


C
C IF SHOP OWNER GET EVERYONE OF HIS CLERKS ACCOUNTS
C
        IF(MANAGER.AND.P(CLRKACT).EQ.0) THEN
            DO 820 CLERK=2,8
                IF(AGTTAB((APSNUM+CLERK)-1,RTER).EQ.0) GOTO 820
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 810 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)

                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 810
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 810
                    IF(GTYP.EQ.TINS) GOTO 810
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 810

                    IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
                      TMP_GNUM = GNUM
                    ELSE
                      TMP_GNUM = 6
                    ENDIF

C                   WORK_COM(1)=0
C                   WORK_COM(2)=0
                    I8WORK_COM = KZEXT(0)
                    DUMMY=0
                    CALL I8GETCOM(CLRKDAY(GSAMT,GNUM,CLERK)-
     *                            CLRKDAY(GCAMT,GNUM,CLERK),
     *                            TWAG,
     *                            GNUM,
     *                            I8COMAMT,
     *                            I8WORK_COM,
     *                            GTYP,GIND,RTER)
                    CALL I8GETCOM(CLRKDAY(GVAMT,GNUM,CLERK),
     *                            TVAL,
     *                            GNUM,
     *                            I8COMAMT,
     *                            I8WORK_COM,
     *                            DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
                    IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
C                       WORK_COM(1)=0
C                       WORK_COM(2)=0
                        I8WORK_COM = KZEXT(0)
                    ENDIF

C                   CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
C                   CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
                    CALL I8ADDI8I8(SALES(IDX(TMP_GNUM))
     *                           , SALES(IDX(TMP_GNUM)), I8WORK_COM)
                    CALL I8ADDI8I8(I8TOTAMT
     *                           , I8TOTAMT, I8WORK_COM)
                    I8WORK_COM = KZEXT(0)
810             CONTINUE
820         CONTINUE
        ENDIF


C
C IF WEEK TO DATE ALSO ACCUMULATE WEEK TO DATE
C
        IF(SUBCLASS.EQ.8) THEN
            DO 840 I=1,9
                IF(ASFDAT(ASFCDC,I).LE.ASFINV(ASFEND,1)) GOTO 840
                SALIND = 1
                CALL FASTSET(0,IDX,MAXGAM)
                DO 830 GNUM=1,MAXGAM
                    GTYP=GNTTAB(GAMTYP,GNUM)
                    GIND=GNTTAB(GAMIDX,GNUM)
                    IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 830
                    IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 830
                    IF(GTYP.EQ.TINS) GOTO 830
                    IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 830

                    IF(GNUM.NE.7) THEN
                      IF(IDX(GNUM).EQ.0) THEN
                        IDX(GNUM) = SALIND
                        SALIND = SALIND + 2
                      ENDIF
                      TMP_GNUM = GNUM
                    ELSE
                      TMP_GNUM = 6
                    ENDIF

C                   WORK_COM(1)=0
C                   WORK_COM(2)=0
                    I8WORK_COM = KZEXT(0)
                    DUMMY=0
                    CALL I8GETCOM(ASFDAY(GSAMT,GNUM,I)-
     *                            ASFDAY(GCAMT,GNUM,I),
     *                            TWAG,
     *                            GNUM,
     *                            I8COMAMT,
     *                            I8WORK_COM,
     *                            GTYP,GIND,RTER)
                    CALL I8GETCOM(ASFDAY(GVAMT,GNUM,I),
     *                            TVAL,
     *                            GNUM,
     *                            I8COMAMT,
     *                            I8WORK_COM,
     *                            DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
                    IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM))THEN
C                       WORK_COM(1)=0
C                       WORK_COM(2)=0
                        I8WORK_COM = KZEXT(0)
                    ENDIF

C                   CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
C                   CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
                    CALL I8ADDI8I8(SALES(IDX(TMP_GNUM))
     *                           , SALES(IDX(TMP_GNUM)), I8WORK_COM)
                    CALL I8ADDI8I8(I8TOTAMT
     *                           , I8TOTAMT, I8WORK_COM)
830             CONTINUE
840         CONTINUE
            CDC=DAYCDC
        ENDIF


C
C PROCESS FOR DAY REQUESTED
C
        IF(SUBCLASS.GE.1.AND.SUBCLASS.LE.7) THEN
            DO I=1,9
              IF(ASFDAT(ASFDOW,I).EQ.SUBCLASS) GOTO 860
            END DO
            TRABUF(TERR)=INVL
            GOTO 8000
860         CONTINUE
            SALIND = 1
            CALL FASTSET(0,IDX,MAXGAM)
            DO 870 GNUM=1,MAXGAM
                GTYP=GNTTAB(GAMTYP,GNUM)
                GIND=GNTTAB(GAMIDX,GNUM)
                IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GOTO 870   !IF GAME NON EXISTENT
                IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 870
                IF(GTYP.EQ.TINS) GOTO 870
                IF(GTYP.EQ.TKIK .AND. NO_JOK_DET()) GOTO 870

                IF(GNUM.NE.7) THEN
                  IF(IDX(GNUM).EQ.0) THEN
                    IDX(GNUM) = SALIND
                    SALIND = SALIND + 2
                  ENDIF
                  TMP_GNUM= GNUM
                ELSE
                  TMP_GNUM = 6
                ENDIF

C               WORK_COM(1)=0
C               WORK_COM(2)=0
                I8WORK_COM = KZEXT(0)
                DUMMY=0
                CALL I8GETCOM(ASFDAY(GSAMT,GNUM,I)-
     *                        ASFDAY(GCAMT,GNUM,I),
     *                        TWAG,
     *                        GNUM,
     *                        I8COMAMT,
     *                        I8WORK_COM,
     *                        GTYP,GIND,RTER)
                CALL I8GETCOM(ASFDAY(GVAMT,GNUM,I),
     *                        TVAL,
     *                        GNUM,
     *                        I8COMAMT,
     *                        I8WORK_COM,
     *                        DUMMY,DUMMY,DUMMY)
C
C IF NO COMMISSION FLAG OR GAME TYPE IS PASSIVE DON'T SEND COMMISSION
C
                IF(TSBIT(AGTTAB(AGTTYP,RTER),AGTNCM)) THEN      
C                   WORK_COM(1)=0
C                   WORK_COM(2)=0
                    I8WORK_COM = KZEXT(0)
                ENDIF

C               CALL ADDI8I8(SALES(IDX(TMP_GNUM)),WORK_COM(1),BETUNIT)
C               CALL ADDI8I8(TOTAMT,WORK_COM(1),BETUNIT)
                CALL I8ADDI8I8(SALES(IDX(TMP_GNUM))
     *                       , SALES(IDX(TMP_GNUM)), I8WORK_COM)
                CALL I8ADDI8I8(I8TOTAMT
     *                       , I8TOTAMT, I8WORK_COM)
                I8WORK_COM = KZEXT(0)
870         CONTINUE
            CDC=ASFDAT(ASFCDC,I)
        ENDIF
C
C COPY TOTALS TO SALES BUFFER
C
C       CALL FASTMOV(TOTAMT, SALES(SALIND), 2)
        CALL FASTMOV(I8TOTAMT, SALES(SALIND), 2)
        SALOFF = SALIND + 1
C
C WE HAVE TO SEND SALES IN LITLE ENDIAN
C
        DO  SALIDX = 1, SALOFF, 2
          I = SALES(SALIDX)
          SALES(SALIDX) = SALES(SALIDX + 1) 
          SALES(SALIDX + 1) = I 
        ENDDO        
C
C BUILD REPORT BACK TO TERMINAL
C
C       SALES(SALIND) = TOTAMT(2)
C       SALES(SALIND+1) = TOTAMT(1)
8000    CONTINUE

        RETURN

        END
C----+------------------------------------------------------------------
C V17| Correction of SALCOM for Portugal
C----+------------------------------------------------------------------
