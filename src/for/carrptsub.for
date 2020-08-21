C CARRPTSUB.FOR
C
C CARTEL WEEKLY SUMMARY REPORT
C
C V02 24-JAN-2000 OXK Added checking for PEND
C V01 13-NOV-1997 UXN Initial release.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
	SUBROUTINE CARRPTSUB
        IMPLICIT NONE
C
	INCLUDE '(LIB$ROUTINES)'
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:CARRPT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'
C
        INTEGER*4  AGTTOT(GRAMT)            !AGENT TOTALS
        INTEGER*4  REPLU                    !
        INTEGER*4  COPY                     !
        INTEGER*4  TOTRNT                   !
        INTEGER*4  TOTADJ(2)                !
        INTEGER*4  AGCOUNT                  !
        INTEGER*4  TAGCOUNT                 !

        INTEGER*4  CTOTRNT                  !
        INTEGER*4  CTOTADJ(2)               !
        INTEGER*4  CAGCOUNT                 !
        INTEGER*4  CTAGCOUNT                !

        INTEGER*4  OTOTRNT                  !
        INTEGER*4  OTOTADJ(2)               !
        INTEGER*4  OAGCOUNT                 !
        INTEGER*4  OTAGCOUNT                !

        INTEGER*4  TB_TOTRNT                !
        INTEGER*4  TB_TOTADJ(2)             !
        INTEGER*4  TB_AGCOUNT               !
        INTEGER*4  TB_TAGCOUNT              !

        INTEGER*4  PRECAR                   !
        INTEGER*4  PAGE                     !
        INTEGER*4  ST                       !
        INTEGER*4  CARTEL                   !
        INTEGER*4  CERR                     !
        INTEGER*4  ACOMSN(2)                !
        INTEGER*4  GNUM                     !
        INTEGER*4  ACTOT                    !
        INTEGER*4  XOFF                     !
        INTEGER*4  COMMSN(2)                !
        INTEGER*4  AMTDUE(2)                !
        INTEGER*4  SPECIAL_CARTEL           !
        INTEGER*4  TB_CARTEL                !
        INTEGER*4  NETSAL                   !Fishing nets for sale flag
        INTEGER*4  DUMMY(2)                 !Laurel & Hardy
        INTEGER*4  GTYP
        INTEGER*4  GIND
        INTEGER*4  ATYPE
	INTEGER*4  I

        LOGICAL    ACTIVE

	INTEGER*4  PSTART,PEND

C
C ENTRY CARRPT_BEGIN
C
	ENTRY CARRPT_BEGIN()
C
        SPECIAL_CARTEL =  5
        TB_CARTEL =  8
        TYPE *,IAM(),'**** CARRPT Cartel Invoice Summary Report ****'
C***    CALL INPNUM('Enter number of report copies ',COPY,0,10,EXT)
C***    IF(EXT.LT.0) STOP
        COPY=0
C
C CLEAR / SET VARIABLES
C
        CALL FASTSET(0, TOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, GRNTOT, GRAMT)
        CALL FASTSET(0, GRDCOM, 2*MAXGAM)
        DUELOT(1) = 0
        DUELOT(2) = 0
        DUEAGT(1) = 0
        DUEAGT(2) = 0
        TOTCMS(1) = 0
        TOTCMS(2) = 0
        TOTRNT    = 0
        TOTADJ(1) = 0
        TOTADJ(2) = 0
        AGCOUNT   = 0
        TAGCOUNT  = 0
C
        CALL FASTSET(0, CTOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, CGRNTOT, GRAMT)
        CALL FASTSET(0, CGRDCOM, 2*MAXGAM)
C
C
        CDUELOT(1) = 0
        CDUELOT(2) = 0
        CDUEAGT(1) = 0
        CDUEAGT(2) = 0
        CTOTCMS(1) = 0
        CTOTCMS(2) = 0
        CTOTRNT    = 0
        CTOTADJ(1) = 0
        CTOTADJ(2) = 0
        CAGCOUNT   = 0
        CTAGCOUNT  = 0
C
        CALL FASTSET(0, OTOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, OGRNTOT, GRAMT)
        CALL FASTSET(0, OGRDCOM, 2*MAXGAM)
C
        ODUELOT(1) = 0
        ODUELOT(2) = 0
        ODUEAGT(1) = 0
        ODUEAGT(2) = 0
        OTOTCMS(1) = 0
        OTOTCMS(2) = 0
        OTOTRNT    = 0
        OTOTADJ(1) = 0
        OTOTADJ(2) = 0
        OAGCOUNT   = 0
        OTAGCOUNT  = 0
C
        CALL FASTSET(0, TB_TOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, TB_GRNTOT, GRAMT)
        CALL FASTSET(0, TB_GRDCOM, 2*MAXGAM)
C
        TB_DUELOT(1) = 0
        TB_DUELOT(2) = 0
        TB_DUEAGT(1) = 0
        TB_DUEAGT(2) = 0
        TB_TOTCMS(1) = 0
        TB_TOTCMS(2) = 0
        TB_TOTRNT    = 0
        TB_TOTADJ(1) = 0
        TB_TOTADJ(2) = 0
        TB_AGCOUNT   = 0
        TB_TAGCOUNT  = 0
C
        PRECAR = 0
        PAGE   = 0
C
        ! OPEN THE REPORT FILE
	ST = LIB$GET_LUN(REPLU)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
        CALL ROPEN('CARRPT.REP',REPLU,ST)
        IF(ST.NE.0) THEN
            TYPE*,IAM(),'CARRPT.REP Open error  st - ',ST
            CLOSE(REPLU)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
	RETURN
C
C ENTRY CARRPT_UPDATE
C 	
	ENTRY CARRPT_UPDATE()
C
        CARTEL = 0
        CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,CERR)
C
          IF(CARTEL.NE.PRECAR) THEN
C
                ! REPORT ON CARTEL TOTALS PER GAME
                REPTYP = 1
                CALL CARRPT_REPRT(REPLU,PRECAR,
     *                     CTOTRNT,SPECIAL_CARTEL,
     *                     CTOTADJ, CAGCOUNT,
     *                     CTAGCOUNT, .FALSE. )
C
                CALL FASTSET(0, CTOTGAM, MAXGAM*GRAMT)
                CALL FASTSET(0, CGRNTOT, GRAMT)
                CALL FASTSET(0, CGRDCOM, 2*MAXGAM)
C
                CDUELOT(1) = 0
                CDUELOT(2) = 0
                CDUEAGT(1) = 0
                CDUEAGT(2) = 0
                CTOTCMS(1) = 0
                CTOTCMS(2) = 0
                CTOTRNT    = 0
                CTOTADJ(1) = 0
                CTOTADJ(2) = 0
                CAGCOUNT   = 0
                CTAGCOUNT  = 0
                PRECAR     = CARTEL
C
            ENDIF
C
            ! PROCESS AGENT TOTALS
            CALL FASTSET(0,AGTTOT,GRAMT)
            ACOMSN(1) = 0
            ACOMSN(2) = 0
C
            ACTIVE = .FALSE.
C
            DO 400 GNUM = 1, MAXGAM    ! TOTALS FOR EACH GAME
                GTYP = GNTTAB(GAMTYP,GNUM)
                GIND = GNTTAB(GAMIDX,GNUM)
                ACTOT = ASFBIL(GSAMT,GNUM,1)-
     *                  ASFBIL(GCAMT,GNUM,1)+
     *                  ASFBIL(GVAMT,GNUM,1)+
     *                  ASFBIL(GRAMT,GNUM,1)
                IF (ACTOT.NE.0) ACTIVE = .TRUE.
C
                DO 450 XOFF=1,GRAMT    ! OFFSETS(1-GRAMT) IN PRMAGT.DEF
C
                    ! ACCUMULATE TOTALS FOR INDIVIDUAL AGENT
                    AGTTOT(XOFF) = AGTTOT(XOFF) + ASFBIL(XOFF,GNUM,1)
C
                    ! ACCUMULATE GRAND TOTALS FOR ALL AGENTS BY GAME
                    TOTGAM(GNUM,XOFF) = TOTGAM(GNUM,XOFF) +
     *                                  ASFBIL(XOFF,GNUM,1)
                    GRNTOT(XOFF) = GRNTOT(XOFF) +
     *                             ASFBIL(XOFF,GNUM,1)
C
                    CTOTGAM(GNUM,XOFF) = CTOTGAM(GNUM,XOFF) +
     *                                   ASFBIL(XOFF,GNUM,1)
                    CGRNTOT(XOFF) = CGRNTOT(XOFF) +
     *                              ASFBIL(XOFF,GNUM,1)
                    IF (CARTEL.NE.SPECIAL_CARTEL) THEN
                        OTOTGAM(GNUM,XOFF) = OTOTGAM(GNUM,XOFF) +
     *                                       ASFBIL(XOFF,GNUM,1)
                        OGRNTOT(XOFF) = OGRNTOT(XOFF) +
     *                                  ASFBIL(XOFF,GNUM,1)
                    ENDIF
C
                    IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
                        TB_TOTGAM(GNUM,XOFF) = TB_TOTGAM(GNUM,XOFF) +
     *                                       ASFBIL(XOFF,GNUM,1)
                        TB_GRNTOT(XOFF) = TB_GRNTOT(XOFF) +
     *                                  ASFBIL(XOFF,GNUM,1)
                    ENDIF
C
450             CONTINUE
C
                ! CALCULATE SALES COMMISION
                COMMSN(1) = 0
                COMMSN(2) = 0
C		
		PSTART = ASFDAT(ASFCDC,1) - (ASFINV(ASFSTR,1)-1)
		PEND   = ASFDAT(ASFCDC,1) - (ASFINV(ASFEND,1)-1)
C
		IF(PSTART.GT.9) PSTART = 9 ! LAST 9 DAYS ONLY...
		IF(PEND.LT.1)   PEND   = 1
C
		DO I=PSTART,PEND,-1
                   NETSAL=ASFDAY(GSAMT,GNUM,I)-ASFDAY(GCAMT,GNUM,I)
                   CALL GETCOM(NETSAL,TWAG,GNUM,DUMMY,COMMSN,GTYP,GIND,XREC)
		ENDDO
C
                CALL ASCBIN(ASFINF,STTYP,LTTYP,ATYPE,CERR)
                IF(TSBIT(ATYPE,AGTNCM)) THEN
                  COMMSN(1)=0
                  COMMSN(2)=0
                ENDIF
C
                CALL ADDI8I8(ACOMSN, COMMSN, BETUNIT)
                CALL ADDI8I8(GRDCOM(1,GNUM), COMMSN, BETUNIT)
                CALL ADDI8I8(CGRDCOM(1,GNUM), COMMSN, BETUNIT)
C
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		  CALL ADDI8I8(OGRDCOM(1,GNUM), COMMSN, BETUNIT)
		ENDIF
C
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		  CALL ADDI8I8(TB_GRDCOM(1,GNUM), COMMSN, BETUNIT)
		ENDIF
C
400         CONTINUE
C
            ! TERMINAL RENT, ONLY FOR ACTIVE AGENTS
            IF (ACTIVE) THEN
                TOTRNT  = TOTRNT + ASFINV(ASFSRV,1)
                CTOTRNT = CTOTRNT+ASFINV(ASFSRV,1)
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		  OTOTRNT = OTOTRNT + ASFINV(ASFSRV,1)
	        ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		  TB_TOTRNT = TB_TOTRNT + ASFINV(ASFSRV,1)
	        ENDIF
            ENDIF
C
            ! GET ADJUSTMENTS
            CALL ADDI8I8(TOTADJ, ASFINV(ASFADJU,1), BETUNIT)
            CALL ADDI8I8(CTOTADJ, ASFINV(ASFADJU,1), BETUNIT)
            IF (CARTEL.NE.SPECIAL_CARTEL) THEN
	      CALL ADDI8I8(OTOTADJ, ASFINV(ASFADJU,1), BETUNIT)
	    ENDIF
            IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
	      CALL ADDI8I8(TB_TOTADJ, ASFINV(ASFADJU,1), BETUNIT)
	    ENDIF
C
            ! COMMISIONS
            CALL ADDI8I8(TOTCMS, ASFINV(ASFSCMU,1), BETUNIT)
            CALL ADDI8I8(CTOTCMS, ASFINV(ASFSCMU,1), BETUNIT)
            IF (CARTEL.NE.SPECIAL_CARTEL) THEN
	      CALL ADDI8I8(OTOTCMS, ASFINV(ASFSCMU,1), BETUNIT)
	    ENDIF
            IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
	      CALL ADDI8I8(TB_TOTCMS, ASFINV(ASFSCMU,1), BETUNIT)
	    ENDIF
C
            ! CALCULATE AMOUNT DUE
	    AMTDUE(1) = 0
	    AMTDUE(2) = 0
            CALL ADDI8I8(AMTDUE, ASFINV(ASFDUEU,1),BETUNIT)
            IF(AMTDUE(1).LE.0) THEN
                CALL ADDI8I8(DUEAGT, AMTDUE, BETUNIT)
                CALL ADDI8I8(CDUEAGT, AMTDUE, BETUNIT)
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		  CALL ADDI8I8(ODUEAGT, AMTDUE, BETUNIT)
	        ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		  CALL ADDI8I8(TB_DUEAGT, AMTDUE, BETUNIT)
	        ENDIF
            ELSE
                CALL ADDI8I8(DUELOT, AMTDUE, BETUNIT)
                CALL ADDI8I8(CDUELOT, AMTDUE, BETUNIT)
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		  CALL ADDI8I8(ODUELOT, AMTDUE, BETUNIT)
		ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		  CALL ADDI8I8(TB_DUELOT, AMTDUE, BETUNIT)
		ENDIF
            ENDIF

            ! COUNT AGENTS
            TAGCOUNT  = TAGCOUNT + 1
            CTAGCOUNT = CTAGCOUNT + 1
            IF (CARTEL.NE.SPECIAL_CARTEL) THEN
	      OTAGCOUNT = OTAGCOUNT + 1
	    ENDIF
            IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
	      TB_TAGCOUNT = TB_TAGCOUNT + 1
	    ENDIF
            IF (ACTIVE) THEN
                AGCOUNT  = AGCOUNT + 1
                CAGCOUNT = CAGCOUNT + 1
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		  OAGCOUNT = OAGCOUNT + 1
		ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		  TB_AGCOUNT = TB_AGCOUNT + 1
		ENDIF
            ENDIF
C
	RETURN
C
C ENTRY CARRPT_END
C
	ENTRY CARRPT_END()
C
	
        ! REPORT THE LAST CARTEL TOTALS PER GAME
        REPTYP = 1
        CALL CARRPT_REPRT(REPLU,PRECAR,
     *             CTOTRNT,SPECIAL_CARTEL,
     *             CTOTADJ, CAGCOUNT,
     *             CTAGCOUNT, .FALSE. )

        !REPORT NON50 CARTEL TOTALS PER GAME
        REPTYP = 2
        CALL CARRPT_REPRT(REPLU,1000,
     *             OTOTRNT,SPECIAL_CARTEL,
     *             OTOTADJ, OAGCOUNT,
     *             OTAGCOUNT, .FALSE. )

        !REPORT NON 5 AND NON 8 CARTEL TOTALS PER GAME
        REPTYP = 3
        CALL CARRPT_REPRT(REPLU,2000,
     *             TB_TOTRNT,SPECIAL_CARTEL,
     *             TB_TOTADJ, TB_AGCOUNT,
     *             TB_TAGCOUNT, .FALSE. ,TB_CARTEL)

        !REPORT ON FINAL TOTALS PER GAME
        REPTYP = 4
        CALL CARRPT_REPRT(REPLU,PRECAR,
     *             TOTRNT,SPECIAL_CARTEL,
     *             TOTADJ, AGCOUNT, TAGCOUNT, .TRUE. )
C
        CALL SPOOL('CARRPT.REP',COPY,ST)
	ST = LIB$FREE_LUN(REPLU)
	RETURN
        END
