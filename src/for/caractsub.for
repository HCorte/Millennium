C CARACT.FOR
C
C CARTEL DAILY SUMMARY REPORT
C
C V02 13-MAY-2010 RXK ePassive changes and changes for BALANS report. 
C V01 13-NOV-1997 UXN Initial release (produced from CARACT.FOR)
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE CARACTSUB
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
	INCLUDE 'INCLIB:ASFREP.DEF'

        INTEGER*4  TOTGAM(MAXGAM,GRAMT)   ! BOTTOM LINE TOTALS
        INTEGER*4  GRDCOM(2,MAXGAM)       ! BOTTOM LINE TOTALS

        INTEGER*4  TB_TOTGAM(MAXGAM,GRAMT)! NON SPE OR TB BOTTOM LINE TOTALS
        INTEGER*4  TB_GRDCOM(2,MAXGAM)    ! NON SPE OR TB BOTTOM LINE TOTALS
        INTEGER*4  OTOTGAM(MAXGAM,GRAMT)  ! NONSPECIAL_CARTEL BOTTOM LINE TOTALS         
        INTEGER*4  OGRDCOM(2,MAXGAM)      ! NONSPECIAL_CARTEL BOTTOM LINE TOTALS         
        INTEGER*4  CTOTGAM(MAXGAM,GRAMT)  ! CARTEL BOTTOM LINE TOTALS
        INTEGER*4  CGRDCOM(2,MAXGAM)      ! CARTEL BOTTOM LINE TOTALS

        INTEGER*4  GRNTOT(GRAMT)          ! GRAND TOTAL LINE

        INTEGER*4  OGRNTOT(GRAMT)         ! NONSPECIAL_CARTEL GRAND TOTAL LINE
        INTEGER*4  CGRNTOT(GRAMT)         ! CARTEL GRAND TOTAL LINE
        INTEGER*4  TB_GRNTOT(GRAMT)       ! NON SPE OR TB GRAND TOTAL LINE

        INTEGER*4  AGTTOT(GRAMT)          ! AGENT TOTALS

        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4  TOTSUMS(NO_BALSUMS)
        INTEGER*4  GAMECOMS(MAXGAM)
        INTEGER*4  TOTCOM
        INTEGER*4  TOTDUE(2)

        INTEGER*4  DUELOT(2)              !
        INTEGER*4  DUEAGT(2)              !
        INTEGER*4  TOTCMS(2)              !
        INTEGER*4  AGCOUNT                !
        INTEGER*4  TAGCOUNT               !

        INTEGER*4  CDUELOT(2)             !
        INTEGER*4  CDUEAGT(2)             !
        INTEGER*4  CTOTCMS(2)             !
        INTEGER*4  CAGCOUNT               !
        INTEGER*4  CTAGCOUNT              !

        INTEGER*4  ODUELOT(2)             !
        INTEGER*4  ODUEAGT(2)             !
        INTEGER*4  OTOTCMS(2)             !
        INTEGER*4  OAGCOUNT               !
        INTEGER*4  OTAGCOUNT              !

        INTEGER*4  TB_DUELOT(2)           !
        INTEGER*4  TB_DUEAGT(2)           !
        INTEGER*4  TB_TOTCMS(2)           !
        INTEGER*4  TB_AGCOUNT             !
        INTEGER*4  TB_TAGCOUNT            !

        INTEGER*4  COPY                   !
        INTEGER*4  REPLU                  !
        INTEGER*4  PAGE                   !
        INTEGER*4  PRECAR                 !
        INTEGER*4  ACOMSN(2)              !
        INTEGER*4  ST                     !
        INTEGER*4  NETSAL                 !
        INTEGER*4  COMMSN(2)              !
        INTEGER*4  CARTEL                 !
        INTEGER*4  CERR                   !
        INTEGER*4  GNUM                   !
        INTEGER*4  XOFF                   !
        INTEGER*4  AMTDUE(2)              !
        INTEGER*4  IND                    !
        INTEGER*4  GAM                    !
        INTEGER*4  TTYP                   !
        INTEGER*4  RAPCODE                !
        INTEGER*4  SPECIAL_CARTEL         !
        INTEGER*4  TB_CARTEL              !
        INTEGER*4  COMAMT(2)              !
        INTEGER*4  GTYP                   !
        INTEGER*4  GIND                   !
        INTEGER*4  ATYPE                  !

        REAL*8     TOTREAL                !

        LOGICAL    ACTIVE
	LOGICAL    FIRST/.TRUE./
C
C ENTRY CARACT_BEGIN
C
	ENTRY CARACT_BEGIN()
C
        SPECIAL_CARTEL = 5
        TB_CARTEL = 8
        TYPE *,IAM(),'**** CARACT Cartel Daily Summary Report ****'
C        CALL INPNUM('Enter number of report copies ',COPY,0,10,EXT)
C        IF(EXT.LT.0) STOP
        COPY=1

        ! Clear / set variables

        CALL FASTSET(0, TOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, GRNTOT, GRAMT)
        CALL FASTSET(0, GRDCOM, MAXGAM*2)

        CALL FASTSET(0, GAMESUMS, MAXGAM*NUMFIN*NUMTOT)
        CALL FASTSET(0, TOTSUMS, NO_BALSUMS)
        TOTREAL = 0.0D0
     
        DUELOT(1)   = 0
        DUELOT(2)   = 0
        DUEAGT(1)   = 0
        DUEAGT(2)   = 0
        TOTCMS(1)   = 0
        TOTCMS(2)   = 0
        AGCOUNT  = 0
        TAGCOUNT = 0
C
        CALL FASTSET(0, CTOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, CGRNTOT, GRAMT)
        CALL FASTSET(0, CGRDCOM, MAXGAM*2)
        CDUELOT(1) = 0
        CDUELOT(2) = 0
        CDUEAGT(1) = 0
        CDUEAGT(2) = 0
        CTOTCMS(1) = 0
        CTOTCMS(2) = 0
        CAGCOUNT  = 0
        CTAGCOUNT = 0
C
        CALL FASTSET(0, OTOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, OGRNTOT, GRAMT)
        CALL FASTSET(0, OGRDCOM, MAXGAM*2)
        ODUELOT(1) = 0
        ODUELOT(2) = 0
        ODUEAGT(1) = 0
        ODUEAGT(2) = 0
        OTOTCMS(1) = 0
        OTOTCMS(2) = 0
        OAGCOUNT  = 0
        OTAGCOUNT = 0
C
        CALL FASTSET(0, TB_TOTGAM, MAXGAM*GRAMT)
        CALL FASTSET(0, TB_GRNTOT, GRAMT)
        CALL FASTSET(0, TB_GRDCOM, MAXGAM*2)
        TB_DUELOT(1) = 0
        TB_DUELOT(2) = 0
        TB_DUEAGT(1) = 0
        TB_DUEAGT(2) = 0
        TB_TOTCMS(1) = 0
        TB_TOTCMS(2) = 0
        TB_AGCOUNT  = 0
        TB_TAGCOUNT = 0
C
        PRECAR = 0
        PAGE   = 0

        ! Open the report file
	ST=LIB$GET_LUN(REPLU)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
        CALL ROPEN('CARACT.REP', REPLU, ST)
        IF(ST.NE.0) THEN
            TYPE*,IAM(),'CARACT.REP Open error  st - ',ST
            CLOSE(REPLU)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
	RETURN
C
C ENTRY CARACT_UPDATE
C
	ENTRY CARACT_UPDATE()
C
            ! process cartel totals
        CARTEL = 0
        CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,CERR)
	IF(FIRST) THEN
             PRECAR = CARTEL
	     FIRST  = .FALSE.
	ENDIF
C
        IF (CARTEL.NE.PRECAR) THEN
C
                ! REPORT ON CARTEL TOTALS PER GAME
          CALL CARACT_REPRT(REPLU,PRECAR,CTOTGAM,CGRDCOM,CGRNTOT,CTOTCMS,
     *                     CDUEAGT,CDUELOT,
     *                     CAGCOUNT,CTAGCOUNT,.FALSE.,SPECIAL_CARTEL,TB_CARTEL)
C
                CALL FASTSET(0,CTOTGAM,MAXGAM*GRAMT)
                CALL FASTSET(0,CGRNTOT,GRAMT)
                CALL FASTSET(0,CGRDCOM,MAXGAM*2)
                CDUELOT(1) = 0
                CDUELOT(2) = 0
                CDUEAGT(1) = 0
                CDUEAGT(2) = 0
                CTOTCMS(1) = 0 
                CTOTCMS(2) = 0
                CAGCOUNT  = 0
                CTAGCOUNT = 0
                PRECAR = CARTEL
         ENDIF
C
            ! Process agent totals
         CALL FASTSET(0,AGTTOT,GRAMT)
         ACOMSN(1) = 0
         ACOMSN(2) = 0
         ACTIVE = .FALSE.
C
         DO 450 GNUM = 1, MAXGAM    ! TOTALS FOR EACH GAME
             DO 400 XOFF = 1, GRAMT ! OFFSETS(1-GRAMT) IN PRMAGT.DEF

                    ! ACCUMULATE TOTALS FOR INDIVIDUAL AGENT
                    AGTTOT(XOFF) = AGTTOT(XOFF) + ASFDAY(XOFF,GNUM,1)
                    IF (AGTTOT(XOFF).NE.0) ACTIVE = .TRUE.
C
                    ! ACCUMULATE GRAND TOTALS FOR ALL AGENTS BY GAME
                    TOTGAM(GNUM,XOFF) = TOTGAM(GNUM,XOFF) +
     *                                  ASFDAY(XOFF,GNUM,1)
                    GRNTOT(XOFF) = GRNTOT(XOFF) + ASFDAY(XOFF,GNUM,1)
C
                    CTOTGAM(GNUM,XOFF) = CTOTGAM(GNUM,XOFF) +
     *                                   ASFDAY(XOFF,GNUM,1)
                    CGRNTOT(XOFF) = CGRNTOT(XOFF) + ASFDAY(XOFF,GNUM,1)
C
                    IF (CARTEL.NE.SPECIAL_CARTEL) THEN
                        OTOTGAM(GNUM,XOFF) = OTOTGAM(GNUM,XOFF) +
     *                                       ASFDAY(XOFF,GNUM,1)
                        OGRNTOT(XOFF) = OGRNTOT(XOFF) + ASFDAY(XOFF,GNUM,1)
                    ENDIF

                    IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
                        TB_TOTGAM(GNUM,XOFF) = TB_TOTGAM(GNUM,XOFF) +
     *                                       ASFDAY(XOFF,GNUM,1)
                        TB_GRNTOT(XOFF) = TB_GRNTOT(XOFF) + ASFDAY(XOFF,GNUM,1)
                    ENDIF
C
400          CONTINUE

                ! calculate sales commision
              COMMSN(1) = 0
              COMMSN(2) = 0
              NETSAL = ASFDAY(GSAMT,GNUM,1) - ASFDAY(GCAMT,GNUM,1)
              GTYP = GNTTAB(GAMTYP,GNUM)
              GIND = GNTTAB(GAMIDX,GNUM)
              CALL GETCOM(NETSAL,TWAG,GNUM,COMAMT,COMMSN,GTYP,GIND,XREC)
C
      	      CALL ASCBIN(ASFINF,STTYP,LTTYP,ATYPE,CERR)
      	      IF(TSBIT(ATYPE,AGTNCM)) THEN
      		  COMAMT(1)=0
      		  COMAMT(2)=0
      		  COMMSN(1)=0
      		  COMMSN(2)=0
      	      ENDIF
C
              CALL ADDI8I8(ACOMSN,COMMSN,BETUNIT)
              CALL ADDI8I8(GRDCOM(1,GNUM),COMMSN,BETUNIT)
              CALL ADDI8I8(CGRDCOM(1,GNUM),COMMSN,BETUNIT)
C
              IF (CARTEL.NE.SPECIAL_CARTEL) THEN
                    CALL ADDI8I8(OGRDCOM(1,GNUM),COMMSN,BETUNIT)
              ENDIF
C
              IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
                    CALL ADDI8I8(TB_GRDCOM(1,GNUM),COMMSN,BETUNIT)
              ENDIF
C
450         CONTINUE

            ! commisions
            CALL ADDI8I8(TOTCMS,ACOMSN,BETUNIT)
            CALL ADDI8I8(CTOTCMS,ACOMSN,BETUNIT)
C
            IF (CARTEL.NE.SPECIAL_CARTEL) THEN
	        CALL ADDI8I8(OTOTCMS,ACOMSN,BETUNIT)
	    ENDIF
C
            IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
	        CALL ADDI8I8(TB_TOTCMS,ACOMSN,BETUNIT)
	    ENDIF
C
            ! calculate amount due
            AMTDUE(1)=0
            AMTDUE(2)=0
            CALL ADDI8I4(AMTDUE,AGTTOT(GSAMT),BETUNIT)
            CALL SUBI8I4(AMTDUE,AGTTOT(GCAMT),BETUNIT)
            CALL SUBI8I4(AMTDUE,AGTTOT(GVAMT),BETUNIT)
            CALL SUBI8I8(AMTDUE,ACOMSN,BETUNIT)
            CALL SUBI8I4(AMTDUE,AGTTOT(GRAMT),BETUNIT)
C
            IF(AMTDUE(1).LE.0) THEN
                AMTDUE(1) = ABS(AMTDUE(1)) 
                AMTDUE(2) = ABS(AMTDUE(2)) 
                CALL ADDI8I8(DUEAGT,AMTDUE,BETUNIT)
                CALL ADDI8I8(CDUEAGT,AMTDUE,BETUNIT)
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
                    CALL ADDI8I8(ODUEAGT,AMTDUE,BETUNIT)
                ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
                    CALL ADDI8I8(TB_DUEAGT,AMTDUE,BETUNIT)
                ENDIF
            ELSE
                CALL ADDI8I8(DUELOT,AMTDUE,BETUNIT)
                CALL ADDI8I8(CDUELOT,AMTDUE,BETUNIT)
                IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		    CALL ADDI8I8(ODUELOT,AMTDUE,BETUNIT)
		ENDIF
                IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		    CALL ADDI8I8(TB_DUELOT,AMTDUE,BETUNIT)
		ENDIF
            ENDIF
C
            ! count agents
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
            TAGCOUNT  = TAGCOUNT + 1
            CTAGCOUNT = CTAGCOUNT + 1
            IF (CARTEL.NE.SPECIAL_CARTEL) THEN
		OTAGCOUNT = OTAGCOUNT + 1
	    ENDIF
            IF (CARTEL.NE.SPECIAL_CARTEL.AND.CARTEL.NE.TB_CARTEL) THEN
		TB_TAGCOUNT = TB_TAGCOUNT + 1
	    ENDIF
C
	RETURN
C
C ENTRY CARACT_END
C
	ENTRY CARACT_END()
C
        ! report the last cartel totals per game
        CALL CARACT_REPRT(REPLU,PRECAR,CTOTGAM,CGRDCOM,CGRNTOT,CTOTCMS,
     *             CDUEAGT,CDUELOT,CAGCOUNT,
     *             CTAGCOUNT, .FALSE.,SPECIAL_CARTEL,TB_CARTEL )
C
        ! report other than SPECIAL_CARTEL cartel totals per game
        CALL CARACT_REPRT(REPLU,1000,OTOTGAM,OGRDCOM,OGRNTOT,OTOTCMS,
     *             ODUEAGT,ODUELOT,OAGCOUNT,
     *             OTAGCOUNT, .FALSE. ,SPECIAL_CARTEL,TB_CARTEL )

        ! report other than SPECIAL_CARTEL and other the telebettig
        CALL CARACT_REPRT(REPLU,2000,TB_TOTGAM,TB_GRDCOM,TB_GRNTOT,TB_TOTCMS,
     *             TB_DUEAGT,TB_DUELOT,TB_AGCOUNT,
     *             TB_TAGCOUNT, .FALSE. ,SPECIAL_CARTEL,TB_CARTEL )

        ! game totals and comissions to balansfile (BALWRI)  V04
        IND = 0
        DO 520 GAM = 1,MAXGAM
            IND = 0
            DO 510 TTYP = 1,TRET
                IND = IND + 1
                GAMESUMS(GAM,TTYP,1) = TOTGAM(GAM,IND)
                IF(IND.EQ.GVCNT) THEN
                   GAMESUMS(GAM,TTYP,1)=GAMESUMS(GAM,TTYP,1)+TOTGAM(GAM,GRCNT)
                ENDIF
                IND = IND + 1
                GAMESUMS(GAM,TTYP,2) = TOTGAM(GAM,IND)
                IF(IND.EQ.GVAMT) THEN
                   GAMESUMS(GAM,TTYP,2)=GAMESUMS(GAM,TTYP,2)+TOTGAM(GAM,GRAMT)
                ENDIF
510         CONTINUE
520     CONTINUE

        DO 530 GAM = 1,MAXGAM
            GAMECOMS(GAM) = GRDCOM(1,GAM)
530     CONTINUE
        TOTDUE(1) = DUELOT(1)
        TOTDUE(2) = DUEAGT(1)
        TOTCOM    = TOTCMS(1)

        TOTSUMS(1) = GRNTOT(GSCNT)
        TOTSUMS(2) = GRNTOT(GSAMT)
        TOTSUMS(3) = GRNTOT(GCCNT)
        TOTSUMS(4) = GRNTOT(GCAMT)
        TOTSUMS(5) = GRNTOT(GVCNT)+GRNTOT(GRCNT)
        TOTSUMS(6) = GRNTOT(GVAMT)+GRNTOT(GRAMT)
        TOTSUMS(7) = DUELOT(1)
        TOTSUMS(8) = DUEAGT(1)
        TOTSUMS(9) = GRNTOT(GCLCNT)
        TOTSUMS(10)= GRNTOT(GCLAMT)
C
        RAPCODE = 3
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
        CALL BALWRI2(RAPCODE,GAMECOMS,TOTCOM,TOTDUE)
C
        ! report on final totals
        CALL CARACT_REPRT(REPLU,PRECAR,TOTGAM,GRDCOM,GRNTOT,TOTCMS,
     *             DUEAGT, DUELOT, AGCOUNT,
     *             TAGCOUNT, .TRUE. ,SPECIAL_CARTEL,TB_CARTEL)
C
        CALL SPOOL('CARACT.REP',COPY,ST)
	ST = LIB$FREE_LUN(REPLU)
C
        END
