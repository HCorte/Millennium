C VIS_TVOISNP.FOR
C  
C V04 08-JUN-2000 UXN FTNAMES.DEF added.
C V03 31-MAY-2000 PXO Subroutine name from VSALSNP -> TVOISNP
C V02 17-Apr-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V01 27-Oct-1994 PXB Initial revision.
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE TWITSNP (DAT)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C---- Arguments passed in.

        INTEGER*4  DAT              !--- CDC date.

C---- Local paramenters.

	INTEGER*4  SALEN
        PARAMETER (SALEN = NUMTOT * (NUMFIN+1) * MAXGAM)

        INTEGER*4  TLEN
        PARAMETER (TLEN = NUMTOT * (NUMFIN+1) )

C---- Local integer variables.

        INTEGER*4  LIN
        INTEGER*4  I
        INTEGER*4  GNUM         !--- game number
        INTEGER*4  GIND         !--- game index
        INTEGER*4  GTYP         !--- game type
        INTEGER*4  REC          !--- DAF record to read
        INTEGER*4  K            !--- counter
        INTEGER*4  ST           !--- status
        INTEGER*4  OFF          !--- file descriptor block
        INTEGER*4  FDB(7)
	INTEGER*4  SALES(NUMTOT, NUMFIN+1, MAXGAM)
	INTEGER*4  TOTALS(NUMTOT,NUMFIN+1)

	INTEGER*2 D(LDATE_LEN)

	LOGICAL ACTIVE_GAMES    ! THERE ARE ACTIVE GAMES

C-------------------- Start of soucre code -----------------------------

	IF (DAT .LE. 0) THEN
            DAT = DAYCDC
        END IF

	D(VCDC) = DAT

	CALL LCDATE (D)

	IF (DAT .EQ. DAYCDC) THEN
	  SMODE = .FALSE.
	  CALL FASTMOV (DAYSTS,DAFSTS,DAFLEN)
	  GOTO 10
	END IF

	SMODE = .TRUE.

	CALL OPENW (1,SFNAMES(1,DAF),0,0,0,ST)

	IF (ST .NE. 0) THEN
	  CALL USRCLOS1(1)
	  WRITE(CLIN23,8000) (SFNAMES(K,DAF),K=1,5),ST
	  RETURN
	END IF

	CALL IOINIT (FDB,1,DAFSEC*256)

C---- Read DAF record.

	REC = DAT

	CALL READW (FDB,REC,DAFREC,ST)

	IF (ST .NE. 0) THEN
	  CALL USRCLOS1(1)
	  WRITE(CLIN23,8001) (SFNAMES(K,DAF),K=1,5),ST,REC
	  RETURN
	END IF

	CALL USRCLOS1(1)

C---- Check DAF status.

	IF(DAFSTS .EQ. DUNUSD) THEN
	  WRITE(CLIN23,8002) (D(K),K=7,13)
	  RETURN
	END IF

	IF(DAFSTS.EQ.DNOSAL) THEN
	  WRITE(CLIN23,8003) (D(K),K=7,13)
	  RETURN
	END IF

C---- Format sales snapshot.

10	CONTINUE

	CALL FASTSET(0, SALES, SALEN)

	CALL FASTSET(0, TOTALS, TLEN)

C---- Get total sales by game type.

	ACTIVE_GAMES = .FALSE.

        DO 30 GNUM = 1, MAXGAM
            IF (GNTTAB(GAMIDX,GNUM) .EQ. 0) GOTO 30
	    GTYP = GNTTAB(GAMTYP,GNUM)
	    IF (GTYP .NE. TWIT) GOTO 30
            ACTIVE_GAMES = .TRUE.
	    DO 20 I = 1, NUMFIN+1
	        IF(I.LE.NUMFIN) THEN
	            OFF=I
	            IF(OFF.EQ.TREF) OFF=TVAL
	            SALES(TRACNT,OFF,GNUM) = SALES(TRACNT,OFF,GNUM)+
     *	                                     DAFTYP(TRACNT,I,GNUM)
	            SALES(DOLAMT,OFF,GNUM) = SALES(DOLAMT,OFF,GNUM)+
     *	                                     DAFTYP(DOLAMT,I,GNUM)
	            TOTALS(TRACNT,I)       = TOTALS(TRACNT,I)+
     *	                                     DAFTYP(TRACNT,I,GNUM)
	            TOTALS(DOLAMT,I)       = TOTALS(DOLAMT,I)+
     *	                                     DAFTYP(DOLAMT,I,GNUM)
	        ELSE
	            SALES(TRACNT,I,GNUM) = SALES(TRACNT,I,GNUM)+
     *                                     DAFDIS(TRACNT,GNUM)
	            SALES(DOLAMT,I,GNUM) = SALES(DOLAMT,I,GNUM)+
     *                                     DAFDIS(DOLAMT,GNUM)
	            TOTALS(TRACNT,I)     = TOTALS(TRACNT,I)+
     *                                     DAFDIS(TRACNT,GNUM)
	            TOTALS(DOLAMT,I)     = TOTALS(DOLAMT,I)+
     *                                     DAFDIS(DOLAMT,GNUM)
	        ENDIF
20	    CONTINUE
30	CONTINUE

	IF(ACTIVE_GAMES .EQ. .FALSE.) THEN
          WRITE(CLIN23, 905)
 	  RETURN
	ENDIF

	WRITE(CLIN1,9001) (D(K),K=7,13)

	LIN = 3
	WRITE(XNEW(LIN),9005)

	LIN = LIN + 1
	WRITE(XNEW(LIN),902)

	LIN = LIN + 1
	WRITE(XNEW(LIN),9005)

	LIN = 6

        DO 100 GNUM = 1, MAXGAM
	    IF(GNTTAB(GAMIDX,GNUM) .EQ. 0) GOTO 100
	    GIND = GNTTAB(GAMIDX,GNUM)
	    GTYP = GNTTAB(GAMTYP,GNUM)
	    IF (GTYP .NE. TWIT) GOTO 100

            WRITE(XNEW(LIN),903)  GTNAMES(GTYP),GIND,
     *	                          SALES(TRACNT,TWAG,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TWAG,GNUM),11,BETUNIT),
     *                            SALES(TRACNT,TCAN,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TCAN,GNUM),11,BETUNIT),
     *                            SALES(TRACNT,TVAL,GNUM),	      
     *                            CMONY(SALES(DOLAMT,TVAL,GNUM),11,VALUNIT)
                                  
	    LIN=LIN+1

100	CONTINUE	

	LIN=LIN + 2

        WRITE(XNEW(LIN),904)    'Total   ',FTNAMES(TWAG),
     *                          TOTALS(TRACNT,TWAG),
     *                          CMONY(TOTALS(DOLAMT,TWAG),11,BETUNIT),
     *                          'Total   ',FTNAMES(TCAN),
     *                          TOTALS(TRACNT,TCAN),	      
     *                          CMONY(TOTALS(DOLAMT,TCAN),11,BETUNIT)

        WRITE(XNEW(LIN+1),904)  'Total   ',FTNAMES(TVAL),
     *                          TOTALS(TRACNT,TVAL),	      
     *                          CMONY(TOTALS(DOLAMT,TVAL),11,BETUNIT),
     *                          'Total   ',FTNAMES(TREF),
     *                          TOTALS(TRACNT,TREF),	      
     *                          CMONY(TOTALS(DOLAMT,TREF),11,VALUNIT)


C     ================== FORMAT STATEMENTS ================

C---- Error format statements.

8000	FORMAT (5A4,' file open error ',I4)

8001	FORMAT (5A4,' file read error ',I4,' record - ',I4)

8002	FORMAT ('Record not initialized for ',7A2)

8003	FORMAT (7A2,' is not a sales date')

C---- Screen format statements.

9001	FORMAT ('Voittaja Game Sales for ',7A2)

902	FORMAT ('TYPE / IND',11X,'SALES',17X,'CANCELS',10X,'CASHS/REFUNDS')

903	FORMAT (A8,I2,4X,3(I8,1X,A11,2X))

904	FORMAT (2(2A8,1X,I8,1X,A11,1X))

9005	FORMAT (80X)

905     FORMAT(X, 'There Are Not Active Voittaja Games, Enter Vision Command')
	RETURN
	END

C===================== End of Program ======================================
