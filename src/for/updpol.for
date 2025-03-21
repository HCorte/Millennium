C SUBROUTINE UPDPOL
C
C V22 17-MAY-1999 UXN Super Triple added.
C V21 06-APR-1998 UXN CONVERT changed according to BETLIN.
C V20 22-JAN-1996 HXK Removed initialisation of duplicate Double participants 
C                     for DBPOOL
C V19 16-JAN-1996 RXK RFSS 95190. Multiplier 6 for TSPPOL introduced
C V18 11-JAN-1996 HXK Changed calculation of AMT for system bets
C V17 20-DEC-1995 HXK Various fixes as part of the Double / Couple installation
C V16 18-DEC-1995 HXK Fixed bugs for Double/Couple
C V15 10-DEC-1995 HXK Prevent illegal index values in pool tables for 
C                     Double/Couple
C V14 07-NOV-1995 HXK Changes for Double, Couple install
C V13 05-MAR-1994 HXK REMOVED CONTROL CHARS FROM COMMENT (CAUSED BY PVCS 'PUT').
C V12 04-MAR-1994 JXP Utilise TSXMLI value maximum liability limit checking 
C                     instead of TSMXODD which has already been checked in 
C                     CHKLIA
C V11 03-MAR-1994 HXK REMOVED ERRORS IN CANCEL WAGER HANDLING.
C V10 28-FEB-1994 HXK KEEP SECONDS AND DATE IN SAME I*4 TO SAVE SPACE.
C V09 28-FEB-1994 HXK ADDED LIAB ERROR, etc.
C V08 26-FEB-1994 HXK FIXED BOUNDARY OVERFLOW
C V07 25-FEB-1994 HXK PITKA LIABILITY LIMITATIONS CHANGE.
C V06 18-FEB-1994 JXP Include Pitka combination liability restrictions
C V05 11-JAN-1994 JXP Included extra parameters for TOPATS subroutine 
C                     relating to TSPWTL table
C V04 30-JUN-1993 GXA Released for Finland Dec Conversion / Oddset.
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 30-JUN-1992 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C THIS SUBROUTINE UPDATES THE POOLS FOR ODDSET GAMES
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE UPDPOL(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DBLCOM.DEF'
	INCLUDE 'INCLIB:CPLCOM.DEF'
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:HASHMEM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	INTEGER*4 ROWCNT                 !Number of rows played
	INTEGER*4 ROWNUMS(0:5)           !Numbers of rows played
	INTEGER*4 ROWCOMB(0:5)           !Combination for each row
	INTEGER*4 TAB(6,40,5),CMB(5)
	INTEGER*4 CNVTBL(3),BETIND/0/
	INTEGER*4 NEXT_RECORD            !Record# where comb. found
	INTEGER*4 NEXT_BLOCK             !BLOCK# where comb. found
	REAL*8    PERLIM                
	LOGICAL   FIRST /.TRUE./
	DATA      CNVTBL/1,2,4/          !1=1,2=X,4=2
C
C TOPATS and TOPTSL will change ROWCOMB (from 4 to 3). So, 1=1,2=2,3=X
C
	CHARACTER*1 CONVERT(0:4)/' ','1','2','X',' '/ 
	DATA      BLANK/'    '/
	INTEGER*4 FASTBIN
	INTEGER*4 MESS(EDLEN)
	INTEGER*4 TYPE, GTYP, GIND, GNUM, TER
	INTEGER*4 TIME, NOBETS, BETNUM, AMOUNT ,I, TSLOFF, PSTAT
	INTEGER*4 AMT, NEWAMT, STATUS, BLANK, SYS, TAMTLIAB
	INTEGER*4 SIMPLE, SELECT, COUNT, J, IND, K, CNT, COMBIN
	INTEGER*4 TRAIND, POLTYP, ROW, HOMSCR, AWYSCR, INDEX
        INTEGER*4 SEC_DAY/86400/  !seconds in a day
	CHARACTER*4 CMESS(EDLEN)
	EQUIVALENCE (MESS,CMESS)
	LOGICAL POLCAN
C
	TYPE=TRABUF(TTYP)
	GTYP=TRABUF(TGAMTYP)
	GIND=TRABUF(TGAMIND)
	GNUM=GTNTAB(GTYP,GIND)
	TER=TRABUF(TTER)
C
C EXPAND SYSTEM BET TABLES 
C
	IF(FIRST)THEN
	   FIRST=.FALSE.
	   CALL TSLSYS(TAB(1,1,1),4,3,CMB(1))
	   CALL TSLSYS(TAB(1,1,2),5,3,CMB(2))
	   CALL TSLSYS(TAB(1,1,3),6,3,CMB(3))
	   CALL TSLSYS(TAB(1,1,4),5,4,CMB(4))
	   CALL TSLSYS(TAB(1,1,5),6,4,CMB(5))
	ENDIF
C
	POLCAN=.FALSE.
	IF(TYPE.EQ.TCAN.OR.TYPE.EQ.TINC) POLCAN=.TRUE.
C
C UPDATE POOLS FOR ANY ODDSET TRANSACTION
C
	IF(GTYP.EQ.TTSL) GOTO 1000
	IF(GTYP.EQ.TWIT) GOTO 2000
	IF(GTYP.EQ.TSCR) GOTO 3000
	IF(GTYP.EQ.TDBL) GOTO 4000
	IF(GTYP.EQ.TCPL) GOTO 5000
	IF(GTYP.EQ.TSSC) GOTO 6000
	IF(GTYP.EQ.TTRP) GOTO 7000
	IF(GTYP.EQ.TSTR) GOTO 8000
	RETURN
C
C TOTO SELECT TRANSACTION
C
1000	CONTINUE
C
C GET TIME IN SECONDS FOR THIS WAGER
C
	TIME=TRABUF(TTIM)
C
C UPDATE LIABILITY POOLS FOR TOTO SELECT COMBINATIONS OF 4,5&6 ROWS
C
	IF(P(SUPTSP).NE.0) GOTO 1025
	IF(TRABUF(TWSYST).NE.NOSYS) GOTO 1030
C
	NOBETS=TRABUF(TWNBET)
C
1041	CONTINUE
	DO 1040 BETNUM=0,NOBETS-1
C
	   AMOUNT=0
	   CALL FASTSET(0,ROWNUMS,6)
	   CALL FASTSET(0,ROWCOMB,6)
	   BETIND = (BETNUM*TWTBLEN*TWTRMAX) + (BETNUM*TWTHLEN) 
C
C UPDATE FOR COMBINATIONS OF 3,4,5 OR 6
C
	   ROWCNT = TRABUF(TWTSEL1+BETIND)
	   DO 1050 I = 0,ROWCNT - 1
	      ROWNUMS(I) = TRABUF((TWTROW1+BETIND)+I*TWTBLEN)
	      ROWCOMB(I) = CNVTBL(TRABUF((TWTPOL1+BETIND)+I*TWTBLEN))
1050	   CONTINUE
	   CALL ODDSOFF(ROWNUMS,ROWCOMB,ROWCNT,TSLOFF)
	   CALL HASHGET(TSLOFF,ROWCNT,AMOUNT,NEXT_RECORD,
     *	                NEXT_BLOCK,PSTAT)
	   AMT = TRABUF(TWTAMT1+BETIND)/TSLPRC(GIND)
C
C UPDATE NUMBER OF PLAYED COMBINATIONS TODAY
C
	   TSLCMB(GIND)=TSLCMB(GIND)+1
	   IF(POLCAN)TSLCMB(GIND)=TSLCMB(GIND)-2
C
C IF CANCELLED WAGER REMOVE FROM POOLS (AMOUNT IS CURRENTLY IN BET UNITS)
C
	   IF(POLCAN) AMT = -AMT
	   AMOUNT = AMOUNT + AMT
C
C CHECK THAT MINIMUM SALES LEVEL FOR LIABILITY CHECK HAS
C BEEN REACHED.
C
	   IF(DAYTYP(DOLAMT,1,GNUM).GT.P(TSPMIN).AND.
     *	      .NOT.POLCAN.AND.P(TSPMIN).NE.0) THEN
	       PERLIM=(AMOUNT*TSLPRC(GIND))/DAYTYP(DOLAMT,1,GNUM)
C
C IF TAKING THIS BET EXCEEDS THE LIABILITY LIMIT REJECT
C THE WAGER BY SETTING A SYNTAX ERROR CODE AND BACKOUT
C ANY PREVIOUS BETS ACCOMPANING THIS ONE THAT HAVE ALREADY
C BEEN POSTED TO THE POOLS
C
               IF(PERLIM.GT.CALPER(P(TSPPER))) THEN
	          SYNTERRCOD=270
	          NOBETS=BETNUM-1
	          POLCAN=.TRUE.
	          GOTO 1041
	       ENDIF
	   ENDIF

C
C Check if combination is closed
C
	   IF(ROWCNT.LE.3.AND..NOT.POLCAN) THEN
     	     IF(HASH_DIR_SUP(1,TSLOFF).GT.P(TSMXLI)) THEN
		SYNTERRCOD=7
		NOBETS=BETNUM-1
		POLCAN=.TRUE.
		GOTO 1041
             ENDIF
	   ENDIF
           IF(ROWCNT.GT.3.AND.PSTAT.EQ.HASH_RETURN_OK.AND..NOT.POLCAN) THEN
              IF(HASH_TAB_SUP(1,NEXT_RECORD,NEXT_BLOCK).GT.P(TSMXLI)) THEN
                 SYNTERRCOD=707
                 NOBETS=BETNUM-1
                 POLCAN=.TRUE.
                 GOTO 1041
              ENDIF
           ENDIF
C
	   CALL HASHPUT(TSLOFF,ROWCNT,AMOUNT,SAME_RECORD,
     *	                NEXT_RECORD,NEXT_BLOCK,PSTAT)
	   CALL TOPTSL(TSLOFF,AMOUNT,GIND,POLCAN,ROWCNT,
     *		       ROWNUMS,ROWCOMB,TAMTLIAB)
C
C Close combinations and record times when limits have beeen exceeded
C
	TAMTLIAB = TAMTLIAB*100
	IF(TAMTLIAB.GT.P(TSMXLI)) THEN
	    IF(ROWCNT.LE.3) THEN
	        HASH_DIR_SUP(2,TSLOFF)=TIME+DAYCDC*SEC_DAY
	    ELSEIF(PSTAT.EQ.HASH_RETURN_OK) THEN
		HASH_TAB_SUP(2,NEXT_RECORD,NEXT_BLOCK)=TIME+DAYCDC*SEC_DAY
	    ENDIF
	ENDIF
	IF(ROWCNT.LE.3) THEN
     	    HASH_DIR_SUP(1,TSLOFF)=TAMTLIAB
	ELSEIF(PSTAT.EQ.HASH_RETURN_OK) THEN
	    HASH_TAB_SUP(1,NEXT_RECORD,NEXT_BLOCK)=TAMTLIAB
	ENDIF
C
	   CALL AGTPUT(TSLOFF,AMT,TER,ROWCNT,NEWAMT,STATUS)

	   IF(STATUS.EQ.HASH_RETURN_OK) THEN
	   CALL TOPATS(TSLOFF,NEWAMT,GIND,POLCAN,ROWCNT,TER,TIME,
     *			  ROWNUMS,ROWCOMB)
C
C SECURITY LEVEL 2:  SEND A WARNING MESSAGE TO THE CONSOLE
C
	      IF(NEWAMT.LT.P(TSLWRN)) GOTO 1040       !CHECK IF LEVEL REACHED
	      IF(POLCAN) GOTO 1040			!NO MESS IF POLCAN
	      IF((NEWAMT-AMT).GE.P(TSLWRN)) GOTO 1040 !JUST ONE MESS
	      MESS(1)=ODD
	      MESS(2)=TEGEN
	      MESS(3)=23
	      CALL FASTMOV(GTNAMES(TTSL),MESS(4),2)
	      MESS(6)=TER
	      CALL FASTMOV(BLANK,MESS(7),6)
	      DO 1060 I=0,ROWCNT-1
	          WRITE (CMESS(7+I),'(I2.2,A1,A1)') ROWNUMS(I),'-',
     *	                                           CONVERT(ROWCOMB(I))
1060	      CONTINUE
	      CALL QUEMES(MESS)
	   ENDIF
C
1040	CONTINUE
	GOTO 1025
C
C EXPAND SYSTEM BETS
C
1030	CONTINUE
	CALL FASTSET(0,ROWNUMS,6)
	CALL FASTSET(0,ROWCOMB,6)
	SYS=0
	AMOUNT=0
	SIMPLE=TRABUF(TWSYSN)
	SELECT=TRABUF(TWTSEL1)
	IF(SIMPLE.EQ.3.AND.SELECT.EQ.4) SYS=1
	IF(SIMPLE.EQ.3.AND.SELECT.EQ.5) SYS=2
	IF(SIMPLE.EQ.3.AND.SELECT.EQ.6) SYS=3
	IF(SIMPLE.EQ.4.AND.SELECT.EQ.5) SYS=4
	IF(SIMPLE.EQ.4.AND.SELECT.EQ.6) SYS=5
C
	ROWCNT=SIMPLE
	COUNT=CMB(SYS)
	DO 1015 I=1,COUNT
	   DO 1005 J=1,SIMPLE
	      IND=TAB(J,I,SYS)-1
	      ROWNUMS(J-1) = TRABUF(TWTROW1+IND*TWTBLEN)
	      ROWCOMB(J-1) = CNVTBL(TRABUF(TWTPOL1+IND*TWTBLEN))
1005	   CONTINUE
	   CALL ODDSOFF(ROWNUMS,ROWCOMB,ROWCNT,TSLOFF)
	   CALL HASHGET(TSLOFF,ROWCNT,AMOUNT,NEXT_RECORD,
     *	                NEXT_BLOCK,PSTAT)
	   AMT = TRABUF(TWTAMT1+BETIND)/TSLPRC(GIND)
C
C UPDATE NUMBER OF PLAYED COMBINATIONS TODAY
C
	   TSLCMB(GIND)=TSLCMB(GIND)+1
	   IF(POLCAN)TSLCMB(GIND)=TSLCMB(GIND)-2
	   IF(POLCAN) AMT = -AMT
	   AMOUNT = AMOUNT + AMT
	   IF(DAYTYP(DOLAMT,1,GNUM).GT.P(TSPMIN).AND.
     *	      .NOT.POLCAN.AND.P(TSPMIN).NE.0)THEN
	      PERLIM=(AMOUNT*TSLPRC(GIND))/DAYTYP(DOLAMT,1,GNUM)
	      IF(PERLIM.GT.CALPER(P(TSPPER))) THEN
	         SYNTERRCOD=271
                 TRABUF(TSTAT)=REJT
                 TRABUF(TERR)=LIAB
	         RETURN
	      ENDIF
	   ENDIF
C
C Check if combination is closed
C
	IF(ROWCNT.LE.3.AND..NOT.POLCAN) THEN
     	   IF(HASH_DIR_SUP(1,TSLOFF).GT.P(TSMXLI)) THEN
                SYNTERRCOD=709
                NOBETS=BETNUM-1
                POLCAN=.TRUE.
                GOTO 1041
           ENDIF
        ENDIF
 
     	IF(ROWCNT.GT.3.AND.PSTAT.EQ.HASH_RETURN_OK.AND..NOT.POLCAN) THEN
     	   IF(HASH_TAB_SUP(1,NEXT_RECORD,NEXT_BLOCK).GT.P(TSMXLI)) THEN
		SYNTERRCOD=708
		NOBETS=BETNUM-1
		POLCAN=.TRUE.
		GOTO 1041
           ENDIF
	ENDIF
C
	CALL HASHPUT(TSLOFF,ROWCNT,AMOUNT,SAME_RECORD,
     *	                NEXT_RECORD,NEXT_BLOCK,PSTAT)
	CALL TOPTSL(TSLOFF,AMOUNT,GIND,POLCAN,ROWCNT,
     *			ROWNUMS,ROWCOMB,TAMTLIAB)
C
C Close combinations and record times when limits have beeen exceeded
C
	TAMTLIAB = TAMTLIAB*100
	IF(TAMTLIAB.GT.P(TSMXLI)) THEN
	    IF(ROWCNT.LE.3) THEN
	        HASH_DIR_SUP(2,TSLOFF)=TIME+DAYCDC*SEC_DAY
	    ELSEIF(PSTAT.EQ.HASH_RETURN_OK) THEN
		HASH_TAB_SUP(2,NEXT_RECORD,NEXT_BLOCK)=TIME+DAYCDC*SEC_DAY
	    ENDIF
	ENDIF
	IF(ROWCNT.LE.3) THEN
     	    HASH_DIR_SUP(1,TSLOFF)=TAMTLIAB
	ELSEIF(PSTAT.EQ.HASH_RETURN_OK) THEN
	    HASH_TAB_SUP(1,NEXT_RECORD,NEXT_BLOCK)=TAMTLIAB
	ENDIF
C
	   CALL AGTPUT(TSLOFF,AMT,TER,ROWCNT,NEWAMT,STATUS)
	   IF (STATUS.EQ.HASH_RETURN_OK) THEN
	      CALL TOPATS(TSLOFF,NEWAMT,GIND,POLCAN,ROWCNT,TER,TIME,
     *			ROWNUMS,ROWCOMB)
C
C SECURITY LEVEL 2:  SEND A WARNING MESSAGE TO THE CONSOLE
C
	       IF (NEWAMT.LT.P(TSLWRN)) GOTO 1015       !CHECK IF LEVEL REACHED
	       IF (POLCAN) GOTO 1015			!NO MESS IF POLCAN
	       IF ((NEWAMT-AMT).GE.P(TSLWRN)) GOTO 1015 !JUST ONE MESS
	       MESS(1)=ODD
	       MESS(2)=TEGEN
	       MESS(3)=23
	       CALL FASTMOV(GTNAMES(TTSL),MESS(4),2)
	       MESS(6)=TER
	       CALL FASTMOV(BLANK,MESS(7),6)
	       DO 1070 K=0,ROWCNT-1
	          WRITE (CMESS(7+K),'(I2.2,A1,A1)') ROWNUMS(K),'-',
     *	                                           CONVERT(ROWCOMB(K))
1070	       CONTINUE
	       CALL QUEMES(MESS)
	   ENDIF
1015	CONTINUE
C
C TOTO SELECT ROW POOLS
C
1025	CONTINUE
	DO 1020 I=0,TRABUF(TWNBET)-1
	   BETIND = (I*TWTBLEN*TWTRMAX) + (I*TWTHLEN)
	   CNT=1
	   AMT=6*TRABUF(TWTAMT1+BETIND)/TRABUF(TWTSEL1+BETIND)
	   IF(TRABUF(TWSYSN).NE.0) THEN
	      COMBIN=FASTBIN(TRABUF(TWTSEL1+BETIND),TRABUF(TWSYSN))
	      CNT=(COMBIN*TRABUF(TWSYSN))/TRABUF(TWTSEL1+BETIND)
	      AMT=6*TRABUF(TWTAMT1+BETIND)/TRABUF(TWSYSN)
	      AMT=AMT*CNT
	   ENDIF
	   IF(POLCAN) THEN
	      CNT=-CNT
	      AMT=-AMT
	   ENDIF
	   DO 1010 J=0,TRABUF(TWTSEL1+BETIND)-1
	      TRAIND = BETIND + TWTBLEN*J
	      POLTYP=TRABUF(TWTPOL1+TRAIND)
	      ROW=TRABUF(TWTROW1+TRAIND)
	      TSPPOL(TRACNT,ROW,POLTYP,GIND)=
     *	      TSPPOL(TRACNT,ROW,POLTYP,GIND)+CNT
	      TSPPOL(DOLAMT,ROW,POLTYP,GIND)=
     *	      TSPPOL(DOLAMT,ROW,POLTYP,GIND)+AMT
1010	   CONTINUE
1020	CONTINUE
	RETURN
C
C WINNERS TIP TRANSACTION
C
2000	CONTINUE
	DO 2010 I=0,TRABUF(TWNBET)-1
	   ROW=TRABUF(TWWROW+I*TWWBLEN)
	   AMT=TRABUF(TWWAMT+I*TWWBLEN)
	   IF(POLCAN) AMT=-AMT
	   WTPOOL(ROW,WPAMNT,WPDYNM,GIND)=
     *	   WTPOOL(ROW,WPAMNT,WPDYNM,GIND)+AMT
2010	CONTINUE
	RETURN
C
C SCORE TRANSACTION
C
3000	CONTINUE
	IF(TRABUF(TWSYST).EQ.1) THEN               !FULL SYSTEM BET
	   AMT=TRABUF(TWSAMT)
	   IF(POLCAN) AMT=-AMT
	   DO 3030 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWSSCR1+I*TWSBLEN)
	      IF(HOMSCR.EQ.'FF'X) GOTO 3099
	      DO 3010 J=0,TRABUF(TWNBET)-1
	         AWYSCR=TRABUF(TWSSCR2+J*TWSBLEN)
	         IF(AWYSCR.EQ.'FF'X) GOTO 3020
	         CALL POLIND(HOMSCR,AWYSCR,INDEX)
	         SCPOOL(INDEX,SPAMNT,SPDYNM,GIND)=
     *	         SCPOOL(INDEX,SPAMNT,SPDYNM,GIND)+AMT
3010	      CONTINUE
3020	   CONTINUE
3030	   CONTINUE
	ELSE                                       !SIMPLE BET
	   DO 3040 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWSSCR1+I*TWSBLEN)
	      AWYSCR=TRABUF(TWSSCR2+I*TWSBLEN)
	      AMT=TRABUF(TWSAMT+I*TWSBLEN)
	      IF(POLCAN) AMT=-AMT
	      CALL POLIND(HOMSCR,AWYSCR,INDEX)
	      SCPOOL(INDEX,SPAMNT,SPDYNM,GIND)=
     *	      SCPOOL(INDEX,SPAMNT,SPDYNM,GIND)+AMT
3040	   CONTINUE
	ENDIF
3099	CONTINUE
	RETURN
C
C DOUBLE TRANSACTION
C
4000	CONTINUE
	IF(TRABUF(TWSYST).EQ.FULSYS) THEN               !FULL SYSTEM BET
	   AMT=TRABUF(TWDBAMT)
	   IF(POLCAN) AMT=-AMT
	   DO 4030 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWDBROW1+I*TWDBBLEN)
	      IF(HOMSCR.EQ.'FF'X) GOTO 4099
	      DO 4010 J=0,TRABUF(TWNBET)-1
	         AWYSCR=TRABUF(TWDBROW2+J*TWDBBLEN)
	         IF(AWYSCR.EQ.'FF'X) GOTO 4020
		 IF(HOMSCR.EQ.AWYSCR) GOTO 4010
                 INDEX = MAXDBLRW*(HOMSCR-1) + AWYSCR
		 IF(INDEX.LE.0 .OR. INDEX.GT.MAXDBLRW*MAXDBLRW) THEN
                    SYNTERRCOD    = 5000
		    TRABUF(TSTAT) = REJT
                    TRABUF(TERR)  = SYNT
		    RETURN
		 ENDIF
	         DBPOOL(INDEX,DBLPAMNT,DBLPDYNM,GIND)=
     *	         DBPOOL(INDEX,DBLPAMNT,DBLPDYNM,GIND)+AMT
4010	      CONTINUE
4020	   CONTINUE
4030	   CONTINUE
	ELSE                                       !SIMPLE BET
	   DO 4040 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWDBROW1+I*TWDBBLEN)
	      AWYSCR=TRABUF(TWDBROW2+I*TWDBBLEN)
	      AMT=TRABUF(TWDBAMT+I*TWDBBLEN)
	      IF(POLCAN) AMT=-AMT
              INDEX = MAXDBLRW*(HOMSCR-1) + AWYSCR
              IF(INDEX.LE.0 .OR. INDEX.GT.MAXDBLRW*MAXDBLRW) THEN
                 SYNTERRCOD    = 5001
		 TRABUF(TSTAT) = REJT
                 TRABUF(TERR)  = SYNT
		 RETURN
	      ENDIF
	      DBPOOL(INDEX,DBLPAMNT,DBLPDYNM,GIND)=
     *	      DBPOOL(INDEX,DBLPAMNT,DBLPDYNM,GIND)+AMT
4040	   CONTINUE
	ENDIF
4099	CONTINUE
	RETURN
C
C COUPLE TRANSACTION
C
5000	CONTINUE
	IF(TRABUF(TWSYST).EQ.FULSYS) THEN               !FULL SYSTEM BET
	   AMT=TRABUF(TWCPAMT)
	   IF(POLCAN) AMT=-AMT
	   DO 5030 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWCPROW1+I*TWCPBLEN)
	      IF(HOMSCR.EQ.'FF'X) GOTO 5099
	      DO 5010 J=0,TRABUF(TWNBET)-1
	         AWYSCR=TRABUF(TWCPROW2+J*TWCPBLEN)
	         IF(AWYSCR.EQ.'FF'X) GOTO 5020
                 INDEX = (MAXCPLRW/2)*(HOMSCR-1) + AWYSCR
		 IF(INDEX.LE.0 .OR. INDEX.GT.MAXCPLRW/2*MAXCPLRW/2) THEN
                    SYNTERRCOD    = 5002
		    TRABUF(TSTAT) = REJT
                    TRABUF(TERR)  = SYNT
		    RETURN
		 ENDIF
	         CPPOOL(INDEX,CPLPAMNT,CPLPDYNM,GIND)=
     *	         CPPOOL(INDEX,CPLPAMNT,CPLPDYNM,GIND)+AMT
5010	      CONTINUE
5020	   CONTINUE
5030	   CONTINUE
	ELSE                                       !SIMPLE BET
	   DO 5040 I=0,TRABUF(TWNBET)-1
	      HOMSCR=TRABUF(TWCPROW1+I*TWCPBLEN)
	      AWYSCR=TRABUF(TWCPROW2+I*TWCPBLEN)
	      AMT=TRABUF(TWCPAMT+I*TWCPBLEN)
	      IF(POLCAN) AMT = -AMT
              INDEX = (MAXCPLRW/2)*(HOMSCR-1) + AWYSCR
	      IF(INDEX.LE.0 .OR. INDEX.GT.(MAXCPLRW/2)*(MAXCPLRW/2)) THEN
                 SYNTERRCOD    = 5003
		 TRABUF(TSTAT) = REJT
                 TRABUF(TERR)  = SYNT
		 RETURN
              ENDIF
	      CPPOOL(INDEX,CPLPAMNT,CPLPDYNM,GIND)=
     *	      CPPOOL(INDEX,CPLPAMNT,CPLPDYNM,GIND)+AMT
5040	   CONTINUE
	ENDIF
5099	CONTINUE
	RETURN
C
C Super Score
C
6000	CONTINUE
	CALL SSCTOP(TRABUF)
	RETURN
C
C Today's Trio
C
7000	CONTINUE
	CALL TRPTOP(TRABUF)
	RETURN
C
C Super Triple
C
8000	CONTINUE
	CALL STRTOP(TRABUF)
	RETURN
	END
