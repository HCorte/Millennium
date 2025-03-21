C
C PROGRAM ODSPRO
C
C ODSPRO.FOR
C 
C V09 25-JUL-2000 UXN  OPSTXT() ADDED.
C V08 14-MAY-1999 UXN  Super Triple added.
C V07 07-NOV-1995 HXK  Changes for Double, Couple install
C V06 01-FEB-1994 HXK  REMOVED CHECK FOR PRIMARY WHEN CALCULATING ODDS.
C V05 16-JAN-1994 HXK  removed odds broadcast.
C V04 13-JAN-1994 HXK  REMOVED ODDS BROADCAST FOR FINLAND.
C V03 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V02 28-APR-1992 GCAN CHANGED LOGIC. BROADCASTING IS DONE HERE IN ODSPRO
C                      INSTEAD OF IN CALODD. (CALODD NOW REUSABLE, (UNSPRO)).
C		       ALSO SET TTER TO ZERO TO INDICATE THAT THIS IS NOT
C                      A TERMINAL REQUESTED UPDATE.
C V01 22-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C THIS TASK IS IN CHARGE OF INITIATING THE BROADCASTING OF
C ODDS UPDATES TO ALL TERMINALS. THIS IS PERFORMED ON A USER
C SPECIFIED TIME INTERVAL P(ODSUPD). ODSPRO WILL FORCE THE
C LAST ODDS UPDATES OUT BEFORE GOING TO END OF TASK AT
C END OF DAY.
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM ODSPRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C
	INTEGER*4   GM2SND((NUMWIT+NUMSCR+NUMDBL+NUMCPL+
     *                      NUMSSC+NUMTRP+NUMSTR),2) 
                                              !Game To Send (Game Index,Type)
	INTEGER*4   OK2DIE		      !End of Day, Send All Odds Flag
	INTEGER*4   WTGIDX		      !Winners Tip Game Index
	INTEGER*4   WTGNUM		      !Winners Tip Game Number
	INTEGER*4   SCGIDX		      !Score Game Index
	INTEGER*4   SCGNUM		      !Score Game Number
	INTEGER*4   DBGIDX		      !Double Game Index
	INTEGER*4   DBGNUM		      !DoubleGame Number
	INTEGER*4   CPGIDX		      !Couple Tip Game Index
	INTEGER*4   CPGNUM		      !Couple Tip Game Number
	INTEGER*4   SSGIDX		      !Superscore Game Index
	INTEGER*4   SSGNUM		      !Superscore Game Number
	INTEGER*4   TRGIDX		      !Trio Game Index
	INTEGER*4   TRGNUM		      !Trio Game Number
	INTEGER*4   STGIDX		      !Super Triple Game Index
	INTEGER*4   STGNUM		      !Super Triple Game Number
	INTEGER*4   NM2SND		      !# Games to Send in One Batch
	INTEGER*4   MX2SND		      !Max # Games to Send
	INTEGER*4   TIME		      !Time in Sec. to Sleep
	INTEGER*4   IND			      !Index into Games to Send	
        INTEGER*4   I			      !Loop Variables
	INTEGER*4   ST			      !Subroutine Return Status
	INTEGER*4   BUF			      !Procom Buffer #.
C
	LOGICAL	    ALLFLG		      !Send All Odds Flag (True).
C
	CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C INITIALIZE AND SET LOCAL VARIABLES
C
	IND=0
	OK2DIE=0			      !SET OFF SUICIDE FLAG
	WTGIDX=0
	SCGIDX=0
	DBGIDX=0
	CPGIDX=0
	SSGIDX=0
	TRGIDX=0
	STGIDX=0
	NM2SND=3 !?????		      !# OF GAMES TO SND B4 WE SLEEP
C
C CALCUL.MAX GAMES 2 SND
C
	MX2SND=NUMWIT+NUMSCR+NUMDBL+NUMCPL+NUMSSC+NUMTRP+NUMSTR
	DO 5 I=1,MX2SND
	   GM2SND(I,GAMTYP)=0
	   GM2SND(I,GAMIDX)=0
5	CONTINUE
C
C GET NEXT SCORE GAME TO SEND OUT.  STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX.
C
100	CONTINUE
	SCGIDX=SCGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(SCGIDX.GT.NUMSCR) GOTO 200
	SCGNUM=GTNTAB(TSCR,SCGIDX)
	IF(SCGNUM.LT.1) GOTO 200
	IF(DAYDRW(SCGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TSCR
	   GM2SND(IND,GAMIDX)=SCGIDX
	ENDIF
C
C
C GET NEXT WIN TIP GAME TO SEND OUT. STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX
C
200	CONTINUE
	WTGIDX=WTGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(WTGIDX.GT.NUMWIT) GOTO 300
	WTGNUM=GTNTAB(TWIT,WTGIDX)
	IF(WTGNUM.LT.1) GOTO 300
	IF(DAYDRW(WTGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TWIT
	   GM2SND(IND,GAMIDX)=WTGIDX
	ENDIF
C
C
C GET NEXT DOUBLE GAME TO SEND OUT. STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX
C
300	CONTINUE
	DBGIDX=DBGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(DBGIDX.GT.NUMDBL) GOTO 400
	DBGNUM=GTNTAB(TDBL,DBGIDX)
	IF(DBGNUM.LT.1) GOTO 400
	IF(DAYDRW(DBGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TDBL
	   GM2SND(IND,GAMIDX)=DBGIDX
	ENDIF
C
C GET NEXT COUPLE GAME TO SEND OUT. STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX
C
400	CONTINUE
	CPGIDX=CPGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(CPGIDX.GT.NUMCPL) GOTO 420
	CPGNUM=GTNTAB(TCPL,CPGIDX)
	IF(CPGNUM.LT.1) GOTO 420
	IF(DAYDRW(CPGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TCPL
	   GM2SND(IND,GAMIDX)=CPGIDX
	ENDIF
C
C GET NEXT SUPERSCORE GAME TO SEND OUT.  STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX.
C
420	CONTINUE
	SSGIDX=SSGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(SSGIDX.GT.NUMSSC) GOTO 440
	SSGNUM=GTNTAB(TSSC,SSGIDX)
	IF(SSGNUM.LT.1) GOTO 440
	IF(DAYDRW(SSGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TSSC
	   GM2SND(IND,GAMIDX)=SSGIDX
	ENDIF
C
C GET NEXT TRIO GAME TO SEND OUT. STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX
C
440	CONTINUE
	TRGIDX=TRGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(TRGIDX.GT.NUMTRP) GOTO 460
	TRGNUM=GTNTAB(TTRP,TRGIDX)
	IF(TRGNUM.LT.1) GOTO 460
	IF(DAYDRW(TRGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TTRP
	   GM2SND(IND,GAMIDX)=TRGIDX
	ENDIF
C
C GET NEXT TRIPLE GAME TO SEND OUT. STUFF GM2SND WITH GAME TYPE
C AND GAME INDEX
C
460	CONTINUE
	STGIDX=STGIDX+1
	IF(WTGIDX.GT.NUMWIT .AND. SCGIDX.GT.NUMSCR .AND.
     *     DBGIDX.GT.NUMDBL .AND. CPGIDX.GT.NUMCPL .AND.
     *     SSGIDX.GT.NUMSSC .AND. TRGIDX.GT.NUMTRP .AND.
     *     STGIDX.GT.NUMSTR) GOTO 500
	IF(STGIDX.GT.NUMSTR) GOTO 100
	STGNUM=GTNTAB(TSTR,STGIDX)
	IF(STGNUM.LT.1) GOTO 100
	IF(DAYDRW(STGNUM).GT.0) THEN
	   IND=IND+1
	   GM2SND(IND,GAMTYP)=TSTR
	   GM2SND(IND,GAMIDX)=STGIDX
	ENDIF
C
	GOTO 100
C
500	CONTINUE
	IF(IND.EQ.0) THEN
	   CALL OPSTXT('No Score or Winners Tip games active today')
	   CALL OPSTXT('ODSPRO does not need to run')
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	MX2SND=IND
	IND=1
	IF(MX2SND.LT.NM2SND) NM2SND=MX2SND
C
C WAIT FOR P(ODSUPD) SECONDS BEFORE INITIATING ODDS
C BROADCAST. IF UPDATE TIME IS EQUAL TO ZERO WE DO NOT
C UPDATE OR BROADCAST ODDS.
C
C
1000	CONTINUE
	TIME = P(ODSUPD)
	IF(TIME.LE.0) TIME = 60
	IF(DAYSTS.NE.DSOPEN) THEN
	   OK2DIE = 1			      !SET SUICIDE FLAG `OK TO DIE`
	   NM2SND = MX2SND		      !SEND ALL ODD SETS OUT B4 WE DIE
	   IND = 1
	   TIME = 60
	ENDIF
        CALL XWAIT(TIME,2,ST)
        IF(P(ODSUPD).LE.0.AND.DAYSTS.EQ.DSOPEN) GOTO 1000
C
C GET BUFFER FOR SENDING BROADCAST  !DO NOT BROADCAST IN FINLAND
C
2000	CONTINUE
	CALL GETBUF(BUF)
	IF(BUF.EQ.0) GOTO 1000
C
C CALCULATE ODDS 
C
	IF(IND.GT.MX2SND) IND=1
	TRABUF(TTER)    = 0		      !NOT A TERMINAL REQUESTED UPDATE.
	TRABUF(TGAMTYP) = GM2SND(IND,GAMTYP)
	TRABUF(TGAMIND) = GM2SND(IND,GAMIDX)
	ALLFLG = .TRUE.
	IF(TSBIT(P(SUPRPT),ODSNEW)) ALLFLG = .FALSE.
        CALL CALODD(TRABUF,BPRO(BOUTTAB,BUF),HPRO(OUTLEN,BUF),ALLFLG)
        IND=IND+1
C
C
	CALL RELBUF(BUF)
C
C
C CHECK IF WE HAVE SENT OUT ALL ODDS (NM2SND). IF THE SYSTEM HAS BEEN
C SHUTDOWN THEN WE DIE. IF THE SYSTEM IS STILL UP WE GOTO SLEEP.
C
	IF(MOD(IND,NM2SND+1).EQ.0)THEN
	   IF(OK2DIE.EQ.0) THEN
              GOTO 1000			      !GOTO SLEEP
	   ELSE
	      CALL GSTOP(GEXIT_SUCCESS)	      !DIE
	   ENDIF
	ENDIF
C
        GOTO 2000			      !SEND OUT NEXT SET OF ODDS'
	END
