C
C SUBROUTINE CMDGEN
C
C CMDGEN.FOR
C
C V13 29-NOV-2000 UXN TOTOGOLA ADDED.
C V12 13-OCT-1999 RXK World Tour added.
C V11 13-MAY-1999 UXN Super Triple added.
C V10 01-FEB-1997 RXK Changes for CDU.
C V09 23-JAN-1996 HXK Fix for MIN and MOV commands (caused by typo)
C V08 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V07 10-SEP-1993 HXK Put in Winners reserve fund functions
C V06 12-JUL-1993 GXA Added Ticket Message Revision update command.
C V05 12-JUL-1993 HXK added TICKET TEXT (marketing message) commands
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 31-DEC-1992 TD   ONLY CALL CHKSYS IF SYSTEM IS FROZEN, THIS ELIMINATES
C		       THE NEED FOR A DUMMY CHKSYS ROUTINE FOR LINKING WITH
C		       RESET.
C V02 02-DEC-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C                      ADDED REDMIN PARAMETER
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS GENERAL SYSTEM COMMANDS
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CMDGEN(TRABUF,MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:RWFCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKPNT.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4 MESS(EDLEN), CMDNUM, FILE, GTYP, GIND, VALDOL
        INTEGER*4 GAM, LIN, OFF, DIV, AMOUNT, OLDAMT, INDEX
        INTEGER*4 BLANKS
	DATA FILE/0/
        DATA BLANKS/'    '/
C
C
C
	CMDNUM=TRABUF(TCMNUM)
	GOTO (10,20,30,40,50,60,70,80,90,100,110,120) CMDNUM
	GOTO 1000
C
C SYSTEM CHECKPOINT
C
10	CONTINUE
	TRABUF(TCMOLD)=FILE
	FILE=MOD(FILE,CHKTOT)+1
	TRABUF(TCMNEW)=FILE

C	ONLY CALL CHKSYS IF SYSTEM IS FROZEN, THIS WILL OCCUR ONLY IF
C	CMDGEN WAS CALLED BY THE TASK CMDPRO.  OTHERWISE IF NETMGR OR
C	RESET HAS CALLED CMDGEN NO NEED TO CALL CHKSYS

	IF (P(CMDFRZ) .NE. 0) CALL CHKSYS(FILE)
	IF(FILE.NE.TRABUF(TCMNEW)) THEN
	    FILE=TRABUF(TCMOLD)
            MESS(2)=TEGEN
            MESS(3)=2
	ELSE
	    MESS(2)=TEGEN
	    MESS(3)=1
	    MESS(8)=FILE
	    MESS(9)=NXTSER
	ENDIF
	RETURN
C
C CHANGE DAYSTS
C
20	CONTINUE
	TRABUF(TCMOLD)=DAYSTS
	DAYSTS=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=2
	MESS(8)=TRABUF(TCMOLD)
	MESS(9)=TRABUF(TCMNEW)
	RETURN
C
C CHANGE REDMAX
C
30	CONTINUE
        GIND=TRABUF(TCMDT1)
 	TRABUF(TCMOLD)=REDMAX(GIND)
	REDMAX(GIND)=TRABUF(TCMNEW)
        MESS(2)=TECMD
        MESS(3)=2
        MESS(8)=VALDOL(TRABUF(TCMOLD))
        MESS(9)=VALDOL(TRABUF(TCMNEW))
        RETURN
C
C CHANGE REDMIN
C
40	CONTINUE
	GIND = TRABUF(TCMDT1)
	TRABUF(TCMOLD) = REDMIN(GIND)
	REDMIN(GIND) = TRABUF(TCMNEW)
	MESS(2) = TECMD
	MESS(3) = 2
	MESS(8) = VALDOL(TRABUF(TCMOLD))
	MESS(9) = VALDOL(TRABUF(TCMNEW))
	RETURN
C
C CHANGE TICKET MESSAGE
C
50      CONTINUE
        GAM=TRABUF(TCMTER)
        LIN=TRABUF(TCMLIN)
        OFF=TRABUF(TCMNEW)
        CALL FASTMOV(TRABUF(TCMDT1),TKTMES(OFF,LIN,GAM),4)
        MESS(2)=TEGEN
        IF(GAM.LE.MAXGAM+PRM_NUMOPN) THEN
           MESS(3)=25
           MESS(8)=GAM
        ELSE
           MESS(3)=43
           MESS(8)=GAM-(MAXGAM+PRM_NUMOPN)
        ENDIF
        MESS(9)=LIN
        MESS(10)='1-4 '
        IF(OFF.NE.1) MESS(10)='5-8 '
        RETURN
C
C ENABLE TICKET MESSAGE
C
60      CONTINUE
        GAM=TRABUF(TCMTER)
        TKTMLN(GAM)=TRABUF(TCMNEW)
        MESS(2)=TEGEN
        IF(GAM.LE.MAXGAM+PRM_NUMOPN) THEN
           MESS(3)=26
           MESS(8)=GAM
        ELSE
           MESS(3)=44
           MESS(8)=GAM-(MAXGAM+PRM_NUMOPN)
        ENDIF
        RETURN
C
C DISABLE TICKET MESSAGE
C
70      CONTINUE
        GAM=TRABUF(TCMTER)
        CALL FASTSET(BLANKS,TKTMES(1,1,GAM),24)
        TKTMLN(GAM)=0
        MESS(2)=TEGEN
        IF(GAM.LE.MAXGAM+PRM_NUMOPN) THEN
           MESS(3)=27
           MESS(8)=GAM
        ELSE
           MESS(3)=45
           MESS(8)=GAM-(MAXGAM+PRM_NUMOPN)
        ENDIF
        RETURN
C
C SET TICKET TEXT REVISION #
C
80	CONTINUE
	GAM = TRABUF(TCMTER)
	TKTMRV(GAM) = TRABUF(TCMNEW)
	MESS(2) = TEGEN
        IF(GAM.LE.MAXGAM+PRM_NUMOPN) THEN
           MESS(3)=28
           MESS(8)=GAM
        ELSE
           MESS(3)=46
           MESS(8)=GAM-(MAXGAM+PRM_NUMOPN)
        ENDIF
	MESS(9) = TRABUF(TCMNEW)
	RETURN
C
C CHANGE OFFLINE PURGE AMOUNT FOR GAME
C
90      CONTINUE
        GAM=TRABUF(TCMTER)
        IF(GAM.LT.1.OR.GAM.GT.MAXGAM) RETURN
        RWF_WRFGAM(OFPAMT,GAM)=TRABUF(TCMNEW)
        MESS(2)=TEGEN
        MESS(3)=29
        MESS(8)=GAM
        MESS(9)=TRABUF(TCMNEW)
        RETURN
C
C CHANGE ADVANCE SHARE AMOUNT BY GAME
C
100     CONTINUE
        GAM=TRABUF(TCMTER)
        DIV=TRABUF(TCMDT1)
        OLDAMT=0
        AMOUNT=TRABUF(TCMNEW)
        IF(GAM.LT.1.OR.GAM.GT.MAXGAM) RETURN
        GTYP=GNTTAB(GAMTYP,GAM)
        GIND=GNTTAB(GAMIDX,GAM)
        IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.
     *     GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.GTYP.EQ.TBNG.OR.
     *     GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR) RETURN
        IF(GIND.LT.1.OR.GIND.GT.MAXIND) RETURN
        IF(GTYP.EQ.TLTO) THEN
          OLDAMT=LTOASH(DIV,GIND)
          LTOASH(DIV,GIND)=AMOUNT
        ENDIF
        IF(GTYP.EQ.TSPT) THEN
          OLDAMT=SPTASH(DIV,GIND)
          SPTASH(DIV,GIND)=AMOUNT
        ENDIF
        IF(GTYP.EQ.TTGL) THEN
          OLDAMT=TGLASH(DIV,GIND)
          TGLASH(DIV,GIND)=AMOUNT
        ENDIF
        IF(GTYP.EQ.TKIK) THEN
          OLDAMT=KIKASH(DIV,GIND)
          KIKASH(DIV,GIND)=AMOUNT
        ENDIF
        RWF_WRFGAM(USEAMT,GAM)=RWF_WRFGAM(USEAMT,GAM)-OLDAMT+AMOUNT
        MESS(2)=TEGEN
        MESS(3)=30
        MESS(8)=GAM
        MESS(9)=DIV
        MESS(10)=AMOUNT
        RETURN

C
C CHANGE MINIMUM POOL AMOUNT BY GAME
C
110     CONTINUE
        GAM=TRABUF(TCMTER)
        AMOUNT=TRABUF(TCMNEW)
        IF(GAM.LT.1.OR.GAM.GT.MAXGAM) RETURN
        GTYP=GNTTAB(GAMTYP,GAM)
        GIND=GNTTAB(GAMIDX,GAM)
        IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.
     *     GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *     GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR) RETURN
        IF(GIND.LT.1.OR.GIND.GT.MAXIND) RETURN
        IF(GTYP.EQ.TLTO) LTOMIN(GIND)=AMOUNT
        IF(GTYP.EQ.TSPT) SPTMIN(GIND)=AMOUNT
        IF(GTYP.EQ.TTGL) TGLMIN(GIND)=AMOUNT
        IF(GTYP.EQ.TKIK) KIKMIN(GIND)=AMOUNT
        IF(GTYP.EQ.TBNG) BNGMIN(GIND)=AMOUNT
        MESS(2)=TEGEN
        MESS(3)=31
        MESS(8)=GAM
        MESS(9)=AMOUNT
        RETURN
C
C CHANGE WINSEL DRAW STATUS FOR MULTIWIN PROCEDURES
C
120     CONTINUE
        GAM = TRABUF(TCMDT2)
        INDEX = TRABUF(TCMDT1)
	TRABUF(TCMOLD) = DRWSTS(INDEX, GAM)
        DRWGAM(INDEX, GAM) = TRABUF(TCMDT3)
        DRWSTS(INDEX, GAM) = TRABUF(TCMNEW)
        MESS(2) = TECMD
        MESS(3) = 35
C        MESS(6) = TRABUF(TCMDT4)
        MESS(8) = TRABUF(TCMOLD)
        MESS(9) = TRABUF(TCMNEW)
        RETURN
C
C PUT NEXT GENERAL COMMAND HERE
C


C
C INVALID COMMAND NUMBER
C
1000	CONTINUE
	TRABUF(TSTAT)=REJT
	TRABUF(TERR)=INVL
	MESS(2)=TECMD
	MESS(3)=1
	MESS(4)=TRABUF(TCMTYP)
	MESS(5)=TRABUF(TCMNUM)
	RETURN
	END
