C SUBROUTINE SON
C SON.FOR
C
C
C V33 10-MAR-2016 SCML M16 PROJECT: added new SM game and updated EM game
C V32 18-DEC-2015 FRP CR31 PIN for Owner and Clerk
C V31 14-DEC-2010 HXK LOTTO 2 (remove Lotto1 and Lotto2 from game blocks)
C V30 21-MAR-2005 FRP VALCDULI added for CDU Enhancements.
C V29 09-NOV-99 RXK OGQLIM added, numbers MSON and MGAMBL changed.
C V28 18-FEB-97 HXK Cleaned up hack for AGTXFR
C V26 07-FEB-97 HXK Hack for AGTXFR (temporarily using AGTHCH)
C V25 06-FEB-97 HXK Added AGXFR
C V24 28-JAN-97 RXK Temporary change to support SpectraI and SpectraIII on 
C                   same time removed
C V23 16-JAN-97 HXK Further fix for REDMIN (here it is REDIMN)
C V22 16-JAN-97 HXK Change REDMIN units 
C V21 13-JAN-97 RXK CDU, phase 2 
C V20 05-DEC-96 HXK Updated for Finland IPS
C V19 21-MAY-96 HXK Addition of segmented signon (Rita)
C V18 15-NOV-93 GXA Added Opinion poll revision numbers.
C V17 04-NOV-93 HXK CHANGES FOR 5 PENNY UNITS FOR REDMAX.
C V16 17-OCT-93 GXA Check agents auto invoice flag and the global auto invoice 
C                   flag for automatic invoice report.
C V15 15-JUL-93 GXA Check Pass# in Terminal pass# field instead of agent pass#
C                   field.
C V14 12-JUL-93 GXA Added Instant Ticket Revision # filed.
C V13 22-JUN-93 GXA Changed calling sequence to GAME_OPTIONS.
C V12 19-JUN-93 GXA Removed generic control info. and introduced GAME_OPTIONS
C                  subroutine to take care of the parts in common for all games.
C V11 19-JUN-93 GXA Released for Finland Dec Conversion / Oddset.
C V10 10-JUN 93 GXA Rearanged AGTCOM stuff.
C V09 21-JAN-93 DAB Initial Release Based on Netherlands Bible, 12/92, and Comm 1/93 update
C V08 07-AUG-92 GCAN CORRECTED START OFFSET FOR RIM ID'S AND LENGTH OF MOVE.
C V07 10-JUL-92 GCAN ADDED LOGIC FOR AGENT RAM SET CHANGE,
C                    INCLUDED TOTO SELECT IN THE ADVANCED DRAW LOGIC.
C V06 04-MAR-92 GCAN CHANGED WEDNESDAY DRAW / ADVANCED DRAW LOGIC.
C V05 13-FEB-92 GCAN ADDED CHECK IF TERMINAL CHECKSUM SUPRESSION.
C V04 09-JAN-92 GCAN ADDED SMART CARD LOGIC.
C		     ADDED STATISTICS BYTE TO TERMINAL INPUT MSG.
C V03 04-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V02 14-MAR-91 JPJ  INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C CALLING SEQUENCE:
C      CALL SON(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
C
C
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Sign-On Terminal --> Host
C -------------------------
C 
C Start Byte   End Byte	  Size	  Contents
C ------------------------------------------------------------------------
C          1 -        1      1    Control (0010)      , Sequence #
C          2 -        2      1    Type = 3            , Subtype = 0
C          3 -        4      2    Checksum
C	   5 -        5      1    Statistics Byte
C          6 -        9      4    Agent Number	         (NOT USED)
C         10 -       11      2    Agent Pass Number	 (NOT USED)
C         12 -       13      2    Terminal Pass Number   
C         14 -       21      8    Terminal   Hardware ID
C         22 -       29      8    Printer    Hardware ID
C         30 -       37      8    SSR	     Hardware ID
C         38 -       45      8    Smart Card Hardware ID
C         46 -       53      8    TV Control Hardware ID
C         54 -       61      8		     Hardware ID (NOT USED)          
C         62 -       65      4    CPU	     Rom Rev
C         66 -       69      4    Printer    Rom Rev
C         70 -       73      4    SSR	     Rom Rev
C         74 -       77      4    Smart Card Rom Rev
C         78 -       81      4    Tv Control Rom Rev
C         82 -       85      4		     Rom Rev     (NOT USED)
C         86 -       89      4		     Rom Rev     (NOT USED)
C         90 -       93      4		     Rom Rev     (NOT USED)
C         94 -       97      4		     Rom Rev     (NOT USED)
C         98 -      101      4		     Rom Rev     (NOT USED)
C        102 -      105      4		     Rom Rev     (NOT USED)
C        106 -      109      4		     Rom Rev     (NOT USED)
C
C ------------------------------------------------------------------------
C
C Sign-On Host --> Terminal
C--------------------------
C
C Start Byte   End Byte   Size    Contents
C ------------------------------------------------------------------------
C	   1 -        1	     1	  Controll (0010)    , Sequence #
C          2 -        2      1    Type = 3           , Subtype = 0
C          3 -        4      2    Checksum
C          5 -        7      3    Time (seconds since midnight)
C          8 -        9      2    Terminal Number
C         10 -       11      2    Application Revision number
C         12 -       12      1    Sign-On Count
C         13 -       14      2    CDC Date
C         15 -       15      1    Current Day of the Week (1 = Mon, 2 = Tues..)
C         16 -       19      4    Agent number
C         20 -       20      1    Clerk ID , Terminal Language
C         21 -       22      2    Terminal Option Flags
C         23 -       24      2    Wager Units
C         25 -       26      2    Validation Units
C         27 -       28      2    CDU Text Revision (All)
C         29 -       32      4    Redemption Minimum (Amount, Instant&Regular)
C         33 -       36      4    Redemption Maximum (Amount, Instant&Regular)
C         37 -       38      2    Second Teller Password (CR31 PIN for Owner and Clerk)
C         39 -       40      2    Not used
C         41 -       48      8    Valid Opinion ID #'s. (2 bytes / ID). 
C         49 -       52      4    EURO rate
C         53 -       56      4    Amount Limit for CDU
C         57 -       57      1    Number of On-line Games
C
C         58 -       58      1    Number of On-line Games in SON message 
C         59 -       59      1    Game Type          , Game Index
C         60 -       61      2    Game Option Flags
C         62 -       63      2    Control Revision (if bit is set)
C         64 -       65      2    Text Revision    (if bit is set)
C         66 -       67      2    Ticket Text Revision (if bit is set).
c         ...        ...
C
C -------------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SON(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SPECOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
!	INCLUDE 'INCLIB:EUROCONFIG.DEF'                                               !V33
	INCLUDE 'INCLIB:EURCOM.DEF'                                                   !V33
C
C
C
C
C This flag indicates wheter the game options will be sent in SON (like other
C places) this will save on # of transactions that needs to go back and forth
C betwean Central and Terminal, OR
C whether the game options will be sent on the Control messages only (this will
C increase the # of transactions but is the only way when # of games increases
C beyond the 'normal' and the options/game will no longer fit in the SON message
C In Finlands case with 19 games and 27 bytes/game in options it has to come
C from the control messages (the flag MUST be set to .TRUE.).
C
        LOGICAL   GAME_OPTIONS_FROM_CNTRL/.TRUE./!If options comes from CNTRL
C
	INTEGER*4 REV, REL, TEMP, TEMP1
	INTEGER*4 BYT2, BYT1
C
	BYTE	    MESTAB(*)		    !terminal Message Table.
	BYTE	    STABYT		    !Statistics Byte.
C
        INTEGER*4 MAXSIGON                  !Maximum Sign On Counter
C
        PARAMETER(MAXSIGON = 2)             !Maximum Sign On Counter
C
	INTEGER*4   PASSOFFSET		    !Cleark Passnumber Offset
	INTEGER*4   ANUM		    !Agent Number
	INTEGER*4   TER			    !Terminal Number
	INTEGER*4   PNUM		    !Terminal Pass Number
	INTEGER*4   IND			    !Index into Message Table.
	INTEGER*4   ACTGMS                  !Active Games.
	INTEGER*4   GNUM		    !Game Number.
	INTEGER*4   I			    !Loop variable.
	INTEGER*4   CHKLEN		    !Length of Message to Checksum
	INTEGER*4   MYCHKSUM		    !Checksum of Message
	INTEGER*4   CRDPASOK		    !Card Used Pass# Matches.
	INTEGER*4   CRDPASNO		    !Card Used Pass# does NOT Match.
	INTEGER*4   NOCRDUSD		    !No Card Used.                   
	INTEGER*4   MESS(EDLEN)		    !Message buffer for ERRLOG
	INTEGER*4   ACTTAB(MAXGAM)	    !Active Games Table.
	INTEGER*2   OUTLEN		    !Message Output length.
	INTEGER*2   SGNTAB(NUMAGT)	    !SON count table by agent.
	INTEGER*2   DATE(12)		    !Date Table.

	CHARACTER   CTEMP(4)
	EQUIVALENCE (TEMP,CTEMP)
C
        LOGICAL	    PREPRT/.FALSE./
	LOGICAL	    PASMATCH		    !Pass# Sent and Central Matches.
C 
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C EURO MIL PROJECT - INCLUDE VAR FOR EURO SIGN ON
C	
!        COMMON ECFREC                                                          !V33 - COMMENT OUT
!        BYTE EUROGTGI                                                          !V33 - COMMENT OUT
!        PARAMETER (EUROGTGI = '89'X)                                           !V33 - COMMENT OUT
!        BYTE EUROGAMEOPT                                                       !V33 - COMMENT OUT
!        PARAMETER (EUROGAMEOPT = '04'X)                                        !V33 - COMMENT OUT
!        BYTE EUROGAMEREV1,EUROGAMEREV2                                         !V33 - COMMENT OUT
!        PARAMETER (EUROGAMEREV1 = '12'X)                                       !V33 - COMMENT OUT
!        PARAMETER (EUROGAMEREV2 = '11'X)                                       !V33 - COMMENT OUT
        
	DATA PASSOFFSET/0/
C
C TERMINAL OPTION FLAGS
C
	INTEGER*2   REQINV_OPT  /Z8000/	    !Request Invoice at first SON.
	INTEGER*2   PRTON_OPT   /Z4000/     !Preprint ON.
	INTEGER*2   TSTMOD_OPT  /Z2000/	    !Test Game.
	INTEGER*2   PRVAGT_OPT  /Z1000/     !Priviliged Agent.
	INTEGER*2   CHKOFF_OPT  /Z0800/     !Checksumming OFF.
	INTEGER*2   BETDON_OPT  /Z0400/     !Print Bet Details on Reprints.
	INTEGER*2   XFRENAB_OPT /Z0200/     !Instant Transfer enabled Agent
C
	DATA CRDPASOK/Z00/		    !Card Used Pass# Matches.
	DATA CRDPASNO/Z01/		    !Card Used Pass# does NOT Match.
	DATA NOCRDUSD/Z02/		    !No Card Used.
	DATA SGNTAB/NUMAGT*0/
                                                                !**SON**...
        INTEGER*4   MSON                    !MAX Number of games in SON message
        INTEGER*4   MGAMBL                  !MAX Number of games in GAME BLOCK
        PARAMETER(MSON=22)                  !(256-58)/9
        PARAMETER(MGAMBL=27)                !(256-6)/9
        INTEGER*4   GAMSON                  !actual number of games in SON mess 
        INTEGER*4   GAMLEFT                 !actual number of games in GAME BL. 
C
        INTEGER*4 EACTGMS                                                       !V33 - EUROMILLIONS SYSTEM ACTIVE GAMES
        INTEGER*4 EACTTAB(EMAXGAM)                                              !V33 - EUROMILIIONS SYSTEM ACTIVE GAMES TABLE
        INTEGER*4 EGAMSON                                                       !V33 - EUROMILLIONS SYSTEM GAMES SON
        INTEGER*4 EGTYP                                                         !V33 - GAME TYPE
        INTEGER*4 EGIND                                                         !V33 - GAME INDEX
        INTEGER*4 EGNUM                                                         !V33 - EUROMILLIONS SYSTEM GAME NUMBER
C
C SET / CLEAR VARIABLES
C
	PASMATCH = .FALSE.
	IND = 5	
C
C GET STATISTICS BYTE
C
	STABYT = MESTAB(IND+0)
	IND = IND + 1 !IND=6
C
C GET AGENT NUMBER
C
	I4TEMP=0
	I1TEMP(4) = MESTAB(IND+0) !IND=6
	I1TEMP(3) = MESTAB(IND+1) !IND=7
	I1TEMP(2) = MESTAB(IND+2) !IND=8
	I1TEMP(1) = MESTAB(IND+3) !IND=9
	ANUM = I4TEMP
	IND=IND+4 !IND=10
C
C GET PASS NUMBER
C
C ********* ENCPRO KNOWS WHERE THE PASSWORD IS ***********
C	    YOU MUST CHANGE IT (IN ENCSGNON) IF ITS LOCATION IN THE
C	    MESSAGE CHANGES ***************
C
C
C CHANGED TO LOOK AT TERMINAL PASS#
C
	IND = IND + 2 !IND=12
C
	I4TEMP=0
	I1TEMP(2) = MESTAB(IND+0) !IND=12
	I1TEMP(1) = MESTAB(IND+1) !IND=13
	PNUM      = I4TEMP
	IND=IND+2 !IND=14
C
C CHECK PASS NUMBER FROM AGENT
C
	TER=TRABUF(TTER)
C
C IF SECURITY VIOLATION THAN DO NOT SIGNON TERMINAL
C
	IF(AGTHTB(AOPSTS,TER).EQ.SERSOF) THEN
	  TRABUF(TERR) = TSEC
	  TRABUF(TSTAT) = REJT
	  MESTAB(1) = '20'X+TRABUF(TTRN)
	  MESTAB(2) = '90'X
	  MESTAB(5) = TRABUF(TERR)
	  MESTAB(6) = 0
	  OUTLEN = 6
	  MESS(1) = SPE
	  MESS(2) = TEGEN
	  MESS(3) = 15
	  MESS(4) = TER
	  MESS(5) = PNUM
	  CALL QUEMES(MESS)
	  RETURN
	ENDIF
C
C INCREASE NUMBER OF SIGN ON COUNTER
C
	SGNTAB(TER) = SGNTAB(TER) + 1
C
C IF MAIN PASSNUMBER IS SHUT OFF THEN DO NOT ALLOW ANY CLERK SIGNON
C OR AGENT NUMBER ENTERED INCORRECT
C
	IF(AGTTAB(APSNUM, TER) .EQ. 0 .OR. ANUM .NE. AGTTAB(AGTNUM, TER)) THEN
	  TRABUF(TSTAT)=REJT
C	  TRABUF(TERR)=INVL
	  TRABUF(TERR)= BTOPSN
	  MESTAB(1) = '20'X+TRABUF(TTRN)
	  MESTAB(2) = '90'X
	  MESTAB(5) = TRABUF(TERR)
	  MESTAB(6) = 0
	  OUTLEN=6
	  TRABUF(TSOLD) = AGTHTB(AOPSTS,TER)
	  TRABUF(TSNEW) = SIGNOF
	  AGTHTB(AOPSTS,TER) = SIGNOF
          IF(SGNTAB(TER) .GT. MAXSIGON) GOTO 1000          
	  RETURN
	ENDIF
C
	IF(P(CLRKACT).EQ.0) THEN
C	   DO I=0,NUMCLERK-1
	   DO I=0,0  !only check with first password (PAS1), since this is the real agent password
	      IF(PNUM.EQ.AGTTAB(APSNUM+I,TER)) THEN
	         PASSOFFSET=I
	         GOTO 60
	      ENDIF
	   END DO
	  TRABUF(TERR) = INVL
	ELSE
	  PASSOFFSET=0
	  IF(PNUM.EQ.AGTTAB(APSNUM,TER)) PASMATCH = .TRUE.
	  IF(STABYT.EQ.CRDPASOK.AND.PASMATCH)  GOTO 60
	  IF(STABYT.EQ.NOCRDUSD.AND.PASMATCH.AND.
     *	     TSBIT(AGTTAB(AGTTYP,TER),AGTSON))	GOTO 60
	  IF(STABYT.EQ.CRDPASNO.AND.PASMATCH.AND.
     *	     TSBIT(AGTTAB(AGTTYP,TER),AGTSON))  GOTO 60
	  TRABUF(TERR) = INVL
	  IF(STABYT.EQ.CRDPASNO)		TRABUF(TERR) = INCRD
	  IF(STABYT.EQ.NOCRDUSD.AND.PASMATCH)	TRABUF(TERR) = NOCRD
	ENDIF
C
C CHECK IF WE HAVE ERROR PASSWORD
C
        IF(TRABUF(TERR) .EQ. INVL .AND. PASMATCH .EQ. .FALSE.) THEN
          TRABUF(TERR) = BTOPSN
        ENDIF
C  
C SEND ERROR MESSAGE TO TERMINAL 
C
	TRABUF(TSTAT)=REJT
	MESTAB(1) = '20'X+TRABUF(TTRN)
	MESTAB(2) = '90'X
	MESTAB(5) = TRABUF(TERR)
	MESTAB(6) = 0
	OUTLEN=6
	TRABUF(TSOLD)=AGTHTB(AOPSTS,TER)
C
C CHECK IF TERMINAL NUMBER HAS TRY TO DO SOME SIGN ONS
C
1000    CONTINUE
	IF(SGNTAB(TER) .GT. MAXSIGON) THEN
	  MESS(1)=SPE
	  MESS(2)=TEGEN
	  MESS(3)=14
	  MESS(4)=TER
	  CALL QUEMES(MESS)
	  AGTHTB(AOPSTS,TER)=SERSOF
	  TRABUF(TSNEW)=SERSOF
	  SGNTAB(TER) = 0
	ELSE
	  AGTHTB(AOPSTS,TER)=SIGNOF
	  TRABUF(TSNEW)=SIGNOF
	ENDIF
	RETURN
C
60	CONTINUE
C
C STUFF TRABUF WITH SON INFO
C GET ROM ID REVS. AND HARDWARE REVS. (REFER TO MESSAGE FORMATS)
C
	SGNTAB(TER)=0
	CALL MOVBYT(MESTAB,14,TRABUF(TSOLD),1,TRALEN-TSOLD)   !REV/ID NUMBERS
C
C SIGNON AGENT AND BUILD SIGNON MESSAGE
C
	AGTHTB(AOPSTS,TER)=SIGNON
	AGTHTB(ASONCT,TER)=AGTHTB(ASONCT,TER)+1
	AGTTAB(AGTSC2,TER)=0
	AGTHTB(AGTPASOFF,TER)=PASSOFFSET+1
	TRABUF(TSSGN)=PASSOFFSET+1
	AGTTAB(AGTOCL,TER)=AGTTAB(AGTNCL,TER)
	AGTTAB(AGTNCL,TER)=TRABUF(TSSGN)
	CALL ENCINI1(AGTTAB(APSNUM+PASSOFFSET,TER),TER)
	IF(P(CLRKACT).EQ.0) THEN
	  CALL INICLERK(TRABUF(TSER),TER,AGTTAB(AGTOCL,TER),
     *	                AGTTAB(AGTNCL,TER))
	ENDIF
C
	IND = 1
	MESTAB(IND) = '20'X + TRABUF(TTRN)
	IND=IND+1
	MESTAB(IND) = '30'X
	IND=5
C
C GET CURRENT TIME
C
	I4TEMP = TRABUF(TTIM)
	MESTAB(IND+0) = I1TEMP(3)
	MESTAB(IND+1) = I1TEMP(2)
	MESTAB(IND+2) = I1TEMP(1)
	IND=IND+3 !IND=8
C
C GET TERMINAL NUMBER
C
	I4TEMP = TRABUF(TTER)
	MESTAB(IND+0) = I1TEMP(2) !IND=8
	MESTAB(IND+1) = I1TEMP(1) !IND=9
	IND=IND+2 !IND=10
C
C GET REVISION NUMBER
C
        TEMP = P(ROMREV)
	IF(CTEMP(2).EQ.'0'.OR.CTEMP(2).EQ.' '.OR.TEMP.EQ.0) THEN
	   MESTAB(IND+0) = 0 !IND=10
	   MESTAB(IND+1) = 0 !IND=11
	   IND=IND+2 !IND=12
	ELSE
	   READ  (CTEMP(1),900) BYT1
	   READ  (CTEMP(2),900) BYT2
	   TEMP1=BYT1*16+BYT2
	   CALL NMOVBYT(TEMP1,4,MESTAB,IND,1)
	   IND=IND+1
	   READ  (CTEMP(3),900) REL
	   READ  (CTEMP(4),900) REV
900	   FORMAT(Z1)
	   TEMP=REL*16+REV
	   CALL NMOVBYT(TEMP,4,MESTAB,IND,1)
	   IND=IND+1
	ENDIF
C
C GET SIGNON COUNT FOR G-GUARD NUMBER ALGORITHM
C
	I4TEMP = AGTHTB(ASONCT,TER)
	MESTAB(IND) = I1TEMP(1) !IND=12
	IND=IND+1 !IND=13
C
C GET CDC NUMBER
C
	I4TEMP = DAYCDC
	MESTAB(IND+0) = I1TEMP(2) !IND=13
	MESTAB(IND+1) = I1TEMP(1) !IND=14
	IND=IND+2 !IND=15
C
C GET DAY OF WEEK
C
	DATE(VCDC)=DAYCDC
	CALL CDATE(DATE)
	I4TEMP=DATE(VDOW)
	MESTAB(IND)=I1TEMP(1) !IND=15
	IND=IND+1 !IND=16
C
C GET AGENT NUMBER
C
	I4TEMP = AGTTAB(AGTNUM,TER)
	MESTAB(IND+0) = I1TEMP(4)
	MESTAB(IND+1) = I1TEMP(3)
	MESTAB(IND+2) = I1TEMP(2)
	MESTAB(IND+3) = I1TEMP(1)
	IND=IND+4
C
C GET CLERK NUMBER AND TERMINAL LANGUAGE
C
	I4TEMP = ISHFT(AGTTAB(AGTNCL,TER),4)+AGTHTB(AGTLANG,TER)
	MESTAB(IND) = I1TEMP(1)
	IND=IND+1
C
C SET OPTION FLAGS
C
	I4TEMP = 0
C
C PRIV TERMINAL FLAG
C
	IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) I4TEMP = I4TEMP + PRVAGT_OPT
C
C TEST MODE FLAG
C
	IF(P(TSTMOD).EQ.1) I4TEMP = I4TEMP + TSTMOD_OPT
C
C PREPRINT FLAG
C
	IF(PREPRT) I4TEMP = I4TEMP + PRTON_OPT
C
C CHECKSUMMING OFF
C
	IF(P(SUPSUM).NE.0) I4TEMP = I4TEMP + CHKOFF_OPT   !Checksuming OFF.
C
C DISPLAY BET DETAILES ON REPRINTS
C
	IF(P(DSPBDET).EQ.0) I4TEMP = I4TEMP + BETDON_OPT  !Display Bet Detailes.
C
C INSTANT TRANSFER ENABLED FLAG
C
	IF(TSBIT(AGTTAB(AGTTYP,TER),AGTXFR)) I4TEMP = I4TEMP + XFRENAB_OPT
C
C REQUEST INVOICE REPORT ON FIRST SON.
C
	IF(P(AUTOINV).EQ.0.AND.AGTHTB(ASONCT,TER).EQ.1.AND.
     *     AGTHTB(AINRPT,TER).EQ.1) 
     *     I4TEMP = I4TEMP + REQINV_OPT		      !Request Invoice Report.
C	
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND = IND + 2
C
C SET WAGER UNITS
C
	I4TEMP = DYN_BETUNIT
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND = IND + 2
C
C SET VALIDATION UNITS
C
	I4TEMP = DYN_VALUNIT
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND = IND + 2
C
C CDU TEXT REVISION 
C
	I4TEMP = TKTCDU
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND = IND + 2
C
C REDEMPTION MINIMUM FOR INSTANT AND REGULAR VALIDATIONS
C
	IF(AGTTAB(AGTRMN, TER) .NE. 0) THEN
	   I4TEMP = AGTTAB(AGTRMN, TER)
	ELSE
	   I4TEMP = P(REDIMN)
	ENDIF
	MESTAB(IND + 0) = I1TEMP(4)
	MESTAB(IND + 1) = I1TEMP(3)
	MESTAB(IND + 2) = I1TEMP(2)
	MESTAB(IND + 3) = I1TEMP(1)
	IND = IND + 4
C
C REDEMPTION MAXIMUM FOR INSTANT AND REGULAR VALIDATIONS
C
	IF(AGTTAB(AGTRMX, TER) .NE. 0) THEN
	   I4TEMP = AGTTAB(AGTRMX, TER)
	ELSE
	   I4TEMP = P(REDDEF)
	ENDIF
	MESTAB(IND + 0) = I1TEMP(4)
	MESTAB(IND + 1) = I1TEMP(3)
	MESTAB(IND + 2) = I1TEMP(2)
	MESTAB(IND + 3) = I1TEMP(1)
	IND = IND + 4
C
C LIMIT FOR ODDSET GAMES TICKET PRICE CONFIRMATION QUESTION
C
C	I4TEMP = P(OGQLIM)*DYN_BETUNIT/DOLL_BASE     !marks 
C	MESTAB(IND+0) = I1TEMP(4)
C	MESTAB(IND+1) = I1TEMP(3)
C	MESTAB(IND+2) = I1TEMP(2)
C	MESTAB(IND+3) = I1TEMP(1)
C	IND = IND + 4
C
C SECOND TELLER PASSWORD (CR31 - PASSWORD FOR MANAGER), STORED IN PAS8 FROM ASFINF (SEE UPDASF)
C
	I4TEMP = AGTTAB(APSNUM+7,TER)
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND=IND+2
C
C NOT USED
C
	I4TEMP = 0
	MESTAB(IND+0) = I1TEMP(2)
	MESTAB(IND+1) = I1TEMP(1)
	IND=IND+2
C
C VALID OPINION POLL REVISION NUMBERS
C
	DO I = 1,PRM_NUMOPN
	   CALL OPNPOL_CNTRL(I,I4TEMP)
	   MESTAB(IND+0) = I1TEMP(2) 
	   MESTAB(IND+1) = I1TEMP(1)
	   IND = IND + 2                 
	END DO
C
C EURO EXCANGE RATE.
C
	I4TEMP = P(EUROCR)
	MESTAB(IND+0) = I1TEMP(4)
	MESTAB(IND+1) = I1TEMP(3)
	MESTAB(IND+2) = I1TEMP(2)
	MESTAB(IND+3) = I1TEMP(1)
	IND = IND + 4
C
C AMOUNT LIMIT FOR CDU.
C
	I4TEMP = P(VALCDULI)
	MESTAB(IND+0) = I1TEMP(4)
	MESTAB(IND+1) = I1TEMP(3)
	MESTAB(IND+2) = I1TEMP(2)
	MESTAB(IND+3) = I1TEMP(1)
	IND = IND + 4
C
C CREATE BITMAPS OF ACTIVE GAMES
C
	ACTGMS=0
	DO I=1,MAXGAM
	   IF(I.EQ.2.OR.I.EQ.4) CYCLE   ! ensure Lotto 1 and Lotto 2 inactive
	   IF(DAYDRW(I).GT.0) THEN
	     ACTGMS=ACTGMS+1
	     ACTTAB(ACTGMS)=I
	   ENDIF
	END DO
C
C EURO MIL PROJECT - INCLUDE ONE MORE ACTIVE GAME AND NUMBER OF GAMES
C
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD SM GAME TO ACTIVE GAMES
C----+---+-------------+------------------------------------------------
!        IF (ECFACTIVE .EQ. 0) THEN 
!           MESTAB(IND) = ACTGMS
!        ELSE
!	   MESTAB(IND) = ACTGMS + 1 !EURO
!        ENDIF
!	IND=IND+1
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD SM GAME TO ACTIVE GAMES
C----+---+-------------+------------------------------------------------
        EACTGMS = 0
        EGAMSON = 0
        DO I=1,EMAXGAM
          IF(I.EQ.RAF1GN) CYCLE                                                 !SoM NOT APPLICABLE
C
          EGTYP = EGNTTAB(GAMTYP,I)
          EGIND = EGNTTAB(GAMIDX,I)
          IF(EGTYP.EQ.TEUM) THEN
            IF(EGIND.GE.1 .AND. EGIND.LE.ENUMEUM) THEN
              IF(EUMGACTIVE(EGIND).NE.0) THEN
                EACTGMS = EACTGMS + 1
                EGAMSON = EGAMSON + 1
                EACTTAB(EACTGMS) = I
              ENDIF
            ENDIF
          ELSEIF(EGTYP.EQ.TRAF) THEN
            IF(EGIND.GE.1 .AND. EGIND.LE.ENUMRAF) THEN
              IF(RAFGACTIVE(EGIND).NE.0) THEN
                EACTGMS = EACTGMS + 1
                EGAMSON = EGAMSON + 1
                EACTTAB(EACTGMS) = I
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        MESTAB(IND) = ACTGMS + EACTGMS                                          !TOTAL NUMBER OF GAMES
        IND=IND+1
C----+---+-------------+------------------------------------------------
C V33|END| M16 PROJECT | ADD SM GAME TO ACTIVE GAMES
C----+---+-------------+------------------------------------------------
                                                                !**SON**...
C
C NUMBER OF GAMES TO BE SENT IN SON MESSAGE AND IN GAME BLOCK REQUEST
C
       GAMLEFT = ACTGMS - MSON
       IF(GAMLEFT.LT.0) THEN
          GAMSON = ACTGMS
          GAMLEFT = 0
       ELSE
          GAMSON = MSON
       ENDIF
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD SM GAME TO NUMBER OF GAMES SENT
C----+---+-------------+------------------------------------------------
!       IF (ECFACTIVE .EQ. 0) THEN 
!          MESTAB(IND) = GAMSON
!       ELSE
!          MESTAB(IND) = GAMSON + 1 !EURO 
!       ENDIF
!       IND=IND+1
        MESTAB(IND) = GAMSON + EGAMSON
        IND = IND + 1
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD SM GAME TO NUMBER OF GAMES SENT 
C----+---+-------------+------------------------------------------------
C
C EURO MIL PROJECT - INCLUDE GAME TYPE/INDEX, GAME OPTIONS AND GAME REV
C 
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | SON DATA OF EM AND SM GAMES
C----+---+-------------+------------------------------------------------
!       IF (ECFACTIVE .NE. 0) THEN 
!          MESTAB(IND) = ECFGTGISON
!          IND=IND+1
!          I4TEMP = ECFGOPTSON
!          MESTAB(IND) = I1TEMP(2)
!          IND=IND+1
!          MESTAB(IND) = I1TEMP(1)
!          IND=IND+1
!          I4TEMP = ECFGREVSON
!          MESTAB(IND) = I1TEMP(2)
!          IND=IND+1
!          MESTAB(IND) = I1TEMP(1)
!          IND=IND+1
!       ENDIF
C
C       GET GAME NUMBER FROM EUR ACTIVE TABLE, THEN GET ALL GAME OPTIONS
C       FOR EACH GAME.
C
        DO I=1,EGAMSON
          EGNUM = EACTTAB(I)
          EGTYP = EGNTTAB(GAMTYP,EGNUM)
          EGIND = EGNTTAB(GAMIDX,EGNUM)
          MESTAB(IND+0) = ISHFT(EGTYP,3) + EGIND                                !GAME TYPE/GAME INDEX
          IND = IND+1
          CALL EURGOPTSON(MESTAB, IND, EGNUM)
        ENDDO
C----+---+-------------+------------------------------------------------
C V52|END| M16 PROJECT | SON DATA OF EM AND SM GAMES
C----+---+-------------+------------------------------------------------
C
C LOOP THROUGH NUMBER OF ACTIVE GAMES
C
	DO I=1,GAMSON
C                                                               !...**SON**
C
C GET GAME NUMBER FROM ACTIVE TABLE, THEN GET ALL GAME OPTIONS FOR EACH GAME.
C
	   GNUM = ACTTAB(I)
	   CALL GAME_OPTIONS(MESTAB,IND,TER,GNUM,GAME_OPTIONS_FROM_CNTRL)
C
	END DO
C
C IF AGENT RAM SET WAS REQUESTED RESET PARAMETER.
C
	IF(TSBIT(AGTTAB(AGTTYP,TER),AGTRAM)) 
     *	   CALL BCLR(AGTTAB(AGTTYP,TER),AGTRAM)
C
	OUTLEN = IND - 1
C
C CALCULATE CHECKSUM AND RETURN
C
	I4CCITT = TRABUF(TCHK)
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	CHKLEN=OUTLEN-1
	CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
                                              
        RETURN                                                        
	END         
                    
                    
                    
                    
                    
