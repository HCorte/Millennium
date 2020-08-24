C
C SUBROUTINE TKTREQ
C
C V13 18-MAR-1999 RXK Game type/game index change. Hack for V5 removed.
C   
C     Rev 1.0   17 Apr 1996 15:34:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.8   05 May 1995 16:24:36   HXK
C  V5 entered into database again!!!!
C  
C     Rev 1.8   22 Feb 1995 21:31:22   HXK
C  HACK FOR V5
C  
C     Rev 1.7   15 Oct 1994 16:40:28   HXK
C  Adding /developing Bingo (15.Oct.94)
C  
C     Rev 1.6   17 Aug 1993 15:31:34   HXK
C  fix for mktmes rev bugs
C  
C     Rev 1.5   04 Aug 1993 12:23:22   CXK
C  CHANGED MESSAGE LENGTHS SUCH THAT INDIVIDUAL MESSAGE LINE
C  LENGTHS ARE SENT TO TERMINAL
C  
C     Rev 1.4   27 Jul 1993 20:02:22   GXA
C  Corrected starting point of message decoding and maximised row length to 30.
C  
C     Rev 1.3   12 Jul 1993 12:09:10   GXA
C  Allowed for Ticket requests for opinion poll tickets. (Not nice!)
C  
C     Rev 1.2   28 Jun 1993 17:29:20   HXK
C  changed err message length from 5 to 6
C  
C     Rev 1.1   13 Jun 1993 14:58:50   HXK
C   added AGTINF.DEF, changed TKTMFL to TKTMLN
C  
C     Rev 1.0   21 Jan 1993 17:51:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_tktreq.for **
C
C TKTREQ.FOR
C
C V01 26-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C
C SUBROUTINE TO PROCESS TICKET MESSAGE REQUEST
C
C CALLING SEQUENCE:
C      CALL TKTREQ(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TKTREQ(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
	INTEGER*4 I4TEMP
	BYTE      MESTAB(*),I1TEMP(4)
	EQUIVALENCE(I4TEMP,I1TEMP)
 
	INTEGER*4 MYCHKSUM, CHKLEN, IND, ERRTYP, GNUM, GIND, GTYP
        INTEGER*4 TKT_LINE_LENGTH
        INTEGER*4 I
	INTEGER*4 LEN
	INTEGER*2 OUTLEN
	DATA ERRTYP/Z90/

        TKT_LINE_LENGTH=(TICKET_LENGTH*4)-2

C
C DECODE TRANSACTION
C
	GTYP=ZEXT( MESTAB(5) )
	GIND=ZEXT( MESTAB(6) )

	IF(GTYP.GT.MAXTYP.OR.GTYP.LE.0) TRABUF(TERR)=SYNT
C
C INSTANT TICKETS AND OPINION POLLS TOGETHER CAN HAVE LARGER GAME INDEX THEN 6.
C
	IF(GTYP.NE.TINS.AND.(GIND.GT.MAXIND.OR.GIND.LT.1)) THEN
	   TRABUF(TERR) = SYNT
	ELSE
	   IF(GIND.LT.1.OR.GIND.GT.PRM_NUMOPN+PRM_NUMINS)
     *	   TRABUF(TERR) = SYNT
	ENDIF
C
	IF(TRABUF(TERR).NE.NOER) GOTO 1000
	TRABUF(TGAMTYP)=GTYP
	TRABUF(TGAMIND)=GIND
C
C GET GAME NUMBER, ALIGN FOR INSTANT TICKETS AND OPINION POLLS
C
	IF(GTYP.NE.TINS) THEN
	   GNUM=GTNTAB(GTYP,GIND)
	ELSE
	   GIND = GIND - PRM_NUMINS
	   GNUM = MAXGAM + GIND
	ENDIF
C
	IF(GNUM.EQ.0) THEN
	  TRABUF(TERR)=SYNT
          GOTO 1000
	ENDIF
C
        IF(GNUM.GT.MAXGAM+PRM_NUMOPN) THEN  !ensure gnum is in range
          TRABUF(TERR)=SYNT
          GOTO 1000
        ENDIF
C
C CHECK IF THERE IS A TICKET MESSAGE FOR THIS GAME
C
	IF(TKTMLN(GNUM).EQ.0) THEN
	  TRABUF(TERR)=INVL
	  GOTO 1000
	ENDIF
C
C IF AGENT CAN GET TICKET MESSAGES
C BUILD OUTPUT MESSAGE BACK TO TERMINAL
C
	IF(TSBIT(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTKM)) THEN
	  IND = 7
	  LEN = TKT_LINE_LENGTH*TKTMLN(GNUM)
	  I4TEMP = TKTMRV(GNUM)
	  MESTAB(IND+0) = I1TEMP(1)
	  MESTAB(IND+1) = I1TEMP(2)
	  IND = IND + 2
	  MESTAB(IND+0) = LEN
	  IND = IND + 1
          DO I=1,TKTMLN(GNUM)
 	    CALL MOVBYT(TKTMES(1,I,GNUM),1,MESTAB,IND,
     *                  TKT_LINE_LENGTH)
            IND=IND+TKT_LINE_LENGTH
          ENDDO
C
C TERMINATOR
C
	  MESTAB(IND) = '00'X
	  IND = IND + 1
	  OUTLEN = IND - 1
	  GOTO 9000
	ELSE
	  TRABUF(TERR) = SUPR
	ENDIF
C
C RETURN ERROR
C
1000	CONTINUE
	TRABUF(TSTAT)=REJT
	MESTAB(2) = ERRTYP
	MESTAB(5) = TRABUF(TERR)
	MESTAB(6) = 0
	OUTLEN=6
C
C CALCULATE CHECKSUM AND RETURN
C
9000	CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN = OUTLEN - 1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
	RETURN
	END
