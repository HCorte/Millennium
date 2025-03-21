C
C SUBROUTINE GETOFF
C
C GETOFF.FOR
C 
C V16 31-MAR-2010 RXK  ePassive changes
C V15 18-SEP-2009 FJG  Fix OFFBEG/OFFEND FF LOT2 Offset
C V14 29-NOV-2000 UXN  TOTOGOLO ADDED.
C V13 28-FEB-2000 RXK  Promotion ("add 1 free week") added.
C V12 13-OCT-1999 RXK  World Tour added.
C V11 14-MAY-1999 UXN  Super Triple added.
C V10 23-NOV-1995 HXK  Merge of post 65 stuff; changes for Double/Couple
C V09 01-NOV-1994 HXK  added Bingo
C V08 20-AUG-1993 HXK  added 'ff' to prevent printing info of game that is 
C                      no longer valid on terminal ticket, e.g. jokeri on 
C                      Viking when jokeri game is finished.
C V07 04-AUG-1993 SXH  Fixed bug with second RAVI offset
C V06 23-JUL-1993 HXK  REMOVED NETHERLAND'S SPORTS CDC OFFSET BUG
C V05 17-JUL-1993 GXA  Released for Finland Dec Conversion / Oddset.
C V04 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V03 09-JUL-1992 GCAN ADDED TOTO SELECT, WIN TIP AND SCORE OFFSETS.
C		       FOR TOTO SELECT BEGOFF WILL BE DRAW OFFSET FOR THE
C                      EARLIEST DRAWN ROW AND ENDOFF FOR THE LAST DRAWN
C                      ROW ON THIS TICKET.
C V02 01-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SUBROUTINE TO CALCULATE DRAW DATE OFFSETS FOR ALL GAMES.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETOFF(TRABUF,OFFBEG,OFFEND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
C
	INTEGER*4   IND, DRAW, GIND
	INTEGER*4   OFFEND, OFFBEG
	INTEGER*4   FROW		!Lowest Row Number on Ticket.
	INTEGER*4   TROW		!Highest Row Number on Ticket.
	INTEGER*4   NBRD		!Number of Boards on Ticket.
	INTEGER*4   SELOFF(3)		!Selection Offsets in Destra.
	INTEGER*4   ROWOFF(3)		!Row Offsets in Destra.
        INTEGER*4   YEAR,EMIS
C
	DATA SELOFF /TWTSEL1,TWTSEL2,TWTSEL3/
	DATA ROWOFF /TWTROW1,TWTROW2,TWTROW3/
C
	IF(TRABUF(TGAMTYP).EQ.TLTO) THEN !Totoloto
	  GIND=TRABUF(TGAMIND)
	  DRAW=LTODRW(GIND) !LTODRW - Loto Draw deve ser uma tabela/matrix que apartir do GAME INDEX obtém o DRAW 
C=V15===========================================================================
C         IF(TRABUF(TWEND)+TRABUF(TWADDFW).LT.DRAW) THEN
C           OFFBEG='FF'X
C           OFFEND='FF'X
C         ELSE
C=V15===========================================================================
  	    IND=TRABUF(TWBEG)-DRAW+1  	  
  	    IF(IND.EQ.0) THEN           ! POSTPONED DRAWS
             OFFBEG=(LTOBSD(GIND)+(LTODAT(1,GIND) - LTOESD(GIND))-1)-DAYCDC  	    	
  	    ELSE
  	     OFFBEG=LTODAT(IND,GIND)-DAYCDC
  	    ENDIF  	     	 		    
            IND=TRABUF(TWEND)+TRABUF(TWADDFW)-DRAW+1
            IF(IND.EQ.0) THEN
             OFFEND=(LTOBSD(GIND)+(LTODAT(1,GIND) - LTOESD(GIND))-1)-DAYCDC  	    		            
            ELSE
	     OFFEND=LTODAT(IND,GIND)-DAYCDC
	    ENDIF 	    
C         ENDIF                       ! V15
	  RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TSPT) THEN !Totobola
	  GIND=TRABUF(TGAMIND) !Game index
	  DRAW=SPTDRW(GIND) !SPTDRW - SPORT Draw deve ser uma tabela/matrix que apartir do GAME INDEX obtém o DRAW 
	  IND=TRABUF(TWBEG)-DRAW+1 !index <-> starting draw
	  OFFBEG=SPTDAT(IND,GIND)-DAYCDC! SPTDAT = DRAW DATES
	  IND=TRABUF(TWEND)-DRAW+1
	  OFFEND=SPTDAT(IND,GIND)-DAYCDC
	  RETURN
	ENDIF
C
	IF(TRABUF(TGAMTYP).EQ.TTGL) THEN !Totogolo (descontinuado)
	  GIND=TRABUF(TGAMIND)
	  DRAW=TGLDRW(GIND)
	  IND=TRABUF(TWBEG)-DRAW+1
	  OFFBEG=TGLDAT(IND,GIND)-DAYCDC
	  IND=TRABUF(TWEND)-DRAW+1
	  OFFEND=TGLDAT(IND,GIND)-DAYCDC
	  RETURN
	ENDIF
C
	IF(TRABUF(TGAMTYP).EQ.TPAS) THEN !Passive - LN (descontinuado)
	  GIND=TRABUF(TGAMIND)
          YEAR=MOD(TRABUF(TWEPYR),10)
          EMIS = PASEXTDRW(TRABUF(TWEPWK),YEAR,GIND)  
	  OFFBEG=PASESD(EMIS,GIND)-DAYCDC
	  OFFEND=OFFBEG
	  RETURN
	ENDIF
C
        IF(TRABUF(TGAMTYP).EQ.TBNG) THEN !Bingo nunca usado pela SCML
          GIND=TRABUF(TGAMIND)
          DRAW=BNGDRW(GIND)
          IND=TRABUF(TWBEG)-DRAW+1
          OFFBEG=BNGDAT(IND,GIND)-DAYCDC
          IND=TRABUF(TWEND)-DRAW+1
          OFFEND=BNGDAT(IND,GIND)-DAYCDC
          RETURN
        ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TKIK) THEN !Joker (descontinuado)
	  GIND=TRABUF(TGAMIND)
	  DRAW=KIKDRW(GIND)
	  IND=TRABUF(TWKBEG)-DRAW+1
	  OFFBEG=KIKDAT(IND,GIND)-DAYCDC
          IND=TRABUF(TWKEND)+TRABUF(TWADDFW)-DRAW+1
	  OFFEND=KIKDAT(IND,GIND)-DAYCDC
	  RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TNBR) THEN !Numbers Games (nunca usado pela SCML)
	  GIND=TRABUF(TGAMIND)
	  DRAW=NBRDRW(GIND)
	  IND=TRABUF(TWBEG)-DRAW+1
	  OFFBEG=NBRDAT(IND,GIND)-DAYCDC
	  IND=TRABUF(TWEND)-DRAW+1
	  OFFEND=NBRDAT(IND,GIND)-DAYCDC
	  RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TWIT) THEN !Winners Tip Game (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = WITDAT(GIND) - DAYCDC
	   OFFEND = WITDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TSCR) THEN !SCORE GAME TYPE (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = SCRDAT(GIND) - DAYCDC
	   OFFEND = SCRDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TTSL) THEN !Toto select game  (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   FROW = TRABUF(TWTROW1)
	   NBRD = TRABUF(TWNBET) 
	   IND  = (TRABUF(SELOFF(NBRD))-1) * TWTBLEN
	   TROW = TRABUF(ROWOFF(NBRD) + IND)
	   OFFBEG = TSLDAT(FROW,GIND) - DAYCDC
	   OFFEND = TSLDAT(TROW,GIND) - DAYCDC
	   RETURN
	ENDIF
C
	IF(TRABUF(TGAMTYP).EQ.TDBL) THEN ! (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = DBLDAT(GIND) - DAYCDC
	   OFFEND = DBLDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TCPL) THEN ! (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = CPLDAT(GIND) - DAYCDC
	   OFFEND = CPLDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TSSC) THEN ! (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = SSCDAT(GIND) - DAYCDC
	   OFFEND = SSCDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
C
	IF(TRABUF(TGAMTYP).EQ.TTRP) THEN ! (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = TRPDAT(GIND) - DAYCDC
	   OFFEND = TRPDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
	IF(TRABUF(TGAMTYP).EQ.TSTR) THEN ! (nunca usado pela SCML)
	   GIND = TRABUF(TGAMIND)
	   OFFBEG = STRDAT(GIND) - DAYCDC
	   OFFEND = STRDAT(GIND) - DAYCDC
	   RETURN
	ENDIF
C
	OFFBEG=0
	OFFEND=0
	RETURN
	END
