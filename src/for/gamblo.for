C
C SUBROUTINE GAMBLO
C  
C V02 08-NOV-99 RXK There are possible more than 1 game block segments.
C V01 21-MAY-96 HXK Initial revision.
C  
C GAMBLO.FOR
C
C CALLING SEQUENCE:
C      CALL GAMBLO(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
C Game block request Terminal --> Host
C -------------------------------------
C 
C Start Byte   End Byte	  Size	  Contents
C ------------------------------------------------------------------------
C          1 -        1      1    Control (0010)      , Sequence #
C          2 -        2      1    Type = 3            , Subtype = 6
C          3 -        4      2    Checksum
C          5 -        5      1    Segment number
C ------------------------------------------------------------------------
C
C Game block Host --> Terminal
C------------------------------
C
C Start Byte   End Byte   Size    Contents
C ------------------------------------------------------------------------
C	   1 -        1	     1	  Controll (0010)    , Sequence #
C          2 -        2      1    Type = 3           , Subtype = 6
C          3 -        4      2    Checksum
C          5 -        5      1    Number of On-line Games in this segment
C          6 -        6      1    Number of Remaining Games 
C
C          7 -        7      1    Game Type          , Game Index
C          8 -        9      2    Game Option Flags
C         10 -       11      2    Control Revision (if bit is set)
C         12 -       13      2    Text Revision    (if bit is set)
C         14 -       15      2    Ticket Text Revision (if bit is set).
C         ...       ... 
C -------------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GAMBLO(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
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
C
	BYTE	    MESTAB(*)		    !terminal Message Table.
C
	INTEGER*4   IND			    !Index into Message Table.
        INTEGER*4   TER                     !Terminal Number
	INTEGER*4   ACTGMS                  !Active Games.
	INTEGER*4   GNUM		    !Game Number.
	INTEGER*4   I			    !Loop variable.
	INTEGER*4   CHKLEN		    !Length of Message to Checksum
	INTEGER*4   MYCHKSUM		    !Checksum of Message
	INTEGER*4   ACTTAB(MAXGAM)	    !Active Games Table.
	INTEGER*2   OUTLEN		    !Message Output length.
C 
        INTEGER*4   MSON                    !MAX Number of games in SON message
        INTEGER*4   MGAMBL                  !MAX Number of games in GAME BLOCK
        PARAMETER(MSON=22)                  !(256-54)/9
        PARAMETER(MGAMBL=27)                !(256-6)/9
        INTEGER*4   GAMSON                  !number of games in SON mess 
        INTEGER*4   GAMHERE                 !number of games in GAME BL. 
        INTEGER*4   GAMLEFT                 !number of games left
        INTEGER*4   SEGMENT                 !number of segment (SON not incl.)
        INTEGER*4   FROM,UNTIL
C
C GET SEGMENT NUMBER
C
	IND = 5
        SEGMENT = MESTAB(IND)
C
C SET / CLEAR VARIABLES
C
        TER=TRABUF(TTER)
C
C CREATE BITMAPS OF ACTIVE GAMES
C
	ACTGMS=0
	DO I=1,MAXGAM
	   IF(DAYDRW(I).GT.0) THEN
	     ACTGMS=ACTGMS+1
	     ACTTAB(ACTGMS)=I
	   ENDIF
	END DO
C
C NUMBER OF GAMES TO BE SENT IN THIS GAME BLOCK REQUEST
C
        GAMHERE = ACTGMS - (MSON + (SEGMENT-1)*MGAMBL)
        IF(GAMHERE.LE.0) RETURN
        IF(GAMHERE.GT.MGAMBL) GAMHERE = MGAMBL
	MESTAB(IND) = GAMHERE
	IND=IND+1
C
C NUMBER OF GAMES TO BE SENT IN NEXT GAME BLOCK REQUEST
C
        GAMLEFT = ACTGMS - (MSON+(SEGMENT-1)*MGAMBL+GAMHERE)
	MESTAB(IND) = GAMLEFT
	IND=IND+1
C
C LOOP THROUGH NUMBER OF ACTIVE GAMES
C
        FROM  = MSON+(SEGMENT-1)*MGAMBL+1
        IF(ACTGMS.LE.MSON+SEGMENT*MGAMBL) THEN
           UNTIL = ACTGMS 
        ELSE
           UNTIL = MSON+SEGMENT*MGAMBL
        ENDIF

	DO I=FROM,UNTIL
C
C GET GAME NUMBER FROM ACTIVE TABLE, THEN GET ALL GAME OPTIONS FOR EACH GAME.
C
	   GNUM = ACTTAB(I)
	   CALL GAME_OPTIONS(MESTAB,IND,TER,GNUM,GAME_OPTIONS_FROM_CNTRL)
C
	END DO
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
