C
C SUBROUTINE INICLERK
C $Log:   GXAFXT:[GOLS]INICLERK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:36:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   10 Nov 1993 17:04:42   HXK
C  ADDED MISCELLANEOUS STUFF.
C  
C     Rev 1.1   11 Jun 1993 18:16:30   HXK
C  ADDED AGTINF.DEF, PRMAGT.DEF
C  
C     Rev 1.0   21 Jan 1993 16:39:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - spe_iniclerk.for **
C
C INICLERK.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 20-SEP-89 LOU R.    INITIAL RELEASE FOR FINLAND
C
C SUBROUTINE TO PAGE CURRENT CLERK IN MEMORY TO DISK AND RELOAD
C NEW ONE INTO MEMORY.
C
C CALLING SEQUENCE:
C      CALL INICLERK(SERIAL,TERMINAL,OLDCLERK,NEWCLERK)
C INPUT
C     SERIAL   - SERIAL NUMBER OF TRANSACTION UPDATING FILE
C     TERMINAL - TERMINAL NUMBER
C     OLDCLERK - OLD CLERK NUMBER TO SAVE
C     NEWCLERK - NEW CLERK NUMBER TO LOAD
C OUTPUT
C     NONE
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE INICLERK(SERIAL,TERMINAL,OLDCLERK,NEWCLERK)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
C
        INTEGER*4 MESS(10), ST, NEWCLERK, OLDCLERK, TERMINAL, SERIAL
        INTEGER*4 TMPSER
C
        TMPSER=MOD(SERIAL,SYSOFF)
        IF(TERMINAL.LE.0.OR.TERMINAL.GT.NUMAGT) GOTO 9000
        IF(OLDCLERK.EQ.NEWCLERK) GOTO 9000   !NO CHANGE IN MEMORY
        IF(OLDCLERK.LE.0.OR.OLDCLERK.GT.8.OR.NEWCLERK.LE.0.OR.
     *     NEWCLERK.GT.8) GOTO 9000          !SOMETHING WRONG
C
C READ CLERK FILE AND SWAP FOR NEW CLERK
C
        CALL READW(CLRKFDB,TERMINAL,CLRKREC,ST)
        IF(ST.NE.0) THEN
          MESS(1)=SPE
          MESS(2)=TEGEN
          MESS(3)=4
          CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
          MESS(9)=ST
          MESS(10)=TERMINAL
          CALL QUEMES(MESS)
          GOTO 9000
        ENDIF
C
C UPDATE HEADER IN RECORD
C
        CLRKHED(CLRKSER)=TMPSER       !SERIAL NUMBER PAGING DATA WITH
C
C MOVE MEMORY TO RECORD
C
        CALL FASTMOV(AGTGAM(1,1,TERMINAL),CLRKDAY(1,1,OLDCLERK),
     *               AGAMLEN*MAXGAM)
        CALL FASTMOV(AGTSPE(1,1,TERMINAL),CLRKSPE(1,1,OLDCLERK),
     *               ASPELEN*MAXGAM)
        CALL FASTMOV(AGTMIS(1,1,TERMINAL),CLRKMIS(1,1,OLDCLERK),
     *               AMISLEN*NUMTOT)
C
C MOVE RECORD TO MEMORY
C
        CALL FASTMOV(CLRKDAY(1,1,NEWCLERK),AGTGAM(1,1,TERMINAL),
     *               AGAMLEN*MAXGAM)
        CALL FASTMOV(CLRKSPE(1,1,NEWCLERK),AGTSPE(1,1,TERMINAL),
     *               ASPELEN*MAXGAM)
        CALL FASTMOV(CLRKMIS(1,1,NEWCLERK),AGTMIS(1,1,TERMINAL),
     *               AMISLEN*NUMTOT)
C
C MOVE GAME FLAGS FUNCTIONS FROM OLD CLERK BACK FOR NEW CLERK
C
C***  DO 1000 GMOFF=1,MAXGAM
C***  AGTGAM(GFLAGS,GMOFF,TERMINAL)=CLRKDAY(GFLAGS,GMOFF,OLDCLERK)
C***1000  CONTINUE
C
C WRITE UPDATE RECORD TO FILE
C
        CALL WRITEW(CLRKFDB,TERMINAL,CLRKREC,ST)
        IF(ST.NE.0) THEN
          MESS(1)=SPE
          MESS(2)=TEGEN
          MESS(3)=5
          CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
          MESS(9)=ST
          MESS(10)=TERMINAL
          CALL QUEMES(MESS)
          GOTO 9000
        ENDIF
C
9000    CONTINUE
        RETURN
        END
