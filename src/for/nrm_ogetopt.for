C
C SUBROUTINE OGETOPT
C $Log:   GXAFXT:[GOLS]OGETOPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:17:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.5   05 Dec 1994 16:03:06   HXK
C  Merging from 25-Nov -> 5 Dec
C  
C     Rev 1.5   29 Nov 1994 17:54:38   HXK
C  
C  Always set Bingo Full house and Lucky number
C  
C     Rev 1.4   15 Oct 1994 16:38:12   HXK
C  Adding /developing Bingo (15.Oct.94)
C  
C     Rev 1.3   19 Jul 1993 12:01:02   GXA
C  Removed Alternate flag, Added check for fractions = MAXFRC,
C  Corrected flaging of kicker offsets, do not add for both kickers.
C  
C     Rev 1.2   10 Jun 1993 15:55:58   HXK
C  BUG FIX FOR IF/THEN
C  
C     Rev 1.1   10 May 1993 12:02:58   SXH
C  Released for Finland VAX
C  
C     Rev 1.0   21 Jan 1993 17:11:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getopt.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
      SUBROUTINE OGETOPT(TRABUF,OPTION)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
C
      ! arguments
      INTEGER * 4  OPTION        !

        ! variables
C
      INTEGER * 4 GNUM          !Game Number.
      INTEGER * 4 GIND          !Game Index
      INTEGER * 4 TOTCAN_EVENTS ! TOTAL NUMBER OF EVENTS CANCELLED
      ! start of code

      OPTION = 0
      GNUM = TRABUF(TGAM)
      GIND = TRABUF(TGAMIND)

C
C CHECK FOR KICKER GAME
C
      IF(TRABUF(TWKGME).NE.0) THEN
         OPTION = OPTION + '80'X        ! Joker Offsets (Checked in OUTWAG)
      ENDIF
C
      ! check joker 1 
      IF (TRABUF(TWKICK) .NE. 0) THEN
          OPTION = OPTION + '40'X       ! joker 1 number present
      END IF

      ! check joker 2 
      IF (TRABUF(TWKICK2) .NE. 0) THEN
          OPTION = OPTION + '20'X       ! joker 2 number present
      END IF

      ! check fractions
      IF (TRABUF(TFRAC).NE.0.AND.TRABUF(TFRAC).NE.MAXFRC(GNUM)) THEN
          OPTION = OPTION + '10'X
      END IF
C
      IF(TRABUF(TGAMTYP).EQ.TBNG) THEN
          OPTION = OPTION + '04'X       !Bingo Full House Present
          OPTION = OPTION + '01'X       !Lucky Number Present
      ENDIF                             !(NB Bingo A,B only sent exch/reprnt)
C
      IF(TRABUF(TGAMTYP) .EQ. TSPT .AND. TRABUF(TWCEBM) .NE. 0) THEN
        OPTION = OPTION + '02'X ! Sports Events Cancelled
      ENDIF
C
      RETURN

      END
