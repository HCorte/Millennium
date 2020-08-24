C
C SUBROUTINE FCANWIN_GETGAM
C $Log:   GXAFXT:[GOLS]FCANWIN_GETGAM.FOV  
C  
C     Rev 1.0   17 Apr 1996 13:08:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   14 Dec 1994 19:38:38   PXB
C  Added bingo game.
C  
C     Rev 1.0   18 Nov 1993 15:20:14   SXH
C  Initial revision.
C  
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vlf_getgam.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE FCANWIN_GETGAM(DRAWS,GAMCNT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'

        ! arguments
        INTEGER*4  DRAWS(MAXGAM)    !
        INTEGER*4  GAMCNT           !

        ! arguments
        INTEGER*4  K                !
        INTEGER*4  GTYP             !
        INTEGER*4  GIND             !
        INTEGER*4  GNUM             !
        INTEGER*4  DRAW             !
        INTEGER*4  FLAG             !
        INTEGER*4  EXT              !

        CHARACTER*44 STRING         !
        CHARACTER*32 STRING2        !
	
	
C
	GAMCNT=0
	CALL FASTSET(0,DRAWS,MAXGAM)

1000    CONTINUE
        WRITE(5,900) IAM(),(K,GTNAMES(K),K=1,MAXTYP)
        CALL PRMNUM('Enter game type (E-no more) ',GTYP,1,MAXTYP,EXT)
        IF(EXT.LT.0) RETURN

        CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 1000
C
C
        GNUM=GTNTAB(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
            TYPE*,IAM(),' Sorry, game selected is not active'
            GOTO 1000
        ENDIF
C
        WRITE (STRING2,800) GTNAMES(GTYP),GIND
        CALL PRMNUM(STRING2,DRAW,1,9999,EXT)
        IF(EXT.LT.0) GOTO 1000
C
        WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,
     *               (GLNAMES(K,GNUM),K=1,4),DRAW
        CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 1000
	DRAWS(GNUM)=DRAW
	GAMCNT=GAMCNT+1
        GOTO 1000
800     FORMAT('Enter ',A8,I1,' draw number ')
900     FORMAT(//,1X,A,' FCANWIN game selection',//,
     *          8(19X,I2,' - ',A8,/))
910     FORMAT(1X,A,A8,I1,2X,4A4,'Draw ',I5,/)

        END
