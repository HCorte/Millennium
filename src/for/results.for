C
C PROGRAM RESULTS
C
C RESULTS.FOR
C
C V18 01-JAN-2010 FJG ePassive
C V17 06-FEB-2001 ANG Added Passive game 
C V16 01-DEC-2000 UXN Totogolo added.
C V15 05-JAN-1999 OXK WINYES set as status (=removed WINPRV)
C V14 16-DEC-1999 OXK MULTIWIN changes.
C V13 13-OCT-1999 RXK World Tour added.
C V12 14-MAY-1999 UXN Super Triple added.
C V11 12-Jan-1998 RXK Super Score and Today's Triple added.   
C V10 29-SEP-1997 UXN Displaying scratched horses for postponed RAVI
C	              draws added.
C V09 15-DEC-1995 HXK Allow entry of gtyp = 13 (Couple game)
C V08 23-NOV-1995 PXB Added Double and Couple game
C V07 07-NOV-1994 HXK Allow Bingo game type to be entered
C V06 18-OCT-1994 HXK Added Bingo
C V05 14-OCT-1993 HXK PROMPT FOR POSTPONE DRAW FOR RAVI.
C V04 23-JUL-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 01-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C RESULTS ENTRY CONTROL PROGRAM
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM RESULTS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

C
        ! variables
	INTEGER*4  FLAG                    !
	INTEGER*4  DRAW                    !
	INTEGER*4  GNUM                    !
	INTEGER*4  GIND                    !
	INTEGER*4  EXT                     !
	INTEGER*4  GTYP                    !
	INTEGER*4  K                       !
	INTEGER*4  I                       !
	INTEGER*4  ST                      !
        INTEGER*4  CBUF(CDLEN)             ! COMMAND BUFFER
C

	LOGICAL*4   OK

C
	CALL COPYRITE
C
C
	VERREADY = 0
	OPDONE   = 0
	ONLINE   = .TRUE.
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
100	CONTINUE

	WRITE(6,900) (K,GTNAMES(K),K=1,MAXTYP)
	CALL INPNUM('Enter game type ',GTYP,1,MAXTYP,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C
	GNUM=SCFGTN(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	    TYPE*,'Sorry, game selected is not active'
	    GOTO 100
	ENDIF
C
C
	IF(GTYP.EQ.TINS) THEN
            TYPE*,'Sorry, not applicable to Instant games'
            GOTO 100
        ENDIF
C
C
        IF(GTYP.EQ.TPAS) THEN
          IF(PASSTS(PASCURDRW(GIND),GIND).LE.GAMOPN) THEN
            CALL INPYESNO('Are results for a POSTPONED emission [Y/N]: ',FLAG)

            IF(FLAG.EQ.1) THEN
              CALL INPNUM('Enter emission number: ',DRAW,
     *                    PAS_DRW_OFFSET+1,9999,EXT)
              IF(EXT.LT.0) GOTO 100
            ELSE
              TYPE *,IAM(),' Invalid emission'
              GOTO 100
            ENDIF
          ELSEIF(PASSTS(PASCURDRW(GIND),GIND).LT.GAMBFD) THEN
            TYPE*,'Sorry - sales are not closed.'
            GOTO 100
          ELSEIF(PASSTS(PASCURDRW(GIND),GIND).GT.GAMBFD) THEN
            TYPE*,'Sorry - drawing already took place.'
            GOTO 100
          ELSE
            DRAW=PASEMIS(PASCURDRW(GIND),GIND)
          ENDIF
        ELSE
          DRAW=DAYDRW(GNUM)
          CALL INPNUM('Enter draw number [C-current draw] ',DRAW,1,99999,EXT)
          IF(EXT.LT.0.AND.EXT.NE.-5) GOTO 100
        ENDIF
C
C
	WRITE(6,910) GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
	CALL INPYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100
C
110   CONTINUE
C
C CHECK IF ONLINE OF OFFLINE DRAWING
C
	IF(DAYSTS.NE.DSOPEN .OR. ( GTYP.NE.TPAS .AND. DRAW.NE.DAYDRW(GNUM) ) .OR.
     *     GTYP.EQ.TSCR .OR. GTYP.EQ.TWIT .OR. GTYP.EQ.TTSL) THEN
	    ONLINE=.FALSE.
	ENDIF
C
C SET FOR NOT ACTIVE SPORT GAMES
C
        IF((GTYP.EQ.TSPT) .OR. (GTYP.EQ.TTGL)) THEN
         IF(DRAW .EQ. DAYHDR(GNUM) .AND. DAYDRW(GNUM) .LE. 0) THEN
            IF(DAYSTS .EQ. DSOPEN) ONLINE=.TRUE.
         ENDIF	
        ENDIF
C
C WARNINIG IF NOT OFLINE
C
        IF(ONLINE .EQ. .FALSE.) THEN
	    TYPE*,IAM(),' Winning results will be set on this system only'        	
        ENDIF	        
C
C LOAD LOTTERY TASK AND SEND START OPTIONS
C
	CUR_GTYP = GTYP
	CUR_GIND = GIND
	CUR_GNUM = GNUM
	CUR_DRAW = DRAW
	VERREADY = 1
C
C
	IF(GTYP.EQ.TSCR.OR.GTYP.EQ.TWIT.OR.GTYP.EQ.TTSL.OR.
     *	   GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *     GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.
     *     GTYP.EQ.TSTR) ONLINE=.FALSE.
	IF(GTYP.EQ.TLTO) THEN 
	   CALL LTOENT(GNUM,GIND,DRAW)
	ELSEIF(GTYP.EQ.TSPT) THEN 
	   CALL SPTENT(GNUM,GIND,DRAW)
	ELSEIF(GTYP.EQ.TTGL) THEN 
	   CALL TGLENT(GNUM,GIND,DRAW)
       ELSEIF(GTYP.EQ.TNBR) THEN 
	   CALL NBRENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TKIK) THEN 
	   CALL KIKENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TSCR) THEN 
	   CALL SCRENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TWIT) THEN 
	   CALL WITENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TTSL) THEN 
	   CALL TSLENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TBNG) THEN 
	   CALL BNGENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TDBL) THEN 
	   CALL DBLENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TCPL) THEN 
	   CALL CPLENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TSSC) THEN 
	   CALL SSCENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TTRP) THEN 
	   CALL TRPENT(GNUM,GIND,DRAW)	
        ELSEIF(GTYP.EQ.TSTR) THEN 
	   CALL STRENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TPAS) THEN
           CALL PASENT(GNUM,GIND,DRAW)
	ELSE
	   TYPE*,'Invalid game type ',GTYP
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF

        CALL FASTSET(0, CBUF, CDLEN)
 	IF (GTYP.NE.TPAS) THEN
	    DO I=1,MAX_WINSEL
	       IF ((DRWGAM(I,GNUM).EQ.DRAW).OR.(DRWGAM(I,GNUM).EQ.0)) THEN
                  IF(ONLINE) THEN
		     CBUF(1) = 12
                     CBUF(2) = WINYES 
     		     CBUF(3) = TCGEN
                     CBUF(8) = I
                     CBUF(9) = GNUM
                     CBUF(10)= DRAW
                     CBUF(11)= GIND
                     CALL RESCMD(CBUF)
                  ELSE
                     DRWSTS(I, GNUM) = WINYES   ! SYSTEM DOWN, UPDATE WINSEL STATUS
                     DRWGAM(I, GNUM) = DRAW
                  ENDIF
                  GOTO 20
	       ENDIF
	    ENDDO

20	    CONTINUE
            OK = .TRUE.
            IF (I.GT.MAX_WINSEL) OK=.FALSE.
            IF (I.EQ.MAX_WINSEL) THEN
               IF (DRWGAM(I,GNUM).NE.DRAW) OK=.FALSE.
            ENDIF
            IF (.NOT.OK) THEN
               TYPE*,IAM(),'Too many winner selections for the same game.'
               TYPE*,IAM(),'This may result some probelms in MULTIWIN.'
            ENDIF
	ENDIF

	VERREADY = 0

	WRITE(6,920) GTNAMES(GTYP),GIND
	CALL GSTOP(GEXIT_SUCCESS)
C
C
800	FORMAT('Enter ',A8,I1,' draw number ')
810	FORMAT(A8)
820	FORMAT(A8,I1,I1,I2.2,I5.5)
900	FORMAT(//,' Game results entry ',//,
     *	        <MAXTYP>(1X,I2,' - ',A8,/))
904     FORMAT(/)
905     FORMAT(1X,I2,',')                              
910	FORMAT(1X,A8,I1,2X,4A4,'Draw ',I5,/)
920	FORMAT(1X,A8,I1,' results entry complete')

	END
