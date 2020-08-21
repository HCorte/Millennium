C
C PROGRAM RESULTREP
C
C V01 16-DEC-1999 PXO Taken from results.for
C
C Creates a report of results
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM RESULTREP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
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
	INTEGER*4  ST                      !
C
	CALL GETSCONF(SCFREC,ST)
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
	CALL INPNUM('Enter draw number ',DRAW,1,99999,EXT)
	IF(EXT.LT.0.AND.EXT.NE.-5) GOTO 100
C
C
	WRITE(6,910) GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
	CALL INPYESNO('Is this correct (Y/N) ',FLAG)
	IF(FLAG.NE.1) GOTO 100

	IF(GTYP.EQ.TLTO) THEN 
	   CALL LORESULT(GIND,DRAW)
	ELSEIF(GTYP.EQ.TSPT) THEN 
	   CALL VARESULT(GIND,DRAW)
C        ELSEIF(GTYP.EQ.TNBR) THEN 
C	   CALL NBRENT(GNUM,GIND,DRAW)
        ELSEIF(GTYP.EQ.TKIK) THEN 
	   CALL JORESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TSCR) THEN 
	   CALL SCRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TWIT) THEN 
	   CALL VORESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TTSL) THEN 
	   CALL TSRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TBNG) THEN 
	   CALL BNRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TDBL) THEN 
	   CALL DBRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TCPL) THEN 
	   CALL CPRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TSSC) THEN 
	   CALL SSRESULT(GIND,DRAW)
        ELSEIF(GTYP.EQ.TTRP) THEN 
	   CALL TRRESULT(GIND,DRAW)	
        ELSEIF(GTYP.EQ.TSTR) THEN 
	   CALL STRESULT(GIND,DRAW)	
	ELSE
	   TYPE*,'Invalid game type ',GTYP
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF

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
