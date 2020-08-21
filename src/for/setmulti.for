C
C SETMULTI.FOR
C 
C V02 16-JAN-2000 UXN DISPWIN changed and SORT_WINSEL added.
C V01 15-DEC-1999 OXK Initial release.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
	PROGRAM SETMULTI

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 FLAG
C
	CALL COPYRITE

	TYPE*,IAM(),'With this program its possible to add & remove winsels'
	TYPE*,IAM(),'for MULTIWIN. '
	TYPE*,IAM(),'You should not run it in normal circumstances.'

	CALL PRMYESNO('Do you want to continue (YES/NO)',FLAG)
	IF (FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)

	CALL ADREGAMTST
	CALL GSTOP(GEXIT_SUCCESS)
	
	END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
      SUBROUTINE ADREGAMTST
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
C

      INTEGER*4 GNUM,GTYP,GIND,DRAW,EXT,FLAG,I,K
      LOGICAL   OK
      INTEGER*4 NEWSTS,OLDSTS,OLDIND
C
C
        GNUM=0
	CALL GETSCONF(SCFREC,EXT)
	IF (EXT.NE.0) CALL GSTOP(GEXIT_FATAL)

   10   CONTINUE
	CALL SORT_WINSEL
	CALL DISPWIN(0)
C
        WRITE(6,900) (K,GTNAMES(K),K=1,MAXTYP)
        CALL PRMNUM('Enter game type ',GTYP,1,MAXTYP,EXT)
        IF(EXT.LT.0) RETURN
        CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) RETURN
C
        GNUM=SCFGTN(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
            TYPE*,'Sorry, game selected is not active'
            GOTO 10
        ENDIF
C
        DRAW=DAYDRW(GNUM)
        CALL PRMNUM('Enter draw number [C-current draw] ',DRAW,1,99999,EXT)
        IF(EXT.LT.0.AND.EXT.NE.-5) GOTO 10
C
	WRITE(6,2020)
        CALL PRMNUM('Enter status ', NEWSTS,0,5,EXT)
	IF(EXT.LT.0) GOTO 10

	OLDSTS=WINNOT
	OLDIND=0
	DO I=1,MAX_WINSEL
	   IF(DRWGAM(I,GNUM).EQ.DRAW) THEN
	      OLDSTS = DRWSTS(I,GNUM)
	      OLDIND = I
	   ENDIF
	ENDDO

        WRITE(6,910) 
     *        GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW,OLDSTS,NEWSTS
        CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
        IF(FLAG.NE.1) GOTO 10
C
  	OK=.FALSE.
	IF (OLDIND.GT.0) THEN		    ! WE CHANGE AN EXISTING ENTRY
	    DRWSTS(OLDIND,GNUM)=NEWSTS
	    DRWGAM(OLDIND,GNUM)=DRAW
	    OK=.TRUE.
	    IF (NEWSTS.EQ.WINNOT) THEN
		DRWGAM(OLDIND,GNUM)=0
		DO I=OLDIND,MAX_WINSEL-1
		    DRWSTS(I,GNUM)=DRWSTS(I+1,GNUM)
		    DRWGAM(I,GNUM)=DRWGAM(I+1,GNUM)
		ENDDO
	    ENDIF
	ELSEIF (NEWSTS.NE.WINNOT) THEN	    ! WE ADD A NEW ENTRY
	    DO I=1,MAX_WINSEL
	       IF(DRWGAM(I,GNUM).EQ.0.AND..NOT.OK) THEN
		  DRWSTS(I,GNUM)=NEWSTS
		  DRWGAM(I,GNUM)=DRAW
		  OK=.TRUE.
	       ENDIF
	    ENDDO
	    IF (.NOT.OK) WRITE(6,*)'Cannot add more than ',MAX_WINSEL,
     *				   ' winsels for the same game' 
	ENDIF
	GOTO 10
C

900     FORMAT(//,<MAXTYP>(1X,I2,' - ',A8,/))
910     FORMAT(1X,A8,I1,2X,4A4,'Draw ',I5,'OLDSTS=',I2,'NEWSTS=',I2/)
2020    FORMAT(1X,' === Select the NEW status ==='/
     *         1X,'  0 - WINNOT = NO WINSEL TODAY'/
     *         1X,'  1 - WINYES = REGULAR WINSEL'/
     *         1X,'  2 - WINPRV = PREV. POSTPONED WINSEL'/
     *         1X,'  3 - RESNOT = RESULTS NOT IN'/
     *         1X,'  4 - WINCAN = WINSEL CANCELLED/REMOVED'/
     *         1X,'  5 - WINSOK = WINSEL DONE'/
     *         1X,'  E - Stop')
      	END
C
