C SUBROUTINE DISPLVL
C
C V04 17-MAY-2000 OXK I/O & layout fixes
C V03 19 May 1996 HXK Wojtek's security stuff added
C V02 21 Jan 1993 DAB Initial Release
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
	SUBROUTINE DISPLVL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESLVL.DEF'
C
	INTEGER*4 PROG, ST, I, BEGIN, END, OP, J
	CHARACTER*80 XKEY,NEW(22)
        CHARACTER*25 FNAM(5)
        DATA FNAM(1)/'    VISION SNAPSHOTS     '/
        DATA FNAM(2)/'    VISION COMMANDS      '/
        DATA FNAM(3)/' AGENT FILE MAINTENANCE  '/
        DATA FNAM(4)/' USER FILE MAINTENANCE   '/
        DATA FNAM(5)/'       CHGLVL FILE       '/
C
10	CALL CLRSCR(6)
C
        WRITE(6,930)
C
        CALL INPNUM('Please enter program number   : ',PROG,1,5,ST)
        IF(ST.EQ.-1) THEN
	  GOTO 200
        ENDIF
C
C Display vision snapshots
C
        IF(PROG.EQ.1) THEN		  
C
          CALL CLRSCR(6)
          WRITE(6,940) FNAM(PROG)
	  DO 20 I=1,SNPCNT,4
	    IF ((I.EQ.41).OR.(I.EQ.81).OR.(I.EQ.121)) THEN
              WRITE(6,960)
              CALL WIMG(6,' Press RETURN to continue ........')
              READ(5,900) XKEY
	      CALL CLRSCR(6)
              WRITE(6,940) FNAM(PROG)
	    ENDIF
	    WRITE(6,910) I,SNPNAM(I),I+1,SNPNAM(I+1),I+2,SNPNAM(I+2),
     *	                 I+3,SNPNAM(I+3)
20	  CONTINUE
          WRITE(6,960)
          CALL WIMG(6,' Press RETURN to exit ............')
          READ(5,900) XKEY
          GOTO 10
C
C Display vision commands
C
        ELSEIF(PROG.EQ.2) THEN
C
          CALL CLRSCR(6)
          WRITE(6,940) FNAM(PROG)
	  DO 30 I=1,CMDCNT,3
	    IF ((I.EQ.46).OR.(I.EQ.91).OR.(I.EQ.136)) THEN
              WRITE(6,970)
              CALL WIMG(6,' Press RETURN to continue ........')
              READ(5,900) XKEY
	      CALL CLRSCR(6)
              WRITE(6,940) FNAM(PROG)
	    ENDIF
	    WRITE(6,920)CMDNUM(I),CMDNAM(I),CMDNUM(I+1),CMDNAM(I+1),
     *	                CMDNUM(I+2),CMDNAM(I+2)
30	  CONTINUE
          WRITE(6,970)
          CALL WIMG(6,' Press RETURN to exit ............')
          READ(5,900) XKEY
          GOTO 10
C
C Display options for hasf
C
        ELSEIF(PROG.EQ.3) THEN
C
          CALL CLRSCR(6)
          WRITE(6,940) FNAM(PROG)
 	  DO 50 I=1,HSFCNT
	    WRITE(6,950) I,HASFNAM(I)
50	  CONTINUE
          WRITE(6,960)
          CALL WIMG(6,' Press RETURN to exit ............')
          READ(5,900) XKEY
	  GOTO 10
C
C Display options for userpass
C
        ELSEIF(PROG.EQ.4) THEN
C
          CALL CLRSCR(6)
          WRITE(6,940) FNAM(PROG)
	  DO 60 I=1,USRCNT
	    WRITE(6,950) I,USENAM(I)
60	  CONTINUE
          WRITE(6,960)
          CALL WIMG(6,' Press RETURN to exit ............')
          READ(5,900) XKEY
          GOTO 10
C
C Display options for chglvl
C
        ELSEIF(PROG.EQ.5) THEN
C
          CALL CLRSCR(6)
          WRITE(6,940) FNAM(PROG)
	  DO 70 I=1,CHGCNT
	    WRITE(6,950) I,CHGNAM(I)
70	  CONTINUE
          WRITE(6,960)
          CALL WIMG(6,' Press RETURN to exit ............')
          READ(5,900) XKEY
          GOTO 10
        ELSE
          GOTO 10
        ENDIF
C
C Exit
C
200     CONTINUE
        CALL XWAIT(1,2,ST)
        CALL CLRSCR(6)
        RETURN
C
C
900	FORMAT(A)
910     FORMAT(4(3X,I3.3,1X,A10))
920     FORMAT(3(4X,I5.5,1X,A13))
930     FORMAT(///21X,'<<<    DISPLAY OPTIONS   >>>',/,
     *            21X,'                            ',/,
     *            21X,'                            ',/,
     *            21X,'    1.  Vision Snapshots    ',/,
     *            21X,'    2.  Vision Commands     ',/,
     *            21X,'    3.  Hasf Selection      ',/,
     *            21X,'    4.  User Password       ',/,
     *            21X,'    5.  Change Level        ',/,
     *            21X,'                            ',/,
     *            21X,'         E - Exit           ',///)
940   FORMAT(//,28X,A25,//)
950   FORMAT(4X,I2.2,2X,A15)
960   FORMAT(//' ')
970   FORMAT(/' ')
      END
