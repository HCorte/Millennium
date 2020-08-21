C
C PROGRAM DWINSEL
C  
C
C V04 14-DEC-1999 OXK MULTIWIN changes.
C V03 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V02 11-JAN-1999 GPW STOPSYS OPTIMIZATION
C V01 23-NOV-1995 PXB Initial revision.
C  
C DWINSEL.FOR
C
C SUPER DOUBLE WINNER SELECTION PROGRAM.
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM DWINSEL
	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'                        !V01               
C---- Local variables used

	INTEGER*4 TUBSIZ
	PARAMETER (TUBSIZ=I4BUCSIZ*7)

	BYTE	  I1TEMP(4)

        INTEGER*4   NTSK
        INTEGER*4   INDTSK                        !FUNCTION  !V04
	INTEGER*4 TFDB(7),TMFBUF(8192)
	INTEGER*4 GTYP, EOF, WIN, I, FILCNT, IND, TYPE
	INTEGER*4 LENGTH, DUMMY, BLOCK, ST, K, TEMP, S
        INTEGER*4 AWNTAB(2,NUMAGT)
	INTEGER*4 FILES(5,200)

	CHARACTER CFILES(200)*20

	EQUIVALENCE (FILES,CFILES)
	EQUIVALENCE (TEMP,I1TEMP)

        COMMON/BIGWIN/ AWNTAB

	DATA EOF/0/,WIN/0/

C------------------------- Start of code  ------------------------------

	CALL COPYRITE


C INITIALIZE WINNER SELECTION COMMON
C
 
        IF (.NOT.ISSUBPROC()) THEN
            TYPE*,IAM(),
     *            'This program can be run only from *WINTSK or MULTIWIN'
            CALL GSTOP(GEXIT_FATAL)
        ENDIF

        IF(STOPMOD.EQ.WINMANUAL) THEN
             CALL DWIN_WININT(CFILES,FILCNT)
        ELSE
             NTSK=INDTSK('DWINTSK ')
             CALL STORFIL(NTSK,CFILES,DUMMY,FILCNT,2,0)
        ENDIF

	IF(FILCNT.EQ.0) THEN
	  TYPE*,IAM(),' Sorry, no Super Double winner selection today'
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF

	CALL DWIN_WINLOD(1,DUMMY)

C---- Start draw file scan.

	CARYSCAN(TDBL) = .FALSE.
	CALL FASTSET(0,V4BUF,VFLEN*4)

	DO 3000 I = 1,FILCNT
	  CALL DWIN_OPNDRW(FILES(1,I),PTMF)

	  CALL IOINIT(TFDB,PTMF,128*256)

	  WRITE(6,910) IAM(),(FILES(S,I),S=1,5)

C---- Scan file.

	  BLOCK = 0
	  EOF = 0
	  IND = 8192
 
2030	  CONTINUE

	  IF (IND .GE. 8157) THEN
	    BLOCK = BLOCK + 1
	    IND = 1
	    CALL READW(TFDB,BLOCK,TMFBUF,ST)
	    IF (ST .NE. 0) THEN
	      WRITE(6,900) IAM(),(FILES(K,I),K=1,5),ST,BLOCK
	      CALL GPAUSE
	    END IF
	  END IF

	  IF (EOF .GT. 1000) GOTO 2090

	  IF (TMFBUF(IND) .EQ. 0) THEN
	    EOF = EOF + 1
	    IND = IND + LREC
	    GOTO 2030
	  END IF

	  EOF = 0
	  TEMP = TMFBUF(IND+LREC-1)
	  TYPE = I1TEMP(4)

	  IF (TYPE .NE. LONE .AND. TYPE .NE. LREG) THEN
	    TYPE*,IAM(),' Bad record type > ',TYPE,' index > ',IND
	    IND = IND+LREC
	    GOTO 2030
	  END IF

	  LENGTH = LREC

	  IF (TYPE .EQ. LONE) THEN
	    TEMP = TMFBUF(IND+LREC*2-1)
	    TYPE = I1TEMP(4)
	    IF (TYPE .EQ. LEND) LENGTH = LREC * 2
	    IF (TYPE .EQ. LTWO) LENGTH = LREC * 3
	  END IF

	  CALL LOGTRA(TRABUF,TMFBUF(IND))

	  IND = IND + LENGTH
	  GTYP = TRABUF(TGAMTYP)

	  IF (GTYP .NE. TDBL) GOTO 2030

C---- Check if winner.

	  CALL DWIN_POST(TRABUF)

	  CALL DCHKWIN(TRABUF,V4BUF,WIN)

	  IF (WIN .NE. 0) THEN
	    CALL DWIN_WINLOD(2,V4BUF)
	    CALL FASTSET(0,V4BUF,VFLEN*4)
	  END IF

	  GOTO 2030

2090	  CONTINUE

C---- Winsel complete, flush buffers to draw files.

3000	CONTINUE

	TYPE*,IAM(),' Draw file scan complete'

	CALL DWIN_WINLOD(4,DUMMY)

	CALL DWIN_PSTGDF

	TYPE*,IAM(),' Posting big winners to agent file'

        IF(STOPMOD.EQ.WINMANUAL) THEN                           !V01
            CALL PSTAWN(AWNTAB)
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
C MULTIWINSEL
C
C WAIT FOR ASF FILE ACCESS
c
        CALL WAITLOCK(ASFBIT,ASFLOCK)                           !V01
        CALL PSTAWN(AWNTAB)
        CALL FREELOCK(ASFBIT,ASFLOCK)
        CALL GSTOP(GEXIT_SUCCESS)
C


C----------------------------- Format Statememnts. -----------------

900	FORMAT (1X,A,1X,5A4,' read error> ',I4,' block> ',I8)

910	FORMAT (1X,A,1X,'Scanning file ',5A4,' for winners ')

	END
