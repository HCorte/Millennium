C
C SUBROUTINE X2BLDCMD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLDCMD.FOV                                 $
C  $Date::   17 Apr 1996 16:08:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2bldcmd.for;1 **
C
C X2BLDCMD.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will build commands which will tell
C CMDPRO to update the memory and to optionally update the
C X2XGBL.FIL.
C
C NOTE:  if SOURCE is equal to 2 (VISION), it is assumed
C that the command will be queued from the snapshot and
C not from this route.
C
C Input parameters:
C
C     ADDFLG      Int*4       0=New record,1=modified record
C     FILE        Int*4       File # relative to X2X subsystem
C     RECORD      Int*4       Record number being modified
C     FIELD       Int*4       Field number of file
C     VALUE       Int*4(4)    New value for field
C     SOURCE      Int*4       Source of modification
C                             1 = BLDX2X
C                             2 = VISION
C     UPDFIL      Int*4       0=update record,1=no update
C
C Output parameters:
C
C     CBUF        Int*4(CDLEN)    Command buffer
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2BLDCMD(ADDFLG,FILE,RECORD,FIELD,
     *	                    VALUE,SOURCE,UPDFIL,CBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4   ADDFLG              !New record flag
	INTEGER*4   FILE                !File being modified (see GLOBAL)
	INTEGER*4   RECORD              !Record number being modified
	INTEGER*4   FIELD               !Field within file
	INTEGER*4   VALUE(4)            !New value
	INTEGER*4   SOURCE              !Source of request
	INTEGER*4   UPDFIL              !Update file flag
	INTEGER*4   CBUF(CDLEN)         !Command buffer
	INTEGER*4   ST, I
C
C CLEAR COMMAND BUFFER
C
	DO 50 I=1,CDLEN
	  CBUF(I)=0
50	CONTINUE
C
C BUILD COMMAND AND QUEUE IT TO THE GAME.
C
	CBUF(1)=1
	CBUF(2)=ADDFLG
	CBUF(3)=TCX2X
	CBUF(4)=VALUE(4)
	CBUF(5)=VALUE(3)
	CBUF(7)=UPDFIL
	CBUF(8)=FILE
	CBUF(9)=FIELD
	CBUF(10)=VALUE(1)
	CBUF(11)=VALUE(2)
	CBUF(12)=RECORD
C
C IF REQUEST FROM BLDX2X QUEUE THE COMMAND TO THE GAME.
C
	IF(SOURCE.EQ.1)THEN
	  CBUF(6)='BLDX'
100	  CONTINUE
	  CALL QUECMD(CBUF,ST)
	  IF(ST.NE.0) THEN
	    TYPE*,'Queue command error '
	    CALL XWAIT(1,2,ST)
	    GOTO 100
	  ENDIF
	ENDIF
C
C PROGRAM EXIT.
C
	RETURN
	END
