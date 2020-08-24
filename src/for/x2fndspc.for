C
C SUBROUTINE X2FNDSPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FNDSPC.FOV                                 $
C  $Date::   17 Apr 1996 16:17:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2fndspc.for;1 **
C
C X2FNDSPC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C This subroutine will find a port on a station defined
C in the Station Port Configuration file.
C
C Calling sequence:
C
C     CALL X2FNDSPC(STATION,PORT,ST,REC,EXSREC)
C
C Input parameters:
C
C     STATION     Int*4   Station number
C     PORT        Int*4   Port number
C
C Output parameters:
C
C     ST          Int*4   Status (0=not exist,-1=exist)
C     REC         Int*4   Next empty slot in file
C     EXSREC      Int*4   Record where station/port exists
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
	SUBROUTINE X2FNDSPC(STATION,PORT,ST,REC,EXSREC)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
C
	INTEGER*4   STATION         !Station number
	INTEGER*4   PORT            !Port number
	INTEGER*4   ST              !Status
	INTEGER*4   REC             !Empty record pointer
	INTEGER*4   EXSREC          !Existing record pointer
C
	REC=STATION*X2X_MAXPORT+(PORT-1)
C
C READ THE RECORD.
C
	CALL READX2X(4,REC,X2XSPC_REC,ST)
	IF(ST.NE.0) THEN
	  IF(ST.NE.-99) ST=0
	  GOTO 8000
	ENDIF
C
C SEE IF THE STATION AND PORT NUMBER ALREADY EXIST.
C IF IT DOES, SET THE STATUS AND STORE THE RECORD NUMBER.
C
	IF(X2XSPC_STN .EQ. STATION .AND.
     *	   X2XSPC_PORT.EQ. PORT) THEN
	  ST=-1
	  EXSREC=REC
	  GOTO 8000
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
