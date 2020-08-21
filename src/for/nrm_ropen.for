C
C SUBROUTINE ROPEN
C $Log:   GXAFXT:[GOLS]ROPEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:45:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:31:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ropen.for **
C
C VAX_ROPEN.FOR
C
C V02 13-NOV-97 UXN OPENW replaced with OPEN command. This allows to use
C	            logical units returned by LIB$GET_LUN (they are > 100)
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ROPEN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 16-JAN-89 LMF INITIAL RELEASE FOR INDIANA (FROM ILL)
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
C
C
C     ROPEN.FTN
C
C     OPEN REPORT FILE
C
C     SUBROUTINE ROPEN(NAME,UNIT,STATUS)
C     IN - FILE - REPORT FILE TO ASSIGN AND OPEN
C          UNIT - FILE UNIT #
C     OUT - STATUS - OPERATION STATUS
C
C     ENTRY ROPEN1(NAME,UNIT,RLEN,STATUS)
C     SAME AS ROPEN, RECORD LENGTH IS NOT DEFAULT (132), IT'S RLEN
C     ROPEN1 WON'T TURN ON CARRIAGE CONTROL OPTION
C
C
C     REPORT FILE IS FIRST DELETED, THEN CREATED
C     THEN ASSIGN TO UNIT,
C     FILE IS CREATED AS INDEXED WITH RECORD SIZE 132
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ROPEN(FILE,UNIT,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 UNIT  !FILE UNIT #
	INTEGER*4 STATUS, RLEN,RECLEN
	CHARACTER FILE*(*)
C
C     TRY TO DELETE FILE FIRST
C
	RECLEN = 133
	CALL DFILX(FILE,0,0,STATUS)
	IF (STATUS.NE.0) RETURN
C
C     NOW TRY TO CREATE FILE
C
	CALL CARCON(UNIT,1) !SET CARRIAGE CONTROL OPTION
	GOTO 10  !CONTINUE FOR BOTH ENTRIES
C
	ENTRY ROPEN1(FILE,UNIT,RLEN,STATUS)
	RECLEN = RLEN
	CALL DFILX(FILE,0,0,STATUS)
	IF (STATUS.NE.0) RETURN
C
C     ASSIGN FILE TO UNIT
C
10	CONTINUE
	OPEN(UNIT=UNIT,FILE=FILE,ACCESS='SEQUENTIAL',
     *       STATUS='NEW',RECL=RECLEN,IOSTAT=STATUS)
	RETURN
	END
