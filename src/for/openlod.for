C
C SUBROUTINE OPENLOD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]OPENLOD.FOV                                  $
C  $Date::   17 Apr 1996 14:18:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodsub.for;1 **
C
C X2LODSUB.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C These subroutines provide a method of handling reads and
C writes to the X2X network files.
C NOTE: Max of 1 X2X files can be open at one time using
C X2LODSUB.
C
C To open the an X2X file:
C
C     CALL OPENLOD(FILNAM,XLUN)
C
C     Input:    FILNAM: name of file to open
C     Input:    XLUN:   logical unit number (1..4)
C              (This will open X2X???.FIL on the current pack)
C
C To read record from the X2X file:
C
C     CALL READLOD(XLUN,REC,X2XBUF,ST)
C
C     Input:    XLUN:  logical unit number (1..4)
C     Input:     REC:  Slot # in the X2X file
C     Output:  X2XBUF: Array to contain X2X record
C
C To write a record:
C
C     CALL WRITLOD(XLUN,REC,X2XBUF,ST)
C
C     Input:    XLUN:  logical unit number (1..4)
C     Input:     REC:  Slot # in the X2X file
C     Output:  X2XBUF: Array to contain X2X record
C
C (Note that the writes are buffered, therefore you must call
C  CLOSX2X to write out the last bunch of records).
C
C To close the file:
C
C     CALL CLOSLOD(XLUN)
C
C     *** This MUST be called if any writes were done ***
C
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
	SUBROUTINE OPENLOD(FILNAM,XLUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2LODSUB.DEF'
C
	INTEGER*4   XLUN, ST
	CHARACTER   FILNAM*(*)
C
C VERIFY LUN FALLS WITHIN RANGE.
C
	IF(XLUN.LT.0 .OR. XLUN.GT.MAXFIL) THEN
	  TYPE *,'INVALID LOGICAL UNIT NUMBER:  ',XLUN
	  CALL GPAUSE
	ENDIF
C
	LUN=XLUN
	X2XNAM(LUN)=FILNAM
	CALL OPENX(LUN,X2XNAM(LUN),4,0,0,ST)
	IF(ST.NE.0)THEN
	  CALL OS32ER(5,X2XNAM(LUN),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
	CALL IOINIT(FDB(1,LUN),LUN,X2XSEC*RECSPERBKT*256)
C
	BKTNUM(LUN)=-1                   !FORCE A READ
	BKTCHG(LUN)=0                    !NOTHING HAS BEEN CHANGED
C
	RETURN
	END
