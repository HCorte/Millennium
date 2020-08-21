C OPENQFILE.FOR
C
C
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]OPENQFILE.FOV                                $
C  $Date::   17 Apr 1996 14:18:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C V01 18-AUG-95 XXX INITIAL RELEASE 
C
C     CALL OPENQFILE(UNIT,FILENR,STATUS)
C     IN:
C     UNIT  - LOGICAL UNIT # TO USE
C     FILENR - SYSTEM FILE #
C     OUT:
C     STATUS - SCF FILE READ STATUS
C
C     FUNCTION:
C        WILL OPEN SYSTEM FILE ACCORDING TO DEFINITION IN SCF.FIL
C        WILL LOOP TILL CAN OPEN SCF.FIL AND FILE IN QUESTION
C        CALL FILERR SUBROUTINE
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
	SUBROUTINE OPENQFILE(UNIT,FILENR,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
C
	INTEGER*4 FDB(7), ST, STATUS, FILENR, UNIT
C
C
5	CONTINUE

       IF (PX2X_TASK) THEN                                             ! V04
          CALL OPENX(UNIT,'PX2XFILES:SCF.FIL',4,0,0,ST)                    ! V02
        ELSE                                                            ! V04
          CALL OPENX(UNIT,'SCF.FIL',4,0,0,ST)                              ! V04
        ENDIF
	CALL IOINIT(FDB,UNIT,SCFSEC*256)
	IF(ST.NE.0) THEN
	  TYPE*,'SCF.FIL open error > ',ST
	  CALL GPAUSE
	  GOTO 5
	ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'SCF.FIL read error > ',ST
	  STATUS=ST
	  RETURN
	ENDIF
C
	CALL CLOSEFIL(FDB)
C
C
C CHANGE SYSTEM FILE SIZES/VOLUME NAMES
C
10	CONTINUE
	IF(SCFSFN(1,FILENR).EQ.'    ') CALL SYSVOL(SCFSFN(1,FILENR))
	CALL OPENQW(UNIT,SCFSFN(1,FILENR),4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,FILENR),1,ST,0)
	  CALL GPAUSE
	  GOTO 10
	ENDIF
C
	RETURN
	END
