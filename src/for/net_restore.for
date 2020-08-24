C
C SUBROUTINE NET_RESTORE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_RESTORE.FOV                              $
C  $Date::   17 Apr 1996 14:11:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_restore.for ***
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	RESTORE NETCOM AND DCNCOM FROM DISK ... AN UNSOPHISTICATED VERSION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_RESTORE
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4	CONCOM_SIZE,
     *			DN_SIZE,
     *			DUMP_FILE_SIZE,
     *			EXT,
     *			FDB(7),
     *			IMAGE_SIZE,
     *			NETCOMFIL(5)	/'NETC', 'OM.F', 'IL  ',
     *					 '    ', '    '/,
     *			NET_SIZE,
     *			NEXT_BLOCK,
     *			NUM_IMAGES,
     *			OPT,
     *			PROCOM_SIZE,
     *			ST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DETERMINE SIZE OF COMMONS.
C
	CONCOM_SIZE = %LOC(LAST_CONCOM) - %LOC(FRST_CONCOM) + 3
	DN_SIZE     = %LOC(DN_LAST_IN_COMMON)
     *              - %LOC(FRST_DN_EVERYTHING) + 3
	NET_SIZE    = %LOC(LAST_NETCOM) - %LOC(FRST_NETCOM) + 3
	PROCOM_SIZE = %LOC(LAST_PROCOM) - %LOC(FRST_PROCOM) + 3
C
C SIZE OF 1 IMAGE (DCNCOM, CONCOM, NETCOM AND PROCOM)
C
	IMAGE_SIZE = (CONCOM_SIZE
     *             +  DN_SIZE
     *             +  NET_SIZE
     *             +  PROCOM_SIZE + 511) / 512
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C OPEN DUMP FILE ...
C USE OPENW ONLY TO GET FILE SIZE.
C
	CALL OPENW(1, NETCOMFIL, 4, 0, 0, ST)
	CALL VAXGETFSIZ(1, DUMP_FILE_SIZE)
	CALL USRCLOS1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RE-OPEN DUMP FILE & INITIALIZE THE FILE DESCRIPTOR BLOCK.
C
	CALL OPENQW(1, NETCOMFIL, 4, 0, 0, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_RESTORE - CANNOT OPEN DUMP FILE ***', ST, 0)
	  GOTO 9999
	ENDIF
C
	CALL IOQINIT(FDB, 1, 512)
C
C DETERMINE THE NUMBER OF IMAGES POSSIBLE.
C
	NUM_IMAGES = DUMP_FILE_SIZE / IMAGE_SIZE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ASK USER WHICH IMAGE TO SELECT.
C
100	CONTINUE
        TYPE *, IAM(), 'NETCOM.FIL MAY CONTAIN ', NUM_IMAGES, ' IMAGES'
C
        CALL INPNUM('SELECT IMAGE NUMBER [E-EXIT] ',
     *              OPT, 1, NUM_IMAGES, EXT)
C
	IF (EXT .LT. 0) GOTO 200
C
	NEXT_BLOCK = (OPT * IMAGE_SIZE) - IMAGE_SIZE + 1         
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ NETCOM FIRST ...
C
	CALL READQIO(FDB, NEXT_BLOCK, FRST_NETCOM, NET_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_RESTORE - ' //
     *             'CANNOT READ NETCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
	NEXT_BLOCK = NEXT_BLOCK + (NET_SIZE + 511) / 512
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ DCNCOM ...
C
	CALL READQIO(FDB, NEXT_BLOCK, FRST_DN_EVERYTHING, DN_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_RESTORE - ' //
     *             'CANNOT READ DCNCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
	NEXT_BLOCK = NEXT_BLOCK + (DN_SIZE + 511) / 512
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ CONCOM ...
C
	CALL READQIO(FDB, NEXT_BLOCK, FRST_CONCOM, CONCOM_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_RESTORE - ' //
     *             'CANNOT READ CONCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
	NEXT_BLOCK = NEXT_BLOCK + (CONCOM_SIZE + 511) / 512
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ PROCOM.....
C
	CALL READQIO(FDB, NEXT_BLOCK, FRST_PROCOM, PROCOM_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_RESTORE - ' //
     *             'CANNOT READ PROCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOOP BACK.
C
        GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CLOSE THE FILE.
C
200	CONTINUE
	CALL USRCLOSQ1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
