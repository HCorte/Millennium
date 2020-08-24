C
C *** SUBROUTINE X2X_RESTORE ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2X_RESTORE.FOV                              $
C  $Date::   17 Apr 1996 16:46:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V01 13-DEC-94 GPR RELEASED FOR UK
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
C	RESTORE X2XCOM FROM DISK ... AN UNSOPHISTICATED VERSION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE X2X_RESTORE
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4	X2XCOM_SIZE,
     *			LANCOM_SIZE,
     *			DUMP_FILE_SIZE,
     *			EXT,
     *			FDB(7),
     *                  IMAGE_SIZE,
     *			X2XCOMFIL(5)	/'X2XC', 'OM.F', 'IL  ',
     *					 '    ', '    '/,
     *			NEXT_REC,
     *                  NUM_IMAGES,
     *			OPT,
     *			ST
C
        INTEGER*4       VAXBLK
        PARAMETER       (VAXBLK=512)
        INTEGER*4       BLKS_PER_REC
        PARAMETER       (BLKS_PER_REC=64)
        INTEGER*4       REC_LEN
        PARAMETER       (REC_LEN=VAXBLK*BLKS_PER_REC)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DETERMINE SIZE OF COMMONS.
C
	X2XCOM_SIZE = %LOC(LAST_X2XCOM) - %LOC(FRST_X2XCOM)
C
	LANCOM_SIZE = %LOC(LAST_LANCOM) - %LOC(FRST_LANCOM)
C 
	IMAGE_SIZE = (X2XCOM_SIZE + REC_LEN-1) / REC_LEN +
     *		     (LANCOM_SIZE + REC_LEN-1) / REC_LEN
 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C OPEN DUMP FILE ...
C USE OPENW ONLY TO GET FILE SIZE.
C
	CALL OPENW(1, X2XCOMFIL, 4, 0, 0, ST)
	CALL VAXGETFSIZ(1, DUMP_FILE_SIZE)
	CALL USRCLOS1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RE-OPEN DUMP FILE & INITIALIZE THE FILE DESCRIPTOR BLOCK.
C
	CALL OPENQW(1, X2XCOMFIL, 4, 0, 0, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** X2X_RESTORE - CANNOT OPEN DUMP FILE ***', ST, 0)
	  GOTO 9999
	ENDIF
C
	CALL IOQINIT(FDB, 1, REC_LEN)
C
C DETERMINE THE NUMBER OF IMAGES POSSIBLE.
C
	DUMP_FILE_SIZE = DUMP_FILE_SIZE / BLKS_PER_REC
	NUM_IMAGES = DUMP_FILE_SIZE / IMAGE_SIZE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ASK USER WHICH IMAGE TO SELECT.
C
100	CONTINUE
        TYPE *, IAM(), 'X2XCOM.FIL MAY CONTAIN ', NUM_IMAGES, ' IMAGES'
C
        CALL INPNUM('SELECT IMAGE NUMBER [E-EXIT] ',
     *              OPT, 1, NUM_IMAGES, EXT)
C
	IF (EXT .LT. 0) GOTO 200
C
	NEXT_REC = (OPT * IMAGE_SIZE) - IMAGE_SIZE + 1         
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ X2XCOM FIRST ...
C
	CALL READQIO(FDB, NEXT_REC, FRST_X2XCOM, X2XCOM_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** X2X_RESTORE - ' //
     *             'CANNOT READ X2XCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
	NEXT_REC = NEXT_REC + 
     *		   (X2XCOM_SIZE + REC_LEN-1) / REC_LEN

C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ LANCOM NEXT ...
C
	CALL READQIO(FDB, NEXT_REC, FRST_LANCOM, LANCOM_SIZE, ST)
C
	IF (ST .NE. 0) THEN
	  CALL OPS('*** X2X_RESTORE - ' //
     *             'CANNOT READ LANCOM FROM THE FILE ***', ST, 1)
	ENDIF
C
	NEXT_REC = NEXT_REC + 
     *		   (LANCOM_SIZE + REC_LEN-1) / REC_LEN

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
