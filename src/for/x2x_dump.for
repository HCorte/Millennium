C
C *** X2X_DUMP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2X_DUMP.FOV                                 $
C  $Date::   17 Apr 1996 16:46:02                                         $
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
C	DUMP X2X COMMONS TO DISK ... AN UNSOPHISTICATED VERSION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE X2X_DUMP
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
C LOCAL DECLARATIONS
C
	INTEGER*4	X2XCOM_SIZE,
     *			LANCOM_SIZE,
     *			DUMP_FILE_SIZE,
     *			FDB(7),
     *                  IMAGE_SIZE,
     *			X2XCOMFIL(5) /'X2XC', 'OM.F', 'IL  ',
     *                                '    ', '    '/,
     *			NEXT_REC/1/,
     *			ST
        INTEGER*4       VAXBLK
        PARAMETER       (VAXBLK=512)
        INTEGER*4       BLKS_PER_REC
        PARAMETER       (BLKS_PER_REC=64)
        INTEGER*4       REC_LEN
        PARAMETER       (REC_LEN=VAXBLK*BLKS_PER_REC)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C THE FOLLOWING STATEMENT IS NOT ABSOULTELY NECESSARY UNDER VAX FORTRAN ...
C HOWEVER, IT WOULD BE REQUIRED TO COMPLY WITH THE ANSI F77 SPECIFICATION.
C
	SAVE		NEXT_REC
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DETERMINE SIZE OF COMMONS.
C
	X2XCOM_SIZE = %LOC(LAST_X2XCOM) - %LOC(FRST_X2XCOM)
C
	LANCOM_SIZE = %LOC(LAST_LANCOM) - %LOC(FRST_LANCOM)
C
        IMAGE_SIZE  = X2XCOM_SIZE + LANCOM_SIZE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C OPEN DUMP FILE USE OPENW ONLY TO GET FILE SIZE
C
10	CONTINUE
	CALL OPENW(1, X2XCOMFIL, 4, 0, 0, ST)
C
C IF FILE DOES NOT EXIST CREATE IT (HOLDS ONLY 1 IMAGE)
C
	IF (ST .EQ. -1) THEN
	  CALL CRTFIL(X2XCOMFIL, ((IMAGE_SIZE + REC_LEN-1) / REC_LEN)*
     *		      BLKS_PER_REC, ST)
	  IF (ST .NE. 0) THEN
	    CALL OPS('*** X2X_DUMP - CANNOT CREATE DUMP FILE ***',
     *               ST, 0)
	    GOTO 9999
	  ENDIF
	  GOTO 10
	ENDIF
C
	IF (ST.NE.0) THEN
	  CALL OPS('*** X2X_DUMP - CANNOT OPEN DUMP FILE ***', ST, 0)
	  GOTO 9999
	ENDIF
C
C GET THE SIZE OF THE FILE & CLOSE IT.
C
	CALL VAXGETFSIZ(1, DUMP_FILE_SIZE)
C
	CALL USRCLOS1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RE-OPEN DUMP FILE & INITIALIZE THE FILE DESCRIPTOR BLOCK.
C
	CALL OPENQW(1, X2XCOMFIL, 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  CALL OPS('*** X2X_DUMP - CANNOT OPEN DUMP FILE ***', ST, 0)
	  GOTO 9999
	ENDIF
C
	CALL IOQINIT(FDB, 1, REC_LEN)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IT IS INTERESTING IF THE SYSTEM DIES DURING THIS I/O ...
C WRITE X2XCOM ...
C
	CALL WRITEQIO(FDB, NEXT_REC, FRST_X2XCOM, X2XCOM_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** X2X_DUMP - CANNOT DUMP X2XCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IT IS INTERESTING IF THE SYSTEM DIES DURING THIS I/O ...
C WRITE LANCOM ...
C
	NEXT_REC = NEXT_REC + (X2XCOM_SIZE + REC_LEN-1) / REC_LEN
C
	CALL WRITEQIO(FDB, NEXT_REC, FRST_LANCOM, LANCOM_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** X2X_DUMP - CANNOT DUMP LANCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C ROUND ROBIN IF NOT ENOUGH ROOM ...
C
	NEXT_REC = NEXT_REC + (LANCOM_SIZE + REC_LEN-1) / REC_LEN
C
	IF (NEXT_REC   +
     *      (X2XCOM_SIZE + REC_LEN-1) / REC_LEN +
     *	    (LANCOM_SIZE + REC_LEN-1) / REC_LEN .GT. DUMP_FILE_SIZE)
     *    NEXT_REC = 1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CLOSE THE DUMP FILE.
C
        CALL USRCLOSQ1(1)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
