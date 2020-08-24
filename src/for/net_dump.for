C
C SUBROUTINE NET_DUMP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_DUMP.FOV                                 $
C  $Date::   17 Apr 1996 14:11:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_dump.for  ***
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
C	DUMP NETCOM AND DCNCOM TO DISK ... AN UNSOPHISTICATED VERSION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_DUMP
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
C LOCAL DECLARATIONS
C
	INTEGER*4	CONCOM_SIZE,
     *			DN_SIZE,
     *			DUMP_FILE_SIZE,
     *			FDB(7),
     *			IMAGE_SIZE,
     *			NETCOMFIL(5) /'NETC', 'OM.F', 'IL  ',
     *                                '    ', '    '/,
     *			NET_SIZE,
     *			NEXT_BLOCK/1/,
     *			PROCOM_SIZE,
     *			ST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C THE FOLLOWING STATEMENT IS NOT ABSOULTELY NECESSARY UNDER VAX FORTRAN ...
C HOWEVER, IT WOULD BE REQUIRED TO COMPLY WITH THE ANSI F77 SPECIFICATION.
C
	SAVE		NEXT_BLOCK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DETERMINE SIZE OF COMMONS.
C
	CONCOM_SIZE = %LOC(LAST_CONCOM) - %LOC(FRST_CONCOM) + 3
C
	DN_SIZE     = %LOC(DN_LAST_IN_COMMON)
     *              - %LOC(FRST_DN_EVERYTHING) + 3
C
	NET_SIZE    = %LOC(LAST_NETCOM) - %LOC(FRST_NETCOM) + 3
C
	PROCOM_SIZE = %LOC(LAST_PROCOM) - %LOC(FRST_PROCOM) + 3
C
C ADD IT ALL UP FOR TOTAL IMAGE SIZE IN BYTES.
C
	IMAGE_SIZE  = CONCOM_SIZE + DN_SIZE + NET_SIZE + PROCOM_SIZE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C OPEN DUMP FILE USE OPENW ONLY TO GET FILE SIZE
C
10	CONTINUE
	CALL OPENW(1, NETCOMFIL, 4, 0, 0, ST)
C
C IF FILE DOES NOT EXIST CREATE IT (HOLDS ONLY 1 IMAGE)
C
	IF (ST .EQ. -1) THEN
	  CALL CRTFIL(NETCOMFIL, (IMAGE_SIZE + 511) / 512, ST)
	  IF (ST .NE. 0) THEN
	    CALL OPS('*** NET_DUMP - CANNOT CREATE DUMP FILE ***',
     *               ST, 0)
	    GOTO 9999
	  ENDIF
	  GOTO 10
	ENDIF
C
	IF (ST.NE.0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT OPEN DUMP FILE ***', ST, 0)
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
	CALL OPENQW(1, NETCOMFIL, 4, 0, 0, ST)
	IF (ST .NE. 0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT OPEN DUMP FILE ***', ST, 0)
	  GOTO 9999
	ENDIF
C
	CALL IOQINIT(FDB, 1, 512)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IT IS INTERESTING IF THE SYSTEM DIES DURING THIS I/O ...
C WRITE NETCOM ...
C
	CALL WRITEQIO(FDB, NEXT_BLOCK, FRST_NETCOM, NET_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT DUMP NETCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IT IS INTERESTING IF THE SYSTEM DIES DURING THIS I/O ...
C WRITE DCNCOM ...
C
	NEXT_BLOCK = NEXT_BLOCK + (NET_SIZE + 511) / 512
C
	CALL WRITEQIO(FDB, NEXT_BLOCK, FRST_DN_EVERYTHING, DN_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT DUMP DCNCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C WRITE CONCOM ...
C
	NEXT_BLOCK = NEXT_BLOCK + (DN_SIZE + 511) / 512
C
	CALL WRITEQIO(FDB, NEXT_BLOCK, FRST_CONCOM, CONCOM_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT DUMP CONCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C WRITE PROCOM ...
C
	NEXT_BLOCK = NEXT_BLOCK + (CONCOM_SIZE + 511) / 512
C
	CALL WRITEQIO(FDB, NEXT_BLOCK, FRST_PROCOM, PROCOM_SIZE, ST)
	IF (ST.NE.0) THEN
	  CALL OPS('*** NET_DUMP - CANNOT DUMP PROCOM TO THE FILE ***',
     *             ST, 1)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ROUND ROBIN IF NOT ENOUGH ROOM ...
C
	NEXT_BLOCK = NEXT_BLOCK + (PROCOM_SIZE + 511) / 512
C
	IF (NEXT_BLOCK   +
     *      (CONCOM_SIZE + 511) / 512 +
     *      (DN_SIZE     + 511) / 512 +
     *      (NET_SIZE    + 511) / 512 +
     *      (PROCOM_SIZE + 511) / 512 .GT. DUMP_FILE_SIZE)
     *    NEXT_BLOCK = 1
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
