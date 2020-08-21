C
C *** PROGRAM X2XDUMP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XDUMP.FOV                                  $
C  $Date::   17 Apr 1996 16:42:36                                         $
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	UTILITY ROUTINE TO DUMP X2X COMMONS TO A FILE AND RESTORE.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM X2XDUMP
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	EXT,
     *			FLAG,
     *			OPT,				! MENU OPTION
     *			ST
C
	CHARACTER*40	NULL,
     *			PROMPT
C
	CHARACTER*20	PASSWRD
C
	CHARACTER*6	PASS,
     *			RESTOREPAS	/'BUGOUT'/
C
	CHARACTER*1	NULLEQV(40)	/40 * Z00/
C
        EQUIVALENCE	(NULL, NULLEQV)
C
        CALL SNIF_AND_WRKSET
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CLEAR THE SCREEN AND DISPLAY THE MENU.
C
100	CONTINUE
	CALL CLRSCR(5)
	WRITE(5, 9000)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C READ THE MENU OPTION.
C
	PROMPT = NULL
	WRITE (PROMPT, 9010)
	CALL INPNUM(PROMPT, OPT, 1, 2, EXT)
	IF (EXT .LT. 0) CALL GSTOP(GEXIT_OPABORT)
C
	GOTO (110, 120) OPT
C
	TYPE *, IAM(), 'INVALID OPTION'
	CALL GSTOP(GEXIT_FATAL)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DUMP THE COMMONS.
C
110	CONTINUE
	TYPE *, IAM(), 'DUMPING X2X COMMONS'
	CALL X2X_DUMP
	CALL XWAIT(5, 2, ST)
	GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RESTORE THE COMMONS.
C
120	CONTINUE
	CALL CLRSCR(5)
	CALL PASSWORD(5, PASSWRD)
	PASS = PASSWRD(1:6)
	IF (PASS .NE. RESTOREPAS) THEN
	  TYPE *, IAM(), 'INVALID PASSWORD'
	  CALL BELLS(3)
	  CALL XWAIT(1, 2, ST)
	  GOTO 120
	ENDIF
C
	TYPE *, IAM(), ' WARNING: IF YOU CONTINUE YOU WILL DESTROY   '
	TYPE *, IAM(), '          ANY CURRENT DATA RESIDING IN MEMORY'
	CALL WIMG(5, 'CONTINUE [Y/N]:')
	CALL YESNO(FLAG)
	IF (FLAG .NE. 1) GOTO 100
C
	TYPE *, IAM(), 'RESTORING X2X COMMONS'
	CALL X2X_RESTORE
	CALL XWAIT(5, 2, ST)
	GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(////, T22, '    GTECH X2X DUMP    ', /,
     *               T22, '1. DUMP X2X COMMONS   ', /,
     *               T22, '2. RESTORE X2X COMMONS', /,
     *               T22, 'E. EXIT               ', //)
9010    FORMAT('     ENTER OPTION [1-2] ')
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END.
C
	END
