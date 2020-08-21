C
C PROGRAM X2EXTCHK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2EXTCHK.FOV                                 $
C  $Date::   17 Apr 1996 16:16:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2extchk.for;1 **
C
C X2EXTCHK.FOR
C
C V01 28-APR-91 XXX RELEASED FOR VAX
C
C This program is called from the BLDX2X.COM file and is
C used to check if any modifications have been made to the
C database without running editchecks.
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
	PROGRAM X2EXTCHK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   X2XFILE	    !X2X file number
	CHARACTER   X2FILNAM*20     !File name function
	CHARACTER   BELL /07/	    
C
C
C CLEAR THE SCREEN, DISPLAY THE MENU, AND GET THE
C MENU OPTION.
C
	CALL CLRSCR(5)
	CALL X2CHKEXT(X2XFILE)
	IF(X2XFILE.NE.0) THEN
	  WRITE(5,9040) X2FILNAM(X2XFILE),BELL,BELL,BELL
	ENDIF
	CALL GSTOP(GEXIT_SUCCESS)
C
C     =================== Format Statements ===================
C
9040	FORMAT(1X,'WARNING: File ',A20,' has not been checked ',3(A1))
	END
