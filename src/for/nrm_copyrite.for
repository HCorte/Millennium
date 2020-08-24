C
C SUBROUTINE COPYRITE
C COPYRITE.FOR
C
C V13 11-AUG-2011 RXK Wait for 100 millisec added
C V12 13-DEC-2010 FJG Update Year
C V11 24-JUL-2000 UXN OPSTXT() and ISDETACHED() added.
C V10 10-FEB-2000 UXN CALL CLOSE() replaced with CLOSE()
C V09 01-JAN-2000 UXN ADDED 2000
C V08 18-FEB-1997 HXK Changed year from 1996 to 1997
C V07 15-DEC-1995 HXK Updated for 1996
C V06 08-JAN-1994 HXK change copyright to 1994
C V05 10-FEB-1993 EBD Added 1993 to copyrite text
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 08-JAN-1992 DAS ADDED 1992                          
C V02 06-FEB-1991 KWP ALSO INCLUDE COPYRITX IN SAME SOURCE
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C V01 14-JUN-90 KWP INITIAL RELEASE
C
C THIS SOURCE CONTAINS THE ONLY TWO VALID ROUTINES THAT CAN BE
C USED TO DISPLAY THE COPYRIGHT NOTICE ON ALL VAX FORTRAN SYSTEMS.
C
C COPYRITE    DISPLAYS COPYRIGHT NOTICE TO SYS$OUTPUT
C COPYRITX    CLEARS THE SCREEN, CALLS COPYRITE, WAITS 2 SECONDS
C
C
C SUBROUTINE TO DISPLAY GTECH COPYRIGHT INFORMATION TO SYSTEM CONSOLE
C
C THIS ROUTINE SHOULD BE THE FIRST ROUTINE CALLED IN ALL MAIN LINE
C PROGRAMS OF THE GTECH SYSTEM.
C
C     eg. CALL COPYRITE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991-2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE COPYRITE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INTEGER*4     ST
	CHARACTER*54  COPYINFO      !Copyright Information
	DATA COPYINFO/
     *	 'Copyright 2011 GTECH Corp. All rights reserved.'/
C
	IF(ISDETACHED()) THEN
	   CALL OPSTXT(COPYINFO)                    ! Send message to NOTPRO
        ELSE
           WRITE(6,900) IAM(), COPYINFO             ! write to the console
	ENDIF
        CALL XWAIT (100,1,ST)
        RETURN
C
900     FORMAT(1X,A18,A54)
C
	END
