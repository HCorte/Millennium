C
C SUBROUTINE X2XCLR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2XCLR.FOV                                   $
C  $Date::   17 Apr 1996 16:42:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xclr.for;1 **
C
C X2XCLR.FOR
C
C V02 18-APR-92 JPJ UPDATED TO CLEAR OUT ALL OF X2QBLK
C V01 28-NOV-90 XXX RELEASED FOR VAX
C
C This subroutine will clear the X2XCOM common of all information.
C NOTE: the size is determined dynamically by opening the
C image file and calling GETSIZ to get the number of words to
C clear.
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
	SUBROUTINE X2XCLR
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XQUE.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
        INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	CALL FASTSET(0,FRST_X2XCOM,
     *                ((%LOC(LAST_X2XCOM)-%LOC(FRST_X2XCOM)+3)/4))
	CALL FASTSET(0,X2QBLK,(%LOC(X2X_OUTPUT(NUMPRO+QHEDSZ))
     *                         - %LOC(X2X_OUTPUT(1))+3)/4)
   	CALL FASTSET(0,FRST_X2XREL,
     *                ((%LOC(LAST_X2XREL)-%LOC(FRST_X2XREL)+3)/4))
        CALL FASTSET(0,STNWEIGHT,2*X2X_STATIONS)
	RETURN
	END
