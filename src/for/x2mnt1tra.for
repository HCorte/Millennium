C
C SUBROUTINE X2MNT1TRA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MNT1TRA.FOV                                $
C  $Date::   17 Apr 1996 16:23:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2mnttra.for;1 **
C
C X2MNTTRA.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This routine will decode the Front End maintainance messages
C into individual field values
C
C Routines contained include:
C
C     X2MNT1TRA(MESS)
C     X2MNT2TRA(MESS)
C     X2MNT3TRA(MESS)
C
C NOTE:  data values are returned in the respective tables
C in X2MAINT.DEF
C
C ===========================================================
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
	SUBROUTINE X2MNT1TRA(MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2MAINT.DEF'
C
	INTEGER*4       MESS(400)           !Input buffer
C
C EXTRACT DATA FROM MESSAGE TYPE 1.
C
	CALL ILBYTE(X2MNT1_DATA(1),MESS,
     *	              X2MAINT_T1M_LINE_NO-1)
	CALL ILBYTE(X2MNT1_DATA(2),MESS,
     *	              X2MAINT_T1M_LINE_STATUS-1)
	CALL MOV2TOI4(X2MNT1_DATA(3),MESS,
     *	              X2MAINT_T1M_NO_CONN_VC-1)
	CALL MOV2TOI4(X2MNT1_DATA(4),MESS,
     *	              X2MAINT_T1M_NO_DISCONN-1)
	CALL MOV2TOI4(X2MNT1_DATA(5),MESS,
     *	              X2MAINT_T1M_TOT_NO_DAT_PACK_REC-1)
	CALL MOV2TOI4(X2MNT1_DATA(6),MESS,
     *	              X2MAINT_T1M_TOT_NO_DAT_PACK_SEN -1)
	CALL MOV2TOI4(X2MNT1_DATA(7),MESS,
     *	              X2MAINT_T1M_NO_AVAIL_LOG_CRC-1)
	CALL MOV2TOI4(X2MNT1_DATA(8),MESS,
     *	              X2MAINT_T1M_TOT_NO_ERR-1)
	CALL MOV2TOI4(X2MNT1_DATA(9),MESS,
     *	              X2MAINT_T1M_LINE_UTIL-1)
C
	RETURN
	END
