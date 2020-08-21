C  GXSRC:NET_FREEBUF.FOR
C
C SUBROUTINE NET_FREEBUF
C
C V02 09-MAR-2001 UXN Queue checking re-written.
C V01 21-NOV-1996 PJS COPY OF DN_FREEBUF FOR NETLOG & NETMON.
C                   THIS DOES NOT CALL SYS$SETAST().
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
C Purpose: Will check any queue if data is on the list
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
        SUBROUTINE NET_FREEBUF(BUF, TIMES)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'                    ! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
C
C LOCAL DECLARATIONS
C
        INTEGER*4       BUF,
     *                  ST,
     *                  TIMES
C
C Check queue corruption
C
	CALL CHKQCOR( DCN_FREE, BUF, TIMES)
        CALL ABL(BUF, DCN_FREE, ST)
C
        END
