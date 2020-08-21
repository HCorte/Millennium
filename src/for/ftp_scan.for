C FTP_SCAN.FOR
C
C $Log:   GXAFXT:[GOLS]FTP_SCAN.FOV  
C  
C     Rev 1.0   17 Apr 1996 13:15:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Dec 1994 15:45:00   JXP
C  Initial revision.
C  
C     Rev 1.0   23 Oct 1994 12:58:44   GXA
C  Initial revision.
C
C This program will build a bitmap from the winning numbers entered by result,
C start the TM and Draw file Bingo scanning programs (TBNGSCAN, DBNGSCAN) 
C and wait for their completion. On the completion of both scanning programs,
C number of winners for each division is going to be stored in RESCOM.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM FTPSCAN
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 TSKSTS(2)             !Task status
	INTEGER*4 ST			!Subroutine return status
C
C
	CALL COPYRITE
	TYPE*,IAM()
	TYPE*,IAM(),
     *  '<<<<< Bingo Fullhouse seeds & cancels Scanning Program  V01 >>>>>'
	TYPE*,IAM()
C
C START SCANNING TASKS
C
	CALL NRUNTSK(8HTFTPSCAN)
	CALL NRUNTSK(8HDFTPSCAN)
C
C
C WAIT FOR THEM TO COMPLETE
C
        ST = 0
        DO WHILE (ST.NE.4)
            CALL XWAIT(5,2,ST)
            CALL STTSK(8HTFTPSCAN,TSKSTS,ST)
        ENDDO

        ST = 0
        DO WHILE (ST.NE.4)
            CALL XWAIT(5,2,ST)
            CALL STTSK(8HDFTPSCAN,TSKSTS,ST)
        ENDDO
C
	END 
