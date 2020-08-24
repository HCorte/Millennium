C
C SUBROUTINE SNDDUMMY
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SNDDUMMY.FOV                                 $
C  $Date::   17 Apr 1996 15:09:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     SNDDUMMY(OFF,WAY,ST)  ;SEND DUMMY TO PROCESSOR OFF
C     IN - OFF   - PROCESSOR # BUFFER IS BEEING SENT TO
C
C        - WAY - WAY WE'RE ON
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
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
	SUBROUTINE SNDDUMMY(OFF,WAY,ST)
	IMPLICIT NONE
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 OFF            !PROCESSOR NR
	INTEGER*4 BUF, ST, WAY
C
D	TYPE *,'SNDDMMY'
	ST=0
C
	IF(OFF.LT.1.OR.OFF.GT.NETSYS)RETURN
C
C Don't send dummys to systems with buffered i/o's queued.
C
	IF(SNDIOCHK(OFF).GT.0)THEN
	    RETURN
	ENDIF
C
	CALL GRABBUF(BUF,WAY,ST)
	IF (ST.EQ.2) THEN
D	  TYPE *,'NO BUFFERS OR FREEZE COMMAND '
D	  PAUSE
          RETURN     !RETURN IF CANNOT SEND BUFFER
	END IF
	ST=0
	NETBUF(MODE,BUF)=DUMMD
	NETBUF(PDEST,BUF)=OFF
	NETBUF(FDEST,BUF)=OFF
	NETBUF(HDRSIZ+1,BUF)=DAYCDC          !SET CDC DATE ;SANITY CHECK
	NETBUF(HDRSIZ+2,BUF)=NETTIMER        !SET TIME DUMMY SENT
	NETDUMM(OFF,WAY)=NETTIMER
	CALL SNDNET(BUF,WAY)
	RETURN
	END
