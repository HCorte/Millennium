C SUBROUTINE NT_FLUSH
C
C V02 16-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 13-NOV-1992 DAS INITIAL RELEASE.
C
C THIS ROUTINE WILL FLUSH ANY MESSAGES WHICH MAY EXIST IN NOTPRO'S MAILBOX
C THESE MAY BE LEFT OVER FROM A PREVIOUS INCARNATION OF NOTPRO. NOTPRO SHOULD
C BE THE FIRST PROCESS STARTED.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE NT_FLUSH
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:NOTEVN.DEF'
        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
        INTEGER*4   FUNCOD
        INTEGER*4   STATUS
        INTEGER*4   I4MESS(25)
        CHARACTER   MESS*100
        EQUIVALENCE (I4MESS,MESS)
C
        STRUCTURE /NT_IOSSTRUCT/
          INTEGER*2 STAT                          !VMS STATUS
          INTEGER*2 XSIZE                         !TRANSFER SIZE
          INTEGER*4 PARM
        END STRUCTURE
        RECORD /NT_IOSSTRUCT/ LOCAL_IOSB
C
C PROCESS READ IMMEDIATELY
C
        FUNCOD=IO$_READVBLK .OR. IO$M_NOW
C
10      CONTINUE
        STATUS=SYS$QIO(,%VAL(NT_MESCHANNEL),%VAL(FUNCOD),
     *                  LOCAL_IOSB,,,I4MESS,%VAL(100),,,,)
        IF(LOCAL_IOSB.STAT .EQ. SS$_ENDOFFILE) THEN
          RETURN
        ELSE
          GOTO 10
        ENDIF
C
        END    
