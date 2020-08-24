C RUNCMD.FOR
C
C V04 24-JAN-2011 RXK Change for LIB$WAIT
C V03 17-JUN-2000 UXN CDEC$ OPTIONS /WARNING=NOALIGNMENT added for QUOTA_DEF
C V02 08-JUN-2000 OXK Non-used variables removed, SETDES removed.
C V01 11-DEB-1999 GPW INITIAL RELEASE FOR POLAND
C
C
C SUBROUTINE TO EXECUTE DCL COMMANDS FROM FORTRAN PROGRAMS (RUNTSK & NRUNTSK)
C 
C    FILCMD - FILE NAME, WHERE GROUP OF DCL COMMANDS IS WRITTEN 
C             FOR EXAMPLE :   $ DELETE AAA.FIL.*
C              
C    PRCNAM - PROCCESS NAME (INPUT) -  ANY UNIQUE NAME (MAX. 8 CHARACTERS)
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
C
      SUBROUTINE RUNCMD(FILCMD,PRCNAM)
      IMPLICIT NONE
C
      INCLUDE '($SYSSRVNAM)'
      INCLUDE '($PRVDEF)'
      INCLUDE '($PQLDEF)'
      INCLUDE '($ACLDEF)'
      INCLUDE '($PRCDEF)'
C
      BYTE PRCB(8)
      CHARACTER * 20 OUTPUT
      CHARACTER * (*) FILCMD,PRCNAM
      CHARACTER * 12 PRCNAME
      CHARACTER * 8 TERMNAME
      CHARACTER * 63 OUT
C
      INTEGER * 4 LL
      INTEGER * 4 PRVADR(2),BASPRI,STATUS,PIDADR
      INTEGER * 4 TSKSTS(2)
      INTEGER * 4 UIC,L,I,K,NFLG
      INTEGER * 2 I2UIC(2)
      EQUIVALENCE(UIC,I2UIC)
      INTEGER * 4 STSFLG,I4PRCNAM,PREFIX_LEN
      EQUIVALENCE(I4PRCNAM,PRCNAME)
C
CDEC$ OPTIONS /WARNING=NOALIGNMENT
      STRUCTURE      /QUOTA_DEF/
         BYTE         QUOTA_TYPE
         INTEGER * 4  QUOTA_AMOUNT
      END STRUCTURE
CDEC$ END OPTIONS 
      RECORD /QUOTA_DEF/ QUOTA(PQL$_LENGTH+1)
C
C
      NFLG=1
      GO TO 1
C
C
      ENTRY NRUNCMD(FILCMD,PRCNAM)      
      NFLG=0
C      
    1 CONTINUE      
C    
      DO I=1,8
        PRCB(I)=0
      ENDDO
C
      LL=LEN(PRCNAM)
      L=0
      LL=MIN0(LL,8)
      DO 3 I=1,LL
        IF(PRCNAM(I:I).EQ.' ') GO TO 4
        L=L+1
        PRCB(L)=ICHAR(PRCNAM(I:I))
    3 CONTINUE
    4 CONTINUE
C
      CALL GETPRFX(I4PRCNAM,PREFIX_LEN)
      K=PREFIX_LEN+1
C
      DO I=1,L
          PRCNAME(K:K)=PRCNAM(I:I)
          K=K+1
      ENDDO
C
      PRVADR(1)='FFFFFFFF'X
      PRVADR(2)='FFFFFFFF'X
C
      QUOTA(1).QUOTA_TYPE   =PQL$_WSDEFAULT
      QUOTA(1).QUOTA_AMOUNT =10000
      QUOTA(2).QUOTA_TYPE   =PQL$_WSQUOTA
      QUOTA(2).QUOTA_AMOUNT =65000
      QUOTA(3).QUOTA_TYPE   =PQL$_WSEXTENT
      QUOTA(3).QUOTA_AMOUNT =65000
      QUOTA(4).QUOTA_TYPE   =PQL$_LISTEND
C
C
      STSFLG=PRC$M_DISAWS.OR.PRC$M_PSWAPM
C
      I2UIC(1)=0 
      I2UIC(2)=0
C
C
      BASPRI=6
C
      OUTPUT(1:20)=' '
      OUT(1:63)=' '
      OUT=OUTPUT
      L=LEN(OUTPUT)
      IF(OUTPUT(1:L).EQ.' ') THEN
         CALL GETTERM(TERMNAME,STATUS)
         OUT=TERMNAME
      ENDIF
C
      STATUS=SYS$CREPRC(PIDADR,                             !PIDADR
     *                  'SYS$SYSTEM:LOGINOUT.EXE', 
     *                  FILCMD,
     *                  TERMNAME,
     *                  TERMNAME,
     *                  PRVADR,                             !PRVADR
     *                  QUOTA,                              !QUOTA
     *                  PRCNAME(1:K-1),
     *                  BASPRI,                             !BASPRI
     *                        ,                             !UIC
     *                        ,                             !MBXUNT
     *                        )                             !STSFLG
C
       CALL LIB$WAIT(0.25,0,4)

       IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
      IF(NFLG.EQ.0) GO TO 1000
C
    2 CONTINUE
      CALL XWAIT(2,2,STATUS)
      CALL STTSK(PRCB,TSKSTS,STATUS)
      IF(STATUS.NE.4) GO TO 2
C
 1000 CONTINUE
      RETURN
      END
