C  GXSRC:ENCINI.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENCINI.FOV  $
C  
C     Rev 1.8   02 Feb 1995 17:19:48   CONSOLE
C  UPDATED TO SUPPORT BOTH SOFT AND HARD ENCYRPTION
C  
C     Rev 1.7   21 Jan 1995 18:00:46   JJOLY
C  SET BIT ON IF ODD PARITY
C  
C     Rev 1.6   21 Jan 1995 16:19:08   JJOLY
C  CHANGED NOT TO MOD PASSNUMBER BY 10,000 TWICE
C  
C     Rev 1.5   17 Jan 1995 11:21:28   JJOLY
C  PERFORM SOFT ENC ON NON ON-LINE TERMINALS
C  
C     Rev 1.4   09 Nov 1994 20:42:52   MCM
C  SET THE PASSNUMBER OFFSET TO 1 IF NOT SET
C  
C     Rev 1.3   21 Jun 1994 17:17:16   MCM
C  DO NOT CLEAR PASSWORD OFFSET IF CLERKS ARE NOT ACTIVE
C  
C     Rev 1.2   08 Jun 1994 14:25:34   MCM
C  CHANGED PASSNUMBER OFFSET FROM A HALFWORD TO A BYTE
C  
C     Rev 1.1   03 Jan 1994 20:20:52   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:41:22   SYSTEM
C  Initial revision.
C
C
C V08 14-MAR-03 GPW DESENCR AGTBTB->AGTHTB (PASS NUMBER)
C V07 13-MAR-03 GPW DESENCR TAKEN FROM UK
C V06 08-MAY-01 GLS REPLACED ENCINI1 BY DES ENCINI1
C V05 31-JUL-92 NJA CHANGED TO CALL ENCINI1 ALL THE TIME.
C V04 13-MAR-92 TKO Fix to flip bytes around for Concurrent compatability
C V03 14-NOV-91 TKO Cleaned up and added DES (even parity)
C V02 01-APR-91 WS  MODIFIED TO SUPPORT SOFT AND DES ENCRYPTION
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This source comprises 2 routines which may be called by other programs.
C
C ENCINI1 may be called whenever it is desired to change a specific terminal's
C         encryption key.  It is called by SPESRV (SPE_SON), RESET & NETMGR
C         (REPROTRA).
C
C Calling sequence:
C
C       CALL ENCINI1( PASS, TERM)
C
C Input:
C       PASS    = 4 digit passnum
C       TERM    = terminal number to change
C
C --------------------------------------------------------------
C
C ENCINI  may be called whenever it is desired to change the passwords of
C         a group (1 or more) of terminals to their normal values (based on
C         the current offset of PASSOFFSET).  It is called by ENCPRO and RESET
C         for all terminals and by CMDPRO (CMDAGT) for a specific terminal.
C
C Calling sequence:
C
C       CALL ENCINI (BEGTERM, ENDTERM)
C
C Input:
C
C       BEGTERM = Beginning terminal #
C       ENDTERM = Ending terminal #
C
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
C
C *** ENCINI
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCINI(BEGTERM,ENDTERM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
C
        INTEGER*4   BEGTERM
        INTEGER*4   ENDTERM
C
        INTEGER*4 PASS, OFF
C
        IF (BEGTERM.LT.1 .OR. ENDTERM.GT.NUMAGT) GOTO 9000
C
        DO 100, OFF = BEGTERM, ENDTERM
          IF (AGTHTB(AGTPASOFF,OFF).LE.0) AGTHTB(AGTPASOFF,OFF)=1
          PASS = AGTTAB(APSNUM+AGTHTB(AGTPASOFF,OFF)-1,OFF)

CCCCCCCC            IF(OFF.LE.10) TYPE*,'ENCINI,TER,PASS',OFF,PASS     !%%GPW


C
          CALL ENCINI1(PASS,OFF)
C
100     CONTINUE
C
9000    CONTINUE
        RETURN
        END
C
C
CCCCC THIS VERSION OF ENCINI1 IS REPLACED BY A NEW ONE (SEE BELLOW)  !V06
C
C *** ENCINI1
C
C       OPTIONS /CHECK=NOOVERFLOW
C       SUBROUTINE ENCINI1(PASS,TER)
C       IMPLICIT NONE
C
C       INCLUDE 'INCLIB:SYSPARAM.DEF'
C       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C       INCLUDE 'INCLIB:GLOBAL.DEF'
C       INCLUDE 'INCLIB:CONCOM.DEF'
C       INCLUDE 'INCLIB:PRMPRO.DEF'
C       INCLUDE 'INCLIB:AGTCOM.DEF'
C       INCLUDE 'INCLIB:ENCCOM.DEF'
C
C       INTEGER*4   FLIP4
C       EXTERNAL    FLIP4
C
C       INTEGER*4   PASS
C       INTEGER*4   TER
C
C       INTEGER*4 BASE/Z69696969/
C       INTEGER*4 KEY, PASN, M, K, I
C       INTEGER*4 TEMP(2)
C
C       PASN=MOD(PASS,10000)
C       CALL BINASC(KEY,1,4,PASN)
C
C       KEY = FLIP4(KEY)
C       TEMP(1)=KEY
C       TEMP(2)=BASE-KEY
C
C       TEMP(1)=ISHFT(TEMP(1),1)
C       TEMP(2)=ISHFT(TEMP(2),1)
C
C REMOVED FLIP SO WE MATCH GVT
C
C       IF(P(DESFLG_TYPE).EQ.DESFLG_HARD.AND.
C     *    TSBIT(AGTTAB(AGTTYP,TER),AGTTOI)) THEN
C         TEMP(1) = FLIP4(TEMP(1))
C         TEMP(2) = FLIP4(TEMP(2))
C        ENDIF
C
C        CALL ENCPARITY(TEMP)             !SET PARITY FOR KEY
C
C        KEYTAB(1,TER)=TEMP(1)
C        KEYTAB(2,TER)=TEMP(2)
C
C Set up HALF_KEYTAB in case we are using soft encryption
C
C       M = 0
C        HALF_KEYTAB(1,TER)=TEMP(1)
C        HALF_KEYTAB(2,TER)=TEMP(2)
C       DO 50 I = 1, 2
C             DO 30 K = 0, 31
C              IF (BTEST(KEYTAB(I,TER),K)) THEN
C                 M = M + 1
C                 IF (MOD(M,2) .EQ. 1)
C     *                   HALF_KEYTAB(I,TER)=IBCLR(HALF_KEYTAB(I,TER),K)
C                 ENDIF
C30           CONTINUE
C             HALF_KEYTAB(I,TER)=ISHFTC(HALF_KEYTAB(I,TER),7,32)
C50       CONTINUE
C
C       RETURN
C       END
C
C
C
C *** ENCPARITY (called only from above routines)
C
C This routine will set odd parity for an 8 byte key
C
C Calling sequence:
C
C       CALL ENCPARITY(KEY)
C
C Input:
C       KEY = 8-byte key.
C
C Output:
C
C       KEY = Each byte changed to be odd parity (parity bit replaces
C             the low order bit in each byte).
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCPARITY(KEY)
        IMPLICIT NONE

        INCLUDE 'INCLIB:PARITY.DEF'

        BYTE    KEY(8)

        INTEGER*4   K,I4VAL


        DO 900 K = 1, 8
          I4VAL  = KEY(K)
          I4VAL  = IAND(I4VAL,'FE'X)
          IF(PARITY(I4VAL).EQ.EVN)THEN
            I4VAL = IOR(I4VAL,1)
          ENDIF
          KEY(K) = I4VAL
900     CONTINUE
C
        RETURN
        END
C
C__________________________________________________________ !V06
C
C SUBROUTINE ENCINI1
C
C V02 24-APR-2001 UXN SOFT DES ENCRYPTION ADDED.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
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
C     SET ENCRYPTION KEY
C
C     ENCINI1(PASS,TER)
C     IN - PASS - TAERMINAL PASS #
C          TER - TERMINAL #
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCINI1(PASS,TER)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
C
        INTEGER*4 BASE/Z69696969/
        INTEGER*4 KEY, PASN, TER, PASS
        INTEGER*4 TEMP(2)
C
        PASN=MOD(PASS,10000)
        CALL BINASC(KEY,1,4,PASN)
        TEMP(1)=KEY
        TEMP(2)=BASE-KEY
        TEMP(1)=ISHFT(TEMP(1),1)
        TEMP(2)=ISHFT(TEMP(2),1)
C*V06*            CALL ENCSWAP(TEMP)   
        KEYTAB(1,TER)=TEMP(1)
        KEYTAB(2,TER)=TEMP(2)

        CALL DES_EXPAND_KEY( KEYTAB(1,TER), DES_KEY_SCHEDULE(1,1,TER) )

        RETURN
        END
