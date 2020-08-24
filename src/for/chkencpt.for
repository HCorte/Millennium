C
C SUBROUTINE CHKENCPT
C 
C V06 23-APR-03   GPW FIXED BUG (CONTROL PRINT REMOVED)
C V05 14-MAR-03   GPW DESENCR PRO(TERNUM -> HPRO(TERNUM
C V04 13-MAR-03   GPW DESENCR TAKEN FROM UK
C V03 09-MAY-2001 MAC CORRECTED PRO(TERNUM... , BPRO(BOUTTAB..., BPRO(BINPTAB
C V02 24-APR-2001 UXN SOFT DES ENCRYPTION ADDED.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C*****************************************************************
C
C       CHKENCPT(BUFNUM,TYPE,STATUS)
C       OUT:
C       BUFNUM  -   BUFFER WITH  DATA
C       TYPE    -   OPERATION TYPE
C       STATUS  -   0 IF ANY DATA RECEIVED
C
C       CHECK IF ANY DATA WAS DECRYPTED OR ENCRYPTED
C
C       FOR SOFT ENCRYPTION
C       BUFFERS TO CRYPT RESIDE ON SOF_ENCQUE LIST,
C       BUFFERS TO ENCRYPT HAVE BUFFER POINTER POSITIVE
C       BUFFERS TO DECRYPT HAVE BUFFER POINTER NEGATIVE
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHKENCPT(BUFNUM,TYPE,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
C
        INTEGER*4 BUFNUM,TYPE,STATUS, TERMINAL
        INTEGER*4 LENGTH
C
C
        CALL RTL(BUFNUM,SOFT_ENCQUE,STATUS)
        IF (STATUS.NE.2) THEN
            STATUS = 0               !REMEMBER THERE WAS SOMETHING
            IF (BUFNUM.GT.0) THEN    !THAT WAS ENCRYPTION REQUEST
C
C           DO SOFT ENCRYPTION NOW
C
                TERMINAL = HPRO(TERNUM,BUFNUM)             !V03
C
                LENGTH = HPRO(OUTLEN,BUFNUM)-2
                IF(LENGTH.LT.8) LENGTH = 8

                CALL DES_ENCRYPT_DATA( 
     *                  BPRO(BOUTTAB+2, BUFNUM),          ! input         !V03
     *                  %VAL(LENGTH),                     ! message length
     *                  DES_KEY_SCHEDULE(1,1,TERMINAL),   ! DES key schedule
     *                  %VAL(1),                          ! ENCRYPT
     *                  BPRO(BOUTTAB+2, BUFNUM))          ! output        !V03
C
C
                TYPE=ENC_TYPE_ENCRYPT
            ELSE
C
C           DO DECRYPTION NOW
C
                BUFNUM=-BUFNUM
                TERMINAL=HPRO(TERNUM,BUFNUM)               !V03
         LENGTH = HPRO(INPLEN,BUFNUM)
CCCC         TYPE *,'TERMINAL ',TERMINAL,LENGTH
CCCC         TYPE 900,KEYTAB(1,TERMINAL),KEYTAB(2,TERMINAL)
900     FORMAT(1H ,2(1X,Z8))
D       TYPE 910,(BPRO(OFFSET,BUFNUM),OFFSET=INPTAB*4-3,INPTAB*4-3+LENGTH)
910     FORMAT( 5(1H ,4Z2))

                LENGTH = HPRO(INPLEN,BUFNUM)-2
                IF(LENGTH.LT.8) LENGTH = 8


CCCC             DO I=1,LENGTH+2
CCCC               WRITE(*,123) I, BPRO(BINPTAB+I-1,BUFNUM)
CCCC             ENDDO
CCCC  123       FORMAT(1X,I4,2X,Z2.2)


                CALL DES_ENCRYPT_DATA( 
     *                  BPRO(BINPTAB+2, BUFNUM),          ! input         !V03
     *                  %VAL(LENGTH),                     ! message length
     *                  DES_KEY_SCHEDULE(1,1,TERMINAL),   ! DES key schedule
     *                  %VAL(0),                          ! DECRYPT
     *                  BPRO(BINPTAB+2, BUFNUM))          ! output        !V03
C
D       TYPE *,'After decryption '
D       TYPE 910,(BPRO(OFFSET,BUFNUM),OFFSET=INPTAB*4-3,INPTAB*4-3+LENGTH)
                TYPE=ENC_TYPE_DECRYPT
            ENDIF
        ENDIF
        RETURN
        END
