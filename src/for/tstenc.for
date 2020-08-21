C  GXSRC:TSTENC.FOR
C
C  $Log:   GXAGBR:[GOLS]TSTENC.FOV  $
C  
C     V03 13-MAR-03 GPW DESENCR TAKEN FROM UK
C     V02 08-MAY-01 MAC DES ENCRYPTION TEST
C
C     Rev 1.1   14 May 1998 13:02:40   NXA
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    14 May 1998 13:02:22   NXA
C  Initial revision.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM TSTENC
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'     
C
        INTEGER*4 TMP1,TMP2
         BYTE BTMP1(4),BTMP2(4)
         EQUIVALENCE(BTMP1,TMP1)
         EQUIVALENCE(BTMP2,TMP2)
        INTEGER*4 I,J, LENGTH                          !V02
        INTEGER*4 TSTPRO(PROLEN)
        INTEGER*2 HTSTPRO(PROLEN*2)
        BYTE      BTSTPRO(PROLEN*4)
        BYTE      OUTPRO(PROLEN*4)
        EQUIVALENCE (BTSTPRO(1), HTSTPRO(1), TSTPRO(1))
C
C SET UP KEY
C


              STOP



        CALL ENCINI1(1234,1)
C
C PERFORM SOFT ENC
C

        TMP1=KEYTAB(1,1)
        TMP2=KEYTAB(2,1)
        WRITE(*,123) (BTMP1(I),I=1,4),(BTMP2(I),I=1,4)
123     FORMAT(1X,'NEW',1X,4Z2.2,1X,4Z2.2)

        TSTPRO(OUTLEN)=12
        DO 100 I=0,11
           BTSTPRO(BOUTTAB+I)=I
100     CONTINUE
        WRITE(5,900) 'BEFORE ENC  ',
     *               (BTSTPRO(BOUTTAB+I),I=0,11),KEYTAB(1,1),
     *               KEYTAB(2,1)
C                                                         !V02...
                LENGTH=12
                CALL DES_ENCRYPT_DATA( 
     *                  BTSTPRO(BOUTTAB),                 ! input
     *                  %VAL(LENGTH),                     ! message length
     *                  DES_KEY_SCHEDULE(1,1,1),          ! DES key schedule
     *                  %VAL(1),                          ! ENCRYPT
     *                  BTSTPRO(BOUTTAB))                 ! output
C                                                         !...V02

        WRITE(5,900) 'AFTER ENC   ',
     *               (BTSTPRO(BOUTTAB+I),I=0,11),KEYTAB(1,1),
     *               KEYTAB(2,1)
      
C
C PERFORM SOFT DEENC
C                                                         !V02...
                LENGTH=12
                CALL DES_ENCRYPT_DATA( 
     *                  BTSTPRO(BOUTTAB),                 ! input
     *                  %VAL(LENGTH),                     ! message length
     *                  DES_KEY_SCHEDULE(1,1,1),          ! DES key schedule
     *                  %VAL(0),                          ! DECRYPT
     *                  OUTPRO(BOUTTAB))                 ! output
C                                                         !...V02 
        WRITE(5,900) 'AFTER DESENC ',
     *               (OUTPRO(BOUTTAB+I),I=0,11),KEYTAB(1,1),
     *               KEYTAB(2,1)

       
              TYPE*,' '

           DO I=1,16

               WRITE(*,901) (DES_KEY_SCHEDULE(J,I,1),J=1,8)               
           ENDDO
        STOP
C
C
C
900     FORMAT (1X,A12,1X,12(Z2.2),1X,Z8.8,1X,Z8.8)
901     FORMAT(1X,8(Z2.2,1X))
        END
