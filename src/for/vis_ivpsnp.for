C
C SUBROUTINE IVPSNP
C  
C V02 16-MAR-2011 GPW NUMAGT=12288
C V01 11-OCT-2010 MAC ePASSIVE INVOICE
C
C VIS_IVPSNP.FOR
C
C AGENT PASSIVE INVOICE SNAPSHOT
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE IVPSNP(NUM,LKEY,LNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:RECAGTIV.DEF'
C
	INTEGER*4 FDB(7)
	INTEGER*4 I, J, K, ST
	INTEGER*4 AGT, NUM, LKEY, LNUM
        CHARACTER*1 CHR_FIRM, CHR_SHOP
C
        CHARACTER CZERO
        DATA      CZERO/Z0/
C
	AGT=NUM
	IF(AGT.LT.1.OR.AGT.GT.NUMAGT) AGT=1
	LSTAGT=AGTTAB(AGTNUM,AGT)
	IF(AGT.EQ.LNUM.AND.LKEY.EQ.29) GOTO 100
	CALL OPENW(1,SFNAMES(1,ASFIV),4,0,0,ST)
	CALL IOINIT(FDB,1,ASFIVSEC*256)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,925) (SFNAMES(K,ASFIV),K=1,5),ST
	  RETURN
	ENDIF
	CALL READW(FDB,AGT,ASFIVREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,1023) (SFNAMES(K,ASFIV),K=1,5),ST,AGT
	  RETURN
	ENDIF
C
	CALL USRCLOS1(     1)
C
        CALL OPENW(1,SFNAMES(1,ASF),4,0,0,ST)
        CALL IOINIT(FDB,1,ASFSEC*256)
        IF(ST.NE.0) THEN
          WRITE(CLIN23,925) (SFNAMES(K,ASF),K=1,5),ST
          RETURN
        ENDIF
        CALL READW(FDB,AGT,ASFREC,ST)
        IF(ST.NE.0) THEN
          WRITE(CLIN23,1023) (SFNAMES(K,ASF),K=1,5),ST,AGT
          RETURN
        ENDIF
        DO I=1,512
          IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
        ENDDO
        CALL USRCLOS1(     1)
C
100	CONTINUE
C
C ENCODE INVOICE SNAPSHOT
C
        WRITE(CLIN1,901)
        WRITE(CLIN2,902) (ASFBYT(J),J=SNAME,ENAME),
     *                    AGTTAB(AGTNUM,AGT)
        WRITE(CLIN3,903) (ASFBYT(J),J=SSTRT,ESTRT),AGT
C
        WRITE(CLIN4,904)ASFIVWEEK,ASFIVYEAR,ASFIVAGT
C
        CHR_FIRM='*'
        CHR_SHOP='*'
        IF (ASFIV_F_SGN.NE.0) CHR_FIRM=CHAR(ASFIV_F_SGN)
        IF (ASFIV_S_SGN.NE.0) CHR_SHOP=CHAR(ASFIV_S_SGN)
        WRITE(CLIN5,906)CHR_FIRM, CHR_SHOP
        WRITE(CLIN6,907)CSMONY(ASFIV_F_INVAMT,13,BETUNIT), 
     *                  CSMONY(ASFIV_S_INVAMT,13,BETUNIT)
        WRITE(CLIN7,908)CSMONY(ASFIV_F_TRAMT,13,BETUNIT)

        WRITE(CLIN8 ,909)ASFIV_F_CNT_ON_SAL,  ASFIV_S_CNT_ON_SAL
        WRITE(CLIN9,910)CSMONY(ASFIV_F_AMT_ON_SAL,13,BETUNIT),
     *                   CSMONY(ASFIV_S_AMT_ON_SAL,13,BETUNIT)
        WRITE(CLIN10,911)ASFIV_F_CNT_OF_SAL,  ASFIV_S_CNT_OF_SAL
        WRITE(CLIN11,912)CSMONY(ASFIV_F_AMT_OF_SAL,13,BETUNIT),  
     *                   CSMONY(ASFIV_S_AMT_OF_SAL,13,BETUNIT)
        WRITE(CLIN12,913)ASFIV_F_CNT_RET_D,   ASFIV_S_CNT_RET_D
        WRITE(CLIN13,914)CSMONY(ASFIV_F_AMT_RET,13,BETUNIT),  
     *                   CSMONY(ASFIV_S_AMT_RET,13,BETUNIT)
        WRITE(CLIN14,915)CSMONY(ASFIV_F_ON_COM,13,BETUNIT),
     *                   CSMONY(ASFIV_S_ON_COM,13,BETUNIT)
        WRITE(CLIN15,916)CSMONY(ASFIV_F_OF_COM,13,BETUNIT), 
     *                   CSMONY(ASFIV_S_OF_COM,13,BETUNIT)
        WRITE(CLIN16,917)CSMONY(ASFIV_F_LST_BAL,13,BETUNIT), 
     *                   CSMONY(ASFIV_S_LST_BAL,13,BETUNIT)
        WRITE(CLIN17,918)CSMONY(ASFIV_F_PAID,13,VALUNIT),
     *                   CSMONY(ASFIV_S_PAID,13,VALUNIT)
        WRITE(CLIN18,919)ASFIV_F_ACC_PAID,    ASFIV_S_ACC_PAID
        WRITE(CLIN19,920)CSMONY(ASFIV_F_AMT_PAID,13,VALUNIT),  
     *                   CSMONY(ASFIV_S_AMT_PAID,13,VALUNIT)
        WRITE(CLIN20,921)CSMONY(ASFIV_F_AMT_DIF,13,BETUNIT),  
     *                   CSMONY(ASFIV_S_AMT_DIF,13,BETUNIT)
        WRITE(CLIN21,922)ASFIV_F_CNT_RET_A,   ASFIV_S_CNT_RET_A
        WRITE(CLIN22,923)ASFIV_F_CODE, ASFIV_S_CODE
C
901     FORMAT('Agent passive invoice snapshot ')
902     FORMAT('Name',T10,<LNAME>A1,T66,'Agtnum',T73,I7.7)
903     FORMAT('Address',T10,<LSTRT>A1,T66,'Terminal',T75,I5.5)
C
904     FORMAT('Week ',I5,' / Year ',I5,' / Terminal# ',I5,T49,'FIRM         SHOP')

906     FORMAT('Sign of Accounting ',          T40,12X,A1,12X,A1)
907     FORMAT('Invoice Amount ',              T40,A13,A13)
908     FORMAT('Transit Amount',               T40,A13)
909     FORMAT('Counter of Online sales ',     T40,I13,I13)
910     FORMAT('Amount of Online sales ',      T40,A13,A13)
911     FORMAT('Counter of Offline sales ',    T40,I13,I13)
912     FORMAT('Amount of Offline sales ',     T40,A13,A13)
913     FORMAT('Counter of Returns ',          T40,I13,I13)
914     FORMAT('Amount of Returns ',           T40,A13,A13)
915     FORMAT('Online Remuneration ',         T40,A13,A13)
916     FORMAT('Offline Remuneration ',        T40,A13,A13)
917     FORMAT('Last week balance ',           T40,A13,A13)
918     FORMAT('Already paid ',                T40,A13,A13)
919     FORMAT('Account of paid prizes ',      T40,I13,I13)
920     FORMAT('Amount of paid prizes ',       T40,A13,A13)
921     FORMAT('Amount of differences ',       T40,A13,A13)
922     FORMAT('Counter of returns ',          T40,I13,I13)     
923     FORMAT('Code ',                        T40,I13,I13)     
C
925     FORMAT(5A4,' open error> ',I4)
1023    FORMAT(5A4,' read error> ',I4,' record> ',I4)
C
	RETURN
	END
