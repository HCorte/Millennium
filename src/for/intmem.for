C
C SUBROUTINE INTMEM
C
C V15 12-MAR-2014 SCML Added support to PLACARD Project - IGS
C V14 22-JAN-2001 CS  ADDED PASSIVE GAME FOR PORTUGAL.
C V13 22-JAN-2000 UXN POOLLTO ADDED.
C V12 29-NOV-2000 UXN TGLCOM ADDED.
C V11 05-JUN-2000 OXK SSOCOM added.
C V10 17-NOV-1999 UXN WINCOM added.
C V09 13-OCT-1999 RXK WRLCOM added.
C V09 14-MAY-1999 UXN STRCOM, STROCOM added.
C V08 05-FEB-1998 UXN SSCCOM,SSPCOM,TRPCOM,TROCOM added.
C V07 13-NOV-1997 UXN Initialization of STOPCOM added."
C V06 10-NOV-1995 HXK Further changes for Double, Couple
C V05 17-JUL-1995 HXK Zap Bingo common
C V04 24-APR-1995 HXK Merge of V5 development with March 10th 1995 bible
C V03 22-FEB-1995 HXK yabba dabba doo
C V02 16-JUN-1993 SXH Added PPPCOM, V65COM, S234COm and SCNCOM
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
C
C SUBROUTINE TO CLEAR SYSTEM COMMONS
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE INTMEM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:SLOCOM.DEF'
        INCLUDE 'INCLIB:POLCOM.DEF'
        INCLUDE 'INCLIB:LOGCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:CTLCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:SSPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:SSOCOM.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
        INTEGER*4  ST               !
        INTEGER*4  I                !
        INTEGER*4  SIZE
C
        TYPE*,IAM(),' Clearing game commons'
        CALL FASTSET(0, PRO, PROLEN*NUMPRO)
C
        FRETST = 0
        CALL FASTSET(0, FREEQ,  NUMPRO+QHEDSZ)
        INPTST = 0
        CALL FASTSET(0, INQUE,  NUMPRO+QHEDSZ)
C
        CALL FASTSET(0, PRFQUE, STALEN+QHEDSZ)
C
        CALL FASTSET(0, QUETAB, (NUMPRO+QHEDSZ)*NUMAPPQUE)
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        CALL FASTSET(0, COMIGSQUE, (NUMPRO+QHEDSZ))
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
C CLEAR GAME COMMONS
C
        CALL FASTSET(0,CONBLK,CONCSB*2048)
        CALL FASTSET(0,AGTBLK,AGTCSB*2048)
        CALL FASTSET(0,LTOBLK,LTOCSB*2048)
        CALL FASTSET(0,SPTBLK,SPTCSB*2048)
        CALL FASTSET(0,TGLBLK,TGLCSB*2048)
        CALL FASTSET(0,PASBLK,PASCSB*2048)
        CALL FASTSET(0,NBRBLK,NBRCSB*2048)
        CALL FASTSET(0,WITBLK,WITCSB*2048)
        CALL FASTSET(0,SCRBLK,SCRCSB*2048)
        CALL FASTSET(0,TSLBLK,TSLCSB*2048)
        CALL FASTSET(0,KIKBLK,KIKCSB*2048)
        CALL FASTSET(0,SLOBLK,SLOCSB*2048)
        CALL FASTSET(0,LOGBLK,LOGCSB*2048)
        CALL FASTSET(0,POLBLK,POLCSB*2048)
        CALL FASTSET(0,BNGBLK,BNGCSB*2048)
        CALL FASTSET(0,DBLBLK,DBLCSB*2048)
        CALL FASTSET(0,CPLBLK,CPLCSB*2048)
        CALL FASTSET(0,STOPCOM_BLK,STOPCOM_CSB*2048)
        CALL FASTSET(0,SSCBLK,SSCCSB*2048)
        CALL FASTSET(0,TRPBLK,TRPCSB*2048)
        CALL FASTSET(0,SSPBLK,SSPCSB*2048)
        CALL FASTSET(0,TROBLK,TROCSB*2048)
        CALL FASTSET(0,STRBLK,STRCSB*2048)
        CALL FASTSET(0,STROBLK,STROCSB*2048)
        CALL FASTSET(0,SSOCOMBLK,SSOCOMCSB*2048)
C
C CLEAR GSALES AUTO 
C
        CALL FASTSET(0, GSALES_AUTO, MAXGAM)
C
C CLEAR WINCOM
C
	SIZE = %LOC(LAST_WINCOM)-%LOC(FRST_WINCOM(1))+4
        CALL FASTSET(0,FRST_WINCOM,SIZE/4)
C
C CLEAR POOLLTO
C
	SIZE = %LOC(LAST_POOLLTO)-%LOC(FRST_POOLLTO(1))+4
        CALL FASTSET(0,FRST_POOLLTO,SIZE/4)
C
C INITIALIZE LOGGER BUFFERS
C
        DO I = 1, NUMLOG
            LOGBUF(BSTATE,I) = LGUSD
            LOGBUF(BLONUM,I) = -1
        END DO
C
C DEFINE SYSTEM QUEUES
C
        CALL DEFLST(INQUE,NUMPRO)
        CALL DEFLST(FREEQ,NUMPRO)
        CALL DEFLST(PRFQUE,STALEN)
        CALL DEFLST(REPVQUE,REPVLEN)
C
C DEFINE PASSIVE REPROCESSING QUEUE'S
C
        CALL DEFLST(REPQUEPAS(1,RQPASPRO),REPVLEN)    !RETURN TICKETS
        CALL DEFLST(REPQUEPAS(1,RQPASVAL),REPVLEN)    !VALIDATIONS
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        CALL DEFLST(COMIGSQUE(1),NUMPRO)    !IGS
C----+------------------------------------------------------------------
C V15| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
        DO I =1, STALEN
            CALL ABL(I,PRFQUE,ST)
        END DO

        DO I = 1, NUMPRO
          CALL RELBUF(I)
        END DO

        DO I =1, NUMAPPQUE
          CALL DEFLST(QUETAB(1,I),NUMPRO)
        END DO
C
        CTLX2XLOC = CTLX2XOK
        LANGO     = 0
        THISSTA   = 0
C
        RETURN
        END
