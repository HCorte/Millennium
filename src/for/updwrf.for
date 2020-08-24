C SUBROUTINE UPDWRF.FOR
C  
C V06 01-DEC-2000 UXN TOTOGOLO ADDED.
C V05 11-APR-2000 UXN WINUPD.DEF added
C V04 15-FEB-2000 UXN BIGWRL added
C V03 13-OCT-1999 RXK World Tour added.
C V02 13-SEP-1993 HXK Fixed IOINIT record length bug
C V01 10-SEP-1993 HXK Initial Release for Finland VAX conversion 
C
C
C SUBROUTINE TO POST FINAL STATUS TO GAME FILES
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE UPDWRF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:RECRDF.DEF'
	INCLUDE 'INCLIB:WINUPD.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

        INTEGER*4 FDB(7)
        INTEGER*4 ST,I,GNUM
C
C
        CALL OPENW(3,SCFSFN(1,RDF),4,0,0,ST)
        CALL IOINIT(FDB,3,RDFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,RDF),1,ST,0)
        CALL READW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,RDF),2,ST,0)

        DO 100 I=1,NUMLTO
        IF(LLTDRW(I).LT.1.OR.LLTSTS(I).NE.GFINAL) GOTO 100
        IF(LLTWRF(POSTED,I).NE.0) GOTO 100 !ALREADY POSTED
        GNUM=SCFGTN(TLTO,I)
        IF(RDF_WRFCUD(GNUM).EQ.LLTDRW(I)) THEN
          RDF_WRFTAB(PENAMT,GNUM)=RDF_WRFTAB(PENAMT,GNUM)+LLTRES(1,I)
          RDF_WRFTAB(USEAMT,GNUM)=RDF_WRFTAB(USEAMT,GNUM)+LLTRES(2,I)
        ELSE
          RDF_WRFTAB(RESAMT,GNUM)=RDF_WRFTAB(RESAMT,GNUM)+LLTRES(1,I)-
     *                            LLTRES(2,I)
        ENDIF
        LLTWRF(POSTED,I)=1
100     CONTINUE


        DO 200 I=1,NUMSPT
        IF(LSPDRW(I).LT.1.OR.LSPSTS(I).NE.GFINAL) GOTO 200
        IF(LSPWRF(POSTED,I).NE.0) GOTO 200 !ALREADY POSTED
        GNUM=SCFGTN(TSPT,I)
        IF(RDF_WRFCUD(GNUM).EQ.LSPDRW(I)) THEN
          RDF_WRFTAB(PENAMT,GNUM)=RDF_WRFTAB(PENAMT,GNUM)+LSPRES(1,I)
          RDF_WRFTAB(USEAMT,GNUM)=RDF_WRFTAB(USEAMT,GNUM)+LSPRES(2,I)
        ELSE
          RDF_WRFTAB(RESAMT,GNUM)=RDF_WRFTAB(RESAMT,GNUM)+LSPRES(1,I)-
     *                            LSPRES(2,I)
        ENDIF
        LSPWRF(POSTED,I)=1
200     CONTINUE

        DO 300 I=1,NUMTGL
        IF(LTGDRW(I).LT.1.OR.LTGSTS(I).NE.GFINAL) GOTO 300
        IF(LTGWRF(POSTED,I).NE.0) GOTO 300 !ALREADY POSTED
        GNUM=SCFGTN(TTGL,I)
        IF(RDF_WRFCUD(GNUM).EQ.LTGDRW(I)) THEN
          RDF_WRFTAB(PENAMT,GNUM)=RDF_WRFTAB(PENAMT,GNUM)+LTGRES(1,I)
          RDF_WRFTAB(USEAMT,GNUM)=RDF_WRFTAB(USEAMT,GNUM)+LTGRES(2,I)
        ELSE
          RDF_WRFTAB(RESAMT,GNUM)=RDF_WRFTAB(RESAMT,GNUM)+LTGRES(1,I)-
     *                            LTGRES(2,I)
        ENDIF
        LTGWRF(POSTED,I)=1
300     CONTINUE




        DO 400 I=1,NUMKIK
        IF(LKKDRW(I).LT.1.OR.LKKSTS(I).NE.GFINAL) GOTO 400
        IF(LKKWRF(POSTED,I).NE.0) GOTO 400 !ALREADY POSTED
        GNUM=SCFGTN(TKIK,I)
        IF(RDF_WRFCUD(GNUM).EQ.LKKDRW(I)) THEN
          RDF_WRFTAB(PENAMT,GNUM)=RDF_WRFTAB(PENAMT,GNUM)+LKKRES(1,I)
          RDF_WRFTAB(USEAMT,GNUM)=RDF_WRFTAB(USEAMT,GNUM)+LKKRES(2,I)
        ELSE
          RDF_WRFTAB(RESAMT,GNUM)=RDF_WRFTAB(RESAMT,GNUM)+LKKRES(1,I)-
     *                        LKKRES(2,I)
        ENDIF
        LKKWRF(POSTED,I)=1
400     CONTINUE

        CALL WRITEW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,RDF),3,ST,1)
        CLOSE(UNIT=3)
        RETURN
        END
