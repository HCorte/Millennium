C GXSRC:CNVSCF.FOR
C
C V05 17-MAR-2016 SCML M16 PROJECT: MAXTYP FROM 18 TO 21
C V04 23-MAR-2011 RXK Added DIR command before and after conversion
C V03 21-FEB-2011 FRP MAXGAM from 10 to 50
C V02 12-MAY-1999 UXN IMPROVED USERINTERFACE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        PROGRAM CNVSCF
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:RECSCF_OLD.DEF'
C
C
        INTEGER*4   ST, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO, I, J
        INTEGER*4   MAXREC_CNV                                                  !# OF RECORDS TO CONVERT
C
        INTEGER*4   FILE_SIZE                                                   !NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE
C
        CALL COPYRITE
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR SCF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'FILE:SCF.NEW'
        OLD_FILE = 'FILE:SCF.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        MAXREC_CNV = 1
        FILE_SIZE  = MAXREC_CNV*SCFSEC/2
        TYPE*,IAM(),'Old file size = ', MAXREC_CNV*OSCFSEC/2
        TYPE*,IAM(),'New file size = ', MAXREC_CNV*SCFSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert SCF file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CRTFIL(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,OSCFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
        ST  = 0
C
C       READ RECORD FROM FILE OLD FILE
C
        CALL READW(OLD_FDB,1,OSCFREC,ST)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,1)
C
        CALL FASTSET(0,SCFREC,SCFLEN)
C
        CALL FASTMOV(OSCFPAR,SCFPAR,NUMPAR)
C
        DO I=1,OMAXTYP
          DO J=1,MAXIND
            SCFGTN(I,J) = OSCFGTN(I,J)                                          !COPY GAMES FROM OLD GTN TO NEW GTN
          ENDDO
        ENDDO
C
        CALL FASTMOV(OSCFGNT,SCFGNT,2*MAXGAM)
        CALL FASTMOV(OSCFKGN,SCFKGN,MAXGAM)
        CALL FASTMOV(OSCFLGN,SCFLGN,4*MAXGAM)
        CALL FASTMOV(OSCFSGN,SCFSGN,MAXGAM)
        CALL FASTMOV(OSCFGFN,SCFGFN,5*MAXGAM)
        CALL FASTMOV(OSCFSFN,SCFSFN,5*MAXFIL)
        CALL FASTMOV(OSCFFSZ,SCFFSZ,MAXFIL)
        CALL FASTMOV(OSCFGSZ,SCFGSZ,MAXGAM)
        CALL FASTMOV(OSCFCOG,SCFCOG,MAXGAM)
        CALL FASTMOV(OSCFCOT,SCFCOT,NUMFIN)
        CALL FASTMOV(OSCFTKC,SCFTKC,MAXGAM)
        CALL FASTMOV(OSCFPRG,SCFPRG,MAXGAM)
        CALL FASTMOV(OSCFRED,SCFRED,MAXGAM)
        CALL FASTMOV(OSCFDLNAM,SCFDLNAM,MAXLOADS)
        CALL FASTMOV(OSCFDLTAB,SCFDLTAB,5*MAXLOADS/2)                           !<<<  I*2
        CALL FASTMOV(OSCFSTP,SCFSTP,MAXIND)
        CALL FASTMOV(OSCFGVN,SCFGVN,5*MAXGAM)
        CALL FASTMOV(OSCFGVS,SCFGVS,MAXGAM)
        SCFHVL = OSCFHVL
        SCFHVR = OSCFHVR
        SCFTAL = OSCFTAL
        SCFTAR = OSCFTAR
        CALL FASTMOV(OSCFRMI,SCFRMI,MAXGAM)
        SCFCTX = OSCFCTX
        CALL FASTMOV(OSCFFRC,SCFFRC,MAXGAM)
        SCFIVC = OSCFIVC
        CALL FASTMOV(OSCF_OPNID,SCF_OPNID,PRM_NUMOPN)
        CALL FASTMOV(OSCF_OPNSEED,SCF_OPNSEED,PRM_NUMOPN)
        CALL FASTMOV(OSCF_OPNDATE,SCF_OPNDATE,PRM_ENDDAT*PRM_NUMOPN)
        CALL FASTMOV(OSCF_HLDLIM,SCF_HLDLIM,3*MAXGAM)
        CALL FASTMOV(OSCF_HLDDAY,SCF_HLDDAY,3*MAXGAM)
        CALL FASTMOV(OSCF_TCPPORTS,SCF_TCPPORTS,2*2)
        CALL FASTMOV(OSCF_TCPPREFIX,SCF_TCPPREFIX,2)
        CALL FASTMOV(OSCF_TCPSUFFIX,SCF_TCPSUFFIX,2*4)
        CALL FASTMOV(OSCF_ORDER,SCF_ORDER,MAXGAM)
        CALL FASTMOV(OSCFRETCOM,SCFRETCOM,NUMPAS)
C
C       WRITE NEW RECORD TO NEW FILE
C
        CALL WRITEW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,1)
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:SCF.FIL to FILE:SCF.OLD'
        CALL LIB$RENAME_FILE('FILE:SCF.FIL','FILE:SCF.OLD')
C
        TYPE*,IAM(),'Renaming FILE:SCF.NEW to FILE:SCF.FIL'
        CALL LIB$RENAME_FILE('FILE:SCF.NEW','FILE:SCF.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR SCF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'SCF file converted successfully!'
        CALL GSTOP(GEXIT_SUCCESS)
        END
C
C END GXSRC:CNVSCF.FOR
C
