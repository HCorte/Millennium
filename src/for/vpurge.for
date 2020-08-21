C PROGRAM VPURGE
C 
C
C VPURGE.FOR
C
C V20 05-FEB-2001 EPH Update with what shoul be paid in cash only (not in OPs)
C V19 02-DEC-2000 UXN TOTOGOLO ADDED.
C V18 01-MAR-2000 UXN Fix for World Tour: SFLAGS replaced with MFLAGS
C V17 13-OCT-1999 RXK World Tour added.
C V16 28-MAY-1999 UXN Display game names instead of game numbers.
C V15 17-MAY-1999 UXN SUPER TRIPLE ADDED.
C V14 09-FEB-1998 UXN SUPER SCORE AND TODAYS TRIPLE ADDED.  
C V13 23-NOV-1995 HXK Merge of post 65 stuff; changes for Double/Couple
C V12 30-OCT-1995 RXK For oddset games added check of drawing date
C V11 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V10 21-OCT-1993 HXK FIX FOR GAMES WITH INDEX GREATER THAN 1.
C V09 18-OCT-1993 HXK CHANGED MAXDRW TO MDRAWS FOR RAVI.
C V08 28-SEP-1993 GXA Take refunds into account when purging.
C V07 10-SEP-1993 HXK Added Win Reserve function
C V06 06-SEP-1993 SXH Use CPDAY (=7) instead of P(CSHDAY)
C V05 03-SEP-1993 SXH Aded IAM() in RAVI message
C V04 18-AUG-1993 SXH Released for Finland (enabled partial purging)
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1999 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM VPURGE
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'

	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DNBREC.DEF'
        INCLUDE 'INCLIB:DSCREC.DEF'
        INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
        INCLUDE 'INCLIB:DCPREC.DEF'
        INCLUDE 'INCLIB:DSSREC.DEF'
        INCLUDE 'INCLIB:DTRREC.DEF'
        INCLUDE 'INCLIB:DSTREC.DEF'

	INCLUDE 'INCLIB:PRGREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'

        INCLUDE 'INCLIB:RECRDF.DEF'
C
        ! parameters
	INTEGER*4  TUBSIZ                               !
	PARAMETER (TUBSIZ=I4BUCSIZ*2)

	INTEGER*4  MDRAWS                               !
	PARAMETER (MDRAWS=400)
	INTEGER*4  MAXPRG
	PARAMETER (MAXPRG=200)

        INTEGER*4  BNKDAY
        PARAMETER (BNKDAY=7)                            ! retain 7 days

        INTEGER*4  CPDAY
        PARAMETER (CPDAY=7)                             ! retain 7 days
C
        ! variables
	INTEGER*4  VLFBUF(TUBSIZ)                       !
	INTEGER*4  NEWBUF(TUBSIZ)                       !
	INTEGER*4  CFDB(7)                              !
	INTEGER*4  UFDB(7)                              !
	INTEGER*4  SVALREC(VALLEN)                      !
	INTEGER*4  PVALREC(VALLEN)                      !
	INTEGER*4  PDRAWS(MAXGAM)                       !
	INTEGER*4  COUNT(4)                             !
	INTEGER*4  FDB(7)                               !
	INTEGER*4  GFDB(7)                               !
	INTEGER*4  INDLEN(4)                            !

	INTEGER*4  LPRG(LTGDIV,2,MDRAWS,NUMLTO)         !
	INTEGER*4  SPRG(SPGDIV,MDRAWS,NUMSPT)           !
	INTEGER*4  TGPRG(TGGDIV,MDRAWS,NUMTGL)           !
	INTEGER*4  KPRG(KIGDIV,MDRAWS,NUMKIK)           !
        INTEGER*4  RPRG(MDRAWS,NUMSCR)                  !
        INTEGER*4  WPRG(MDRAWS,NUMWIT)                  !
        INTEGER*4  TPRG(MDRAWS,NUMTSL)                  !
	INTEGER*4  NPRG(NUMTOT,NBGPOL,2,MDRAWS,NUMNBR)  !
        INTEGER*4  BPRG(BGODIV,BGOSUB,MDRAWS,NUMBGO)    !
        INTEGER*4  DPRG(MDRAWS,NUMDBL)                  !
        INTEGER*4  CPRG(MDRAWS,NUMCPL)                  !
        INTEGER*4  SSC_PRG(MDRAWS,NUMSSC)                  !
        INTEGER*4  TRP_PRG(MDRAWS,NUMTRP)                  !
        INTEGER*4  STR_PRG(MDRAWS,NUMSTR)                  !

	INTEGER*4  LFLAGS(MDRAWS,NUMLTO)                !
	INTEGER*4  SFLAGS(MDRAWS,NUMSPT)                !
	INTEGER*4  TGFLAGS(MDRAWS,NUMTGL)                !
	INTEGER*4  NFLAGS(MDRAWS,NUMNBR)                !
        INTEGER*4  KFLAGS(MDRAWS,NUMKIK)                !
        INTEGER*4  RFLAGS(MDRAWS,NUMSCR)                !
        INTEGER*4  WFLAGS(MDRAWS,NUMWIT)                !
	INTEGER*4  TFLAGS(MDRAWS,NUMTSL)                !
        INTEGER*4  BFLAGS(MDRAWS,NUMBGO)                !
        INTEGER*4  DFLAGS(MDRAWS,NUMDBL)                !
        INTEGER*4  CFLAGS(MDRAWS,NUMCPL)                !
        INTEGER*4  SSC_FLAGS(MDRAWS,NUMSSC)                !
        INTEGER*4  TRP_FLAGS(MDRAWS,NUMTRP)                !
        INTEGER*4  STR_FLAGS(MDRAWS,NUMSTR)                !

	INTEGER*4  MAXIDRW(MAXGAM)
        INTEGER*4  PURTAB(MAXGAM,MAXPRG)

	INTEGER*4  CBLK                   !
	INTEGER*4  UBLK                   !
	INTEGER*4  CIND                   !
	INTEGER*4  UIND                   !
	INTEGER*4  CDC                    !
	INTEGER*4  I                      !
	INTEGER*4  PCDC                   !
	INTEGER*4  BNS                    !
	INTEGER*4  ST                     !
	INTEGER*4  GAM                    !
	INTEGER*4  GIND                   !
	INTEGER*4  GTYP                   !
	INTEGER*4  KGAM                   !
        INTEGER*4  KIND                   !
	INTEGER*4  DRWIND                 !
	INTEGER*4  DIV                    !
	INTEGER*4  SHR                    !
	INTEGER*4  GNUM                   !
	INTEGER*4  K                      !
	INTEGER*4  DRAW                   !
	INTEGER*4  TEMP                   !
	INTEGER*4  NUMREC                 !
	INTEGER*4  MASK                   !
        INTEGER*4  PRGAMT(MAXGAM)         ! for WRF
        INTEGER*4  SUBGAME                !
        INTEGER*4  GDRAW                  ! GAME DRAW

	LOGICAL  KIKWIN                   !
	LOGICAL  PURGE                    !
	LOGICAL  SAVE                     !
	LOGICAL  PARTIAL                  !

        DATA INDLEN/Z00000000,Z40000000,Z80000000,ZC0000000/
        DATA MASK/ZC0000000/
	DATA COUNT/4*0/

	EQUIVALENCE(DLTREC,DSPREC,DKKREC,DNBREC,DBNREC)
	EQUIVALENCE(DLTREC,DSCREC,DWIREC,DTSREC)
        EQUIVALENCE(DLTREC,DDBREC,DCPREC)

        CALL FASTSET(0,PRGAMT,MAXGAM)                                          
	CALL FASTSET(0,KPRG,KIGDIV*MDRAWS*NUMKIK)
	CALL FASTSET(0,LPRG,LTGDIV*MDRAWS*NUMLTO*2)
	CALL FASTSET(0,SPRG,SPGDIV*MDRAWS*NUMSPT)
	CALL FASTSET(0,TGPRG,TGGDIV*MDRAWS*NUMTGL)
	CALL FASTSET(0,NPRG,NUMTOT*NBGPOL*MDRAWS*NUMNBR*2)
	CALL FASTSET(0,RPRG,MDRAWS*NUMSCR)
        CALL FASTSET(0,WPRG,MDRAWS*NUMWIT)
        CALL FASTSET(0,TPRG,MDRAWS*NUMTSL)
        CALL FASTSET(0,BPRG,BGODIV*BGOSUB*MDRAWS*NUMBGO)
        CALL FASTSET(0,DPRG,MDRAWS*NUMDBL)
        CALL FASTSET(0,CPRG,MDRAWS*NUMCPL)
        CALL FASTSET(0,SSC_PRG,MDRAWS*NUMSSC)
        CALL FASTSET(0,TRP_PRG,MDRAWS*NUMTRP)
        CALL FASTSET(0,STR_PRG,MDRAWS*NUMSTR)

	CALL FASTSET(0,LFLAGS,MDRAWS*NUMLTO)
	CALL FASTSET(0,NFLAGS,MDRAWS*NUMNBR)
	CALL FASTSET(0,SFLAGS,MDRAWS*NUMSPT)
	CALL FASTSET(0,TGFLAGS,MDRAWS*NUMTGL)
        CALL FASTSET(0,KFLAGS,MDRAWS*NUMKIK)
        CALL FASTSET(0,RFLAGS,MDRAWS*NUMSCR)
        CALL FASTSET(0,WFLAGS,MDRAWS*NUMWIT)
	CALL FASTSET(0,TFLAGS,MDRAWS*NUMTSL)
        CALL FASTSET(0,BFLAGS,MDRAWS*NUMBGO)
        CALL FASTSET(0,DFLAGS,MDRAWS*NUMDBL)
        CALL FASTSET(0,CFLAGS,MDRAWS*NUMCPL)
        CALL FASTSET(0,SSC_FLAGS,MDRAWS*NUMSSC)
        CALL FASTSET(0,TRP_FLAGS,MDRAWS*NUMTRP)
        CALL FASTSET(0,STR_FLAGS,MDRAWS*NUMSTR)

	PARTIAL = .TRUE.   ! FOR FINLAND

	CBLK = 1
	UBLK = 1
	CIND = 1
	UIND = 1
	CDC=DAYCDC
	IF(DAYSTS.NE.DSCLOS) THEN
	    TYPE*,IAM(),'Invalid daysts > ',DAYSTS
	    CALL GPAUSE
	ENDIF

C GET DRAW NUMBERS TO PURGE FROM DAF FILE
C
	IF(PARTIAL) THEN
	    CALL FASTSET(-1,PDRAWS,MAXGAM)

	    CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	    CALL IOINIT(FDB,1,DAFSEC*256)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

	    DO 10 I=1,MAXGAM
	        IF(DAYHDR(I).LT.1) GOTO 10
	        IF(PRGDAY(I).LT.1) GOTO 10
                IF(GNTTAB(GAMTYP,I) .EQ. TPAS) GOTO 10 ! NOT PURGE FOR PASSIVE
C
	        PCDC=DAYCDC-PRGDAY(I)
	        IF(PCDC.GT.0) THEN
	            CALL READW(FDB,PCDC,DAFREC,ST)
	            IF(ST.NE.0) THEN
	                CALL FILERR(SFNAMES(1,DAF),2,ST,PCDC)
	            ELSE
	                PDRAWS(I)=DAFHDR(I)
                        GTYP=GNTTAB(GAMTYP,I)
C
                        IF(GTYP.EQ.TLTO) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DLTSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           GDRAW = DAFHDR(I) - 1
                           IF(GDRAW .LT. 1) GDRAW = 1
                           CALL READW(GFDB,GDRAW,DLTREC,ST)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,GDRAW)
                           IF(DLTDAT(1).GT.PCDC) PDRAWS(I) = PDRAWS(I) - 1
                           IF(DLTDAT(1).LE.PCDC.AND.DAFDRW(I).LE.0)
     *                        PDRAWS(I)=DAFHDR(I)+1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TSPT) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DSPSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           GDRAW = DAFHDR(I) - 1
                           IF(GDRAW .LT. 1) GDRAW = 1
                           CALL READW(GFDB,GDRAW,DSPREC,ST)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,GDRAW)
                           IF(DSPDAT(1).GT.PCDC) PDRAWS(I) = PDRAWS(I) - 1
                           IF(DSPDAT(1).LE.PCDC.AND.DAFDRW(I).LE.0)
     *                        PDRAWS(I)=DAFHDR(I)+1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TTGL) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DTGSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           GDRAW = DAFHDR(I) - 1
                           IF(GDRAW .LT. 1) GDRAW = 1
                           CALL READW(GFDB,GDRAW,DTGREC,ST)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,GDRAW)
                           IF(DTGDAT(1).GT.PCDC) PDRAWS(I) = PDRAWS(I) - 1
                           IF(DTGDAT(1).LE.PCDC.AND.DAFDRW(I).LE.0)
     *                        PDRAWS(I)=DAFHDR(I)+1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TKIK) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DKKSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           GDRAW = DAFHDR(I) - 1
                           IF(GDRAW .LT. 1) GDRAW = 1
                           CALL READW(GFDB,GDRAW,DKKREC,ST)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,GDRAW)
                           IF(DKKDAT(1).GT.PCDC) PDRAWS(I) = PDRAWS(I) - 1
                           IF(DKKDAT(1).LE.PCDC.AND.DAFDRW(I).LE.0)
     *                        PDRAWS(I)=DAFHDR(I)+1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TSCR) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DSCSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DSCREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DSCDAT.NE.0 .AND. DSCDAT.LE.PCDC .AND.
     *                             DSCPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I, MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 6
                                 ENDIF
                              ENDIF
                           ENDDO
6                          CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TWIT) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DWISEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DWIREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DWIDAT.NE.0 .AND. DWIDAT.LE.PCDC .AND.
     *                             DWIPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 7
                                 ENDIF
                              ENDIF
                           ENDDO
7                          CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF

                        IF(GTYP.EQ.TDBL) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DDBSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DDBREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DDBDAT.NE.0 .AND. DDBDAT.LE.PCDC .AND.
     *                             DDBPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 8
                                 ENDIF
                              ENDIF
                           ENDDO
8                          CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF

                        IF(GTYP.EQ.TCPL) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DCPSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DCPREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DCPDAT.NE.0 .AND. DCPDAT.LE.PCDC .AND.
     *                             DCPPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 9
                                 ENDIF
                              ENDIF
                           ENDDO
9                          CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF

                        IF(GTYP.EQ.TTRP) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DTRSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DTRREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DTRDAT.NE.0 .AND. DTRDAT.LE.PCDC .AND.
     *                             DTRPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 11
                                 ENDIF
                              ENDIF
                           ENDDO
11                         CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TSSC) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DSSSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DSSREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DSSDAT.NE.0 .AND. DSSDAT.LE.PCDC .AND.
     *                             DSSPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 12
                                 ENDIF
                              ENDIF
                           ENDDO
12                         CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF


                        IF(GTYP.EQ.TSTR) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DSTSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           K=0
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DSTREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DSTDAT.NE.0 .AND. DSTDAT.LE.PCDC .AND.
     *                             DSTPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                                 K=K+1
                                 PURTAB(I,K)=DRAW
                                 IF(K.EQ.MAXPRG) THEN
                                    TYPE*,IAM(),' GAME #',I,MAXPRG,
     *                                ' draws written into purge table'
                                    GOTO 13
                                 ENDIF
                              ENDIF
                           ENDDO
13                         CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
                        IF(GTYP.EQ.TTSL) THEN
                           CALL OPENW(3,GFNAMES(1,I),4,0,0,ST)
                           CALL IOINIT(GFDB,3,DTSSEC*256)
                           IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),1,ST,0)
                           DO DRAW=1,PDRAWS(I)
                              CALL READW(GFDB,DRAW,DTSREC,ST)
                              IF(ST.NE.0) CALL FILERR(GFNAMES(1,I),2,ST,DRAW)
                              IF(DTSDAT(1).NE.0 .AND. DTSDAT(1).LE.PCDC .AND.
     *                             DTSPUP.EQ.0) THEN
                                 MAXIDRW(I)=DRAW
                              ENDIF
                           ENDDO
                           CONTINUE
                           PDRAWS(I)=MAXIDRW(I)+1
                           IF(MAXIDRW(I).EQ.0) PDRAWS(I)=-1
                           CALL CLOSEFIL(GFDB)
                        ENDIF
C
C DRAW WRITTEN TO CONSOLE IS LAST DRAW THAT WILL BE IN VLF FILE
C
                        WRITE(6,904) IAM(),PDRAWS(I),(GLNAMES(K,I),K=1,4)
	            ENDIF
	        ENDIF
10	    CONTINUE
	    CALL CLOSEFIL(FDB)
	    CALL LPRIZE(PDRAWS)
	ENDIF
C
C OPEN VALIDATION FILE FOR SEQUENTIAL READ
C
	CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
	CALL ITUBSIZE(VLF,TUBSIZ)
C
C OPEN VALIDATION COPY FILE
C
	CALL IOPEN(SFNAMES(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),1,ST,0)
	CALL ITUBSIZE(VLC,TUBSIZ)
	CALL INOCHKS(VLC)
C
C OPEN PURGE FILES
C
	CALL OPENW(1,SFNAMES(1,CTP),4,0,0,ST)
	CALL IOINIT(CFDB,1,PRGSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,CTP),1,ST,0)
C
	CALL OPENW(2,SFNAMES(1,UTP),4,0,0,ST)
	CALL IOINIT(UFDB,2,PRGSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),1,ST,0)
C
100	CONTINUE
	CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	    CALL ICLOSE(VLF,VLFBUF,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),4,ST,0)

	    CALL ICLOSE(VLC,NEWBUF,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),4,ST,0)

	    CALL WRITEW(CFDB,CBLK,CPREC,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,CTP),3,ST,CBLK)
	    CALL CLOSEFIL(CFDB)
	    CALL WRITEW(UFDB,UBLK,UPREC,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),3,ST,UBLK)

	    CALL CLOSEFIL(UFDB)
	    GOTO 8000
	ENDIF
C
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,COUNT(1)+1)
	COUNT(1)=COUNT(1)+1
	CALL LOGVAL(VALREC,V4BUF)
C
C CHECK IF CASHED
C
	IF(VALREC(VSTAT).EQ.VCASH .OR. VALREC(VSTAT).EQ.VCASHX .OR.
     *     VALREC(VSTAT).EQ.VBANK) THEN
	    PURGE=.FALSE.

            ! KEEP BANK WINNERS IN VALIDATION FILE LONGER THAN REGULAR                      
            ! CASHED TICKETS.   (FOR PIF REASONS)                                           
            IF(VALREC(VSTAT).EQ.VBANK) THEN
                IF(VALREC(VCCDC).LT.DAYCDC-BNKDAY) GOTO 300                                 
                CALL FASTMOV(VALREC,SVALREC,VALLEN)                                   
                GOTO 200                                                              
            ENDIF 

	    IF(VALREC(VCCDC).GE.DAYCDC-CPDAY) THEN
	        CALL FASTMOV(VALREC,SVALREC,VALLEN)
	        GOTO 200
	    ENDIF
	    GOTO 300
	ENDIF
C
C TICKET IS UNCASHED
C
	CALL PRGSUB(PDRAWS,VALREC,SVALREC,PVALREC,PURGE,SAVE,PARTIAL,PURTAB)
	IF(SAVE) GOTO 200
	IF(PURGE) GOTO 400
	GOTO 100
C
C WRITE RECORD TO NEW VALIDATION FILE
C
200	CONTINUE
	COUNT(2)=COUNT(2)+1
	CALL VALLOG(SVALREC,V4BUF)
	CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLC),3,ST,VALREC(VSSER))
	IF(PURGE) GOTO 400
	GOTO 100
C
C WRITE RECORD TO CASHED TICKET FILE
C
300	CONTINUE
	COUNT(3)=COUNT(3)+1
	CALL VALLOG(VALREC,V4BUF)
        NUMREC=1

        TEMP=IAND(V4BUF(1),MASK)
        IF(TEMP.EQ.INDLEN(2)) NUMREC=2
        IF(TEMP.EQ.INDLEN(3)) NUMREC=3
        IF(TEMP.EQ.INDLEN(4)) NUMREC=4
	CALL FASTMOV(V4BUF,CPBUF(1,CIND),VFLEN*NUMREC)
	CIND=CIND+NUMREC
        IF(CIND.GT.PRGBLC) THEN
            CALL WRITEW(CFDB,CBLK,CPREC,ST)
            IF(ST.NE.0) CALL FILERR(SFNAMES(1,CTP),3,ST,CBLK)
            CIND=1
            CBLK=CBLK+1
            CALL FASTSET(0,CPREC,PRGRECLEN)
        ENDIF
	GOTO 100
C
C WRITE RECORD TO UNCASHED TICKET FILE
C
400	CONTINUE
	GAM=PVALREC(VGAM)
	GIND=PVALREC(VGIND)
	GTYP=PVALREC(VGTYP)
	KGAM=PVALREC(VKGME)
        IF(KGAM.GE.1.AND.KGAM.LE.MAXGAM) KIND=GNTTAB(GAMIDX,KGAM)

        ! update PRGAMT for WRF
        IF(GAM.NE.0) PRGAMT(GAM)=PRGAMT(GAM)+PVALREC(VPAMT)+PVALREC(VRAMT)
        IF(KGAM.NE.0)PRGAMT(KGAM)=PRGAMT(KGAM)+PVALREC(VKPAMT)             

        CALL VALLOG(PVALREC,V4BUF)
	COUNT(4)=COUNT(4)+1

        NUMREC=1
        TEMP=IAND(V4BUF(1),MASK)
        IF(TEMP.EQ.INDLEN(2)) NUMREC=2
        IF(TEMP.EQ.INDLEN(3)) NUMREC=3
        IF(TEMP.EQ.INDLEN(4)) NUMREC=4
        CALL FASTMOV(V4BUF,UPBUF(1,UIND),VFLEN*NUMREC)
        UIND=UIND+NUMREC

        IF(UIND.GT.PRGBLC) THEN
            CALL WRITEW(UFDB,UBLK,UPREC,ST)
            IF(ST.NE.0) CALL FILERR(SFNAMES(1,UTP),3,ST,UBLK)
            UIND=1
            UBLK=UBLK+1
            CALL FASTSET(0,UPREC,PRGRECLEN)
        ENDIF

        CALL DLOGVAL(PVALREC,VDETAIL)
	IF(GTYP.EQ.TLTO) GOTO 1000
	IF(GTYP.EQ.TSPT) GOTO 2000
	IF(GTYP.EQ.TTGL) GOTO 2500
        IF(GTYP.EQ.TNBR) GOTO 3000
        IF(GTYP.EQ.TSCR) GOTO 4000
        IF(GTYP.EQ.TWIT) GOTO 5000
	IF(GTYP.EQ.TTSL) GOTO 6000
	IF(GTYP.EQ.TKIK) GOTO 7000
        IF(GTYP.EQ.TBNG) GOTO 7600
        IF(GTYP.EQ.TDBL) GOTO 7700
        IF(GTYP.EQ.TCPL) GOTO 7800
        IF(GTYP.EQ.TSSC) GOTO 7900
        IF(GTYP.EQ.TTRP) GOTO 7950
        IF(GTYP.EQ.TSTR) GOTO 7970

	GOTO 100
C
C UPDATE LOTTO GAME PURGE DATA
C
1000	CONTINUE
	DO 1010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 1010             !V20

	    IF(VDETAIL(VKIK,I).NE.0 .OR.VDETAIL(VKI2,I).NE.0) THEN
	        KIKWIN=.TRUE.
	        GOTO 1010
	    ENDIF

	    DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
	    IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
	        WRITE(5,901) IAM(),GTNAMES(TLTO),GIND,DRWIND
	        CALL GPAUSE
	    ENDIF

	    DIV=VDETAIL(VDIV,I)
	    SHR=VDETAIL(VSHR,I)
	    BNS=VDETAIL(VBDR,I)+1
	    LPRG(DIV,BNS,DRWIND,GIND)=LPRG(DIV,BNS,DRWIND,GIND)+SHR
	    LFLAGS(DRWIND,GIND)=1
1010	CONTINUE
	IF(KIKWIN) GOTO 7000
	GOTO 100
C
C UPDATE SPORTS GAME PURGE DATA
C
2000	CONTINUE
	DO 2010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 2010             !V20

	    IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
	        KIKWIN=.TRUE.
	        GOTO 2010
	    ENDIF

	    DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
	    IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
	        WRITE(5,901) IAM(),GTNAMES(TSPT),GIND,DRWIND
	        CALL GPAUSE
	    ENDIF

	    DIV=VDETAIL(VDIV,I)
	    SHR=VDETAIL(VSHR,I)
	    SPRG(DIV,DRWIND,GIND)=SPRG(DIV,DRWIND,GIND)+SHR
	    SFLAGS(DRWIND,GIND)=1
2010	CONTINUE
	IF(KIKWIN) GOTO 7000
	GOTO 100
C
C UPDATE TOTOGOLO GAME PURGE DATA
C
2500	CONTINUE
	DO 2510 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 2510             !V20

	    IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
	        KIKWIN=.TRUE.
	        GOTO 2510
	    ENDIF

	    DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
	    IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
	        WRITE(5,901) IAM(),GTNAMES(TTGL),GIND,DRWIND
	        CALL GPAUSE
	    ENDIF

	    DIV=VDETAIL(VDIV,I)
	    SHR=VDETAIL(VSHR,I)
	    TGPRG(DIV,DRWIND,GIND)=TGPRG(DIV,DRWIND,GIND)+SHR
	    TGFLAGS(DRWIND,GIND)=1
2510	CONTINUE
	IF(KIKWIN) GOTO 7000
	GOTO 100
C
C UPDATE NUMBERS GAME PURGE DATA
C
3000	CONTINUE
	DO 3010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 3010             !V20

	    IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
	        KIKWIN=.TRUE.
	        GOTO 3010
	    ENDIF

	    DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
	    IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
	        WRITE(5,901) IAM(),GTNAMES(TNBR),GIND,DRWIND
	        CALL GPAUSE
	    ENDIF

	    DIV=VDETAIL(VDIV,I)
	    SHR=VDETAIL(VSHR,I)
	    BNS=VDETAIL(VBDR,I)+1
	    NPRG(1,DIV,BNS,DRWIND,GIND) = NPRG(1,DIV,BNS,DRWIND,GIND)+1
            NPRG(2,DIV,BNS,DRWIND,GIND) = NPRG(2,DIV,BNS,DRWIND,GIND)+SHR
	    NFLAGS(DRWIND,GIND)=1
3010	CONTINUE
	IF(KIKWIN) GOTO 7000
	GOTO 100
C
C UPDATE SCORE GAME PURGE DATA
C
4000    CONTINUE
        DO 4010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 4010             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 4010
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TSCR),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            RPRG(DRWIND,GIND)=RPRG(DRWIND,GIND)+SHR
            RFLAGS(DRWIND,GIND)=1
4010    CONTINUE
        IF(PVALREC(VRAMT).GT.0) RFLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE WINNERS TIP GAME PURGE DATA
C
5000    CONTINUE
        DO 5010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 5010             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 5010
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TWIT),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            WPRG(DRWIND,GIND)=WPRG(DRWIND,GIND)+SHR
            WFLAGS(DRWIND,GIND)=1
5010    CONTINUE
        IF(PVALREC(VRAMT).GT.0) WFLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE TOTO SELECT GAME PURGE DATA
C
6000    CONTINUE
        DO 6010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 6010             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 6010
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TTSL),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            TPRG(DRWIND,GIND)=TPRG(DRWIND,GIND)+SHR
            TFLAGS(DRWIND,GIND)=1
6010    CONTINUE
        IF(PVALREC(VRAMT).GT.0) TFLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE KICKER GAME PURGE DATA
C
7000	CONTINUE
	DO 7010 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7010             !V20

	    IF(VDETAIL(VKIK,I).EQ.0 .AND.VDETAIL(VKI2,I).EQ.0) GOTO 7010
	    DRWIND=DAYHDR(KGAM)-VDETAIL(VDRW,I)+1
	    IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
	        WRITE(5,901) IAM(),GTNAMES(TKIK),GIND,DRWIND
	        CALL GPAUSE
	    ENDIF
	    DIV=VDETAIL(VDIV,I)
	    SHR=VDETAIL(VSHR,I)
	    KPRG(DIV,DRWIND,KIND)=KPRG(DIV,DRWIND,KIND)+SHR
	    KFLAGS(DRWIND,KIND)=1
7010	CONTINUE
	GOTO 100
C
C UPDATE BINGO GAME PURGE DATA
C
7600    CONTINUE
        DO 7610 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7610             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7610
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            SUBGAME=VDETAIL(VSUB,I)
            BPRG(DIV,SUBGAME,DRWIND,GIND) = BPRG(DIV,SUBGAME,DRWIND,GIND) +
     *                                      SHR
            BFLAGS(DRWIND,GIND)=1
7610    CONTINUE
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE DOUBLE GAME PURGE DATA
C
7700    CONTINUE
        DO 7710 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7710             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7710
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TDBL),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            DPRG(DRWIND,GIND)=DPRG(DRWIND,GIND)+SHR
            DFLAGS(DRWIND,GIND)=1
7710    CONTINUE
        IF(PVALREC(VRAMT).GT.0) DFLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE COUPLE GAME PURGE DATA
C
7800    CONTINUE
        DO 7810 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7810             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7810
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TCPL),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            CPRG(DRWIND,GIND)=CPRG(DRWIND,GIND)+SHR
            CFLAGS(DRWIND,GIND)=1
7810    CONTINUE
        IF(PVALREC(VRAMT).GT.0) CFLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE SUPER SCORE GAME PURGE DATA
C
7900    CONTINUE
        DO 7910 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7910             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7910
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TSSC),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            SSC_PRG(DRWIND,GIND)=SSC_PRG(DRWIND,GIND)+SHR
            SSC_FLAGS(DRWIND,GIND)=1
7910    CONTINUE
        IF(PVALREC(VRAMT).GT.0) SSC_FLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE TODAYS TRIO GAME PURGE DATA
C
7950    CONTINUE
        DO 7960 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7960             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7960
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TTRP),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            TRP_PRG(DRWIND,GIND)=TRP_PRG(DRWIND,GIND)+SHR
            TRP_FLAGS(DRWIND,GIND)=1
7960    CONTINUE
        IF(PVALREC(VRAMT).GT.0) TRP_FLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE SUPER TRIPLE GAME PURGE DATA
C
7970    CONTINUE
        DO 7980 I=1,PVALREC(VPZOFF)

            IF (VDETAIL(VOP,I).EQ.1) GOTO 7980             !V20

            IF(VDETAIL(VKIK,I).NE.0.OR.VDETAIL(VKI2,I).NE.0) THEN
                KIKWIN=.TRUE.
                GOTO 7980
            ENDIF

            DRWIND=DAYHDR(GAM)-VDETAIL(VDRW,I)+1
            IF(DRWIND.LT.1.OR.DRWIND.GT.MDRAWS) THEN
                WRITE(5,901) IAM(),GTNAMES(TSTR),GIND,DRWIND
                CALL GPAUSE
            ENDIF

            DIV=VDETAIL(VDIV,I)
            SHR=VDETAIL(VSHR,I)
            STR_PRG(DRWIND,GIND)=STR_PRG(DRWIND,GIND)+SHR
            STR_FLAGS(DRWIND,GIND)=1
7980    CONTINUE
        IF(PVALREC(VRAMT).GT.0) STR_FLAGS(DRWIND,GIND)=1
        IF(KIKWIN) GOTO 7000
        GOTO 100
C
C UPDATE LOTTO GAME FILES
C
8000	CONTINUE

	DO 8100 GIND=1,MAXIND
	    GNUM=GTNTAB(TLTO,GIND)
	    IF(GNUM.LT.1) GOTO 8100
	    IF(DAYHDR(GNUM).LT.1) GOTO 8100
	    WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DLTSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
	    DO 8020 DRWIND=1,MDRAWS
	        IF(LFLAGS(DRWIND,GIND).EQ.0) GOTO 8020
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        CALL READW(FDB,DRAW,DLTREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	        IF(DLTPUP.GE.CDC) GOTO 8020
	        DO BNS=1,2
	            DO DIV=1,LTGDIV
	                DLTPRG(DIV,BNS) = DLTPRG(DIV,BNS) + 
     *                                    LPRG(DIV,BNS,DRWIND,GIND)
	            END DO
                END DO

	        DLTPUP=CDC
C
	        CALL WRITEW(FDB,DRAW,DLTREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
8020	    CONTINUE
	    CALL USRCLOS1(3)
8100	CONTINUE
C
C UPDATE SPORTS GAME FILES
C
	DO 9100 GIND=1,MAXIND
	    GNUM=GTNTAB(TSPT,GIND)
	    IF(GNUM.LT.1) GOTO 9100
	    IF(DAYHDR(GNUM).LT.1) GOTO 9100
	    WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DSPSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
	    DO 9020 DRWIND=1,MDRAWS
	        IF(SFLAGS(DRWIND,GIND).EQ.0) GOTO 9020
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        CALL READW(FDB,DRAW,DSPREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	        IF(DSPPUP.GE.CDC) GOTO 9020
	        DO DIV=1,SPGDIV
	            DSPPRG(DIV)=DSPPRG(DIV)+SPRG(DIV,DRWIND,GIND)
                END DO

	        DSPPUP=CDC
C
	        CALL WRITEW(FDB,DRAW,DSPREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
9020	    CONTINUE
	    CALL USRCLOS1(3)
9100	CONTINUE
C
C UPDATE TOTOGOLO GAME FILES
C
	DO 9500 GIND=1,MAXIND
	    GNUM=GTNTAB(TTGL,GIND)
	    IF(GNUM.LT.1) GOTO 9500
	    IF(DAYHDR(GNUM).LT.1) GOTO 9500
	    WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DTGSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
	    DO 9520 DRWIND=1,MDRAWS
	        IF(TGFLAGS(DRWIND,GIND).EQ.0) GOTO 9520
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        CALL READW(FDB,DRAW,DTGREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	        IF(DTGPUP.GE.CDC) GOTO 9520
	        DO DIV=1,TGGDIV
	            DTGPRG(DIV)=DTGPRG(DIV)+TGPRG(DIV,DRWIND,GIND)
                END DO

	        DTGPUP=CDC
C
	        CALL WRITEW(FDB,DRAW,DTGREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
9520	    CONTINUE
	    CALL USRCLOS1(3)
9500	CONTINUE
C
C UPDATE NUMBERS GAME FILES
C
	DO 10100 GIND=1,MAXIND
	    GNUM=GTNTAB(TNBR,GIND)
	    IF(GNUM.LT.1) GOTO 10100
	    IF(DAYHDR(GNUM).LT.1) GOTO 10100
	    WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DNBSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
	    DO 10020 DRWIND=1,MDRAWS
	        IF(NFLAGS(DRWIND,GIND).EQ.0) GOTO 10020
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        CALL READW(FDB,DRAW,DNBREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	        IF(DNBPUP.GE.CDC) GOTO 10020
	        DO BNS=1,2
	            DO DIV=1,NBGPOL
	                DNBPRG(1,DIV,BNS) = DNBPRG(1,DIV,BNS)+
     *                                      NPRG(1,DIV,BNS,DRWIND,GIND)
                        DNBPRG(2,DIV,BNS) = DNBPRG(2,DIV,BNS)+
     *                    NPRG(2,DIV,BNS,DRWIND,GIND)*DNBPRZ(DIV,BNS)
                    END DO
                END DO

	        DNBPUP=CDC
C
	        CALL WRITEW(FDB,DRAW,DNBREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
10020	    CONTINUE
	    CALL USRCLOS1(3)
10100	CONTINUE
C
C UPDATE SCORE GAME FILES
C
        DO 11100 GIND=1,MAXIND
            GNUM=GTNTAB(TSCR,GIND)
            IF(GNUM.LT.1) GOTO 11100
            IF(DAYHDR(GNUM).LT.1) GOTO 11100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DSCSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 11020 DRWIND=1,MDRAWS
                IF(RFLAGS(DRWIND,GIND).EQ.0) GOTO 11020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DSCREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DSCPUP.GE.CDC) GOTO 11020
                DSCPRG=DSCPRG+RPRG(DRWIND,GIND)
                DSCPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DSCREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
11020       CONTINUE
            CALL USRCLOS1(3)
11100   CONTINUE
C
C UPDATE WINNERS TIP GAME FILES
C
        DO 12100 GIND=1,MAXIND
            GNUM=GTNTAB(TWIT,GIND)
            IF(GNUM.LT.1) GOTO 12100
            IF(DAYHDR(GNUM).LT.1) GOTO 12100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DWISEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 12020 DRWIND=1,MDRAWS
                IF(WFLAGS(DRWIND,GIND).EQ.0) GOTO 12020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DWIREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DWIPUP.GE.CDC) GOTO 12020
                DWIPRG=DWIPRG+WPRG(DRWIND,GIND)
                DWIPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DWIREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
12020       CONTINUE
            CALL USRCLOS1(3)
12100   CONTINUE
C
C UPDATE TOTO SELECT GAME FILES
C
        DO 13100 GIND=1,MAXIND
            GNUM=GTNTAB(TTSL,GIND)
            IF(GNUM.LT.1) GOTO 13100
            IF(DAYHDR(GNUM).LT.1) GOTO 13100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DTSSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 13020 DRWIND=1,MDRAWS
                IF(TFLAGS(DRWIND,GIND).EQ.0) GOTO 13020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DTSREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DTSPUP.GE.CDC) GOTO 13020
                DTSPRG=DTSPRG+TPRG(DRWIND,GIND)
                DTSPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DTSREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
13020       CONTINUE
            CALL USRCLOS1(3)
13100   CONTINUE
C
C UPDATE KICKER GAME FILES
C
	DO 14100 GIND=1,MAXIND
	    GNUM=GTNTAB(TKIK,GIND)
	    IF(GNUM.LT.1) GOTO 14100
	    IF(DAYHDR(GNUM).LT.1) GOTO 14100
	    WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
	    CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
	    CALL IOINIT(FDB,3,DKKSEC*256)
	    IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
	    DO 14020 DRWIND=1,MDRAWS
	        IF(KFLAGS(DRWIND,GIND).EQ.0) GOTO 14020
	        DRAW=DAYHDR(GNUM)-DRWIND+1
	        CALL READW(FDB,DRAW,DKKREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

	        IF(DKKPUP.GE.CDC) GOTO 14020
	        DO DIV=1,KIGDIV
	            DKKPRG(DIV)=DKKPRG(DIV)+KPRG(DIV,DRWIND,GIND)
                END DO
	        DKKPUP=CDC
C
	        CALL WRITEW(FDB,DRAW,DKKREC,ST)
	        IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
14020	    CONTINUE
	    CALL USRCLOS1(     3)
14100	CONTINUE
C
C UPDATE BINGO GAME FILES
C
        DO 20100 GIND=1,MAXIND
            GNUM=GTNTAB(TBNG,GIND)
            IF(GNUM.LT.1) GOTO 20100
            IF(DAYHDR(GNUM).LT.1) GOTO 20100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DBNSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 20020 DRWIND=1,MDRAWS
                IF(BFLAGS(DRWIND,GIND).EQ.0) GOTO 20020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DBNREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DBNPUP.GE.CDC) GOTO 20020
                DO SUBGAME=1,BGOSUB
                   DO DIV=1,BGODIV
                      DBNPRG(DIV,SUBGAME) = DBNPRG(DIV,SUBGAME) + 
     *                                      BPRG(DIV,SUBGAME,DRWIND,GIND)
                   ENDDO
                ENDDO
                DBNPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DBNREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
20020       CONTINUE
            CALL USRCLOS1(3)
20100    CONTINUE
C
C UPDATE DOUBLE GAME FILES
C
        DO 21100 GIND=1,MAXIND
            GNUM=GTNTAB(TDBL,GIND)
            IF(GNUM.LT.1) GOTO 21100
            IF(DAYHDR(GNUM).LT.1) GOTO 21100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DDBSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 21020 DRWIND=1,MDRAWS
                IF(DFLAGS(DRWIND,GIND).EQ.0) GOTO 21020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DDBREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DDBPUP.GE.CDC) GOTO 21020
                DDBPRG=DDBPRG+DPRG(DRWIND,GIND)
                DDBPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DDBREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
21020       CONTINUE
            CALL USRCLOS1(3)
21100   CONTINUE
C
C UPDATE COUPLE GAME FILES
C
        DO 22100 GIND=1,MAXIND
            GNUM=GTNTAB(TCPL,GIND)
            IF(GNUM.LT.1) GOTO 22100
            IF(DAYHDR(GNUM).LT.1) GOTO 22100
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DCPSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 22020 DRWIND=1,MDRAWS
                IF(CFLAGS(DRWIND,GIND).EQ.0) GOTO 22020
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DCPREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DCPPUP.GE.CDC) GOTO 22020
                DCPPRG=DCPPRG+CPRG(DRWIND,GIND)
                DCPPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DCPREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
22020       CONTINUE
            CALL USRCLOS1(3)
22100   CONTINUE
C
C UPDATE SUPER SCORE GAME FILES
C
        DO 23000 GIND=1,MAXIND
            GNUM=GTNTAB(TSSC,GIND)
            IF(GNUM.LT.1) GOTO 23000
            IF(DAYHDR(GNUM).LT.1) GOTO 23000
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DSSSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 23100 DRWIND=1,MDRAWS
                IF(SSC_FLAGS(DRWIND,GIND).EQ.0) GOTO 23100
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DSSREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DSSPUP.GE.CDC) GOTO 23100
                DSSPRG=DSSPRG+SSC_PRG(DRWIND,GIND)
                DSSPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DSSREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
23100       CONTINUE
            CALL USRCLOS1(3)
23000       CONTINUE
C
C UPDATE TODAYS TRIO GAME FILES
C
        DO 24000 GIND=1,MAXIND
            GNUM=GTNTAB(TTRP,GIND)
            IF(GNUM.LT.1) GOTO 24000
            IF(DAYHDR(GNUM).LT.1) GOTO 24000
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DTRSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 24100 DRWIND=1,MDRAWS
                IF(TRP_FLAGS(DRWIND,GIND).EQ.0) GOTO 24100
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DTRREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DTRPUP.GE.CDC) GOTO 24100
                DTRPRG=DTRPRG+TRP_PRG(DRWIND,GIND)
                DTRPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DTRREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
24100       CONTINUE
            CALL USRCLOS1(3)
24000   CONTINUE
C
C UPDATE SUPER TRIPLE GAME FILES
C
        DO 25000 GIND=1,MAXIND
            GNUM=GTNTAB(TSTR,GIND)
            IF(GNUM.LT.1) GOTO 25000
            IF(DAYHDR(GNUM).LT.1) GOTO 25000
            WRITE(5,902) IAM(),(GFNAMES(K,GNUM),K=1,5)
C
            CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,3,DSTSEC*256)
            IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
C
            DO 25100 DRWIND=1,MDRAWS
                IF(STR_FLAGS(DRWIND,GIND).EQ.0) GOTO 25100
                DRAW=DAYHDR(GNUM)-DRWIND+1
                CALL READW(FDB,DRAW,DSTREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)

                IF(DSTPUP.GE.CDC) GOTO 25100
                DSTPRG=DSTPRG+STR_PRG(DRWIND,GIND)
                DSTPUP=CDC
C
                CALL WRITEW(FDB,DRAW,DSTREC,ST)
                IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),3,ST,DRAW)
25100       CONTINUE
            CALL USRCLOS1(3)
25000   CONTINUE
C
C                                                                               
C UPDATE WIN RESERVE FUND FILE                                                  
C                                                                               
        CALL OPENW(1,SFNAMES(1,RDF),4,0,0,ST)      
        CALL IOINIT(FDB,1,RDFSEC*256)              
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),1,ST,0)     

        CALL READW(FDB,1,RDFREC,ST)            
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),1,ST,1)      
        DO I=1,MAXGAM                                                       
            RDF_WRFTAB(ONPAMT,I)=RDF_WRFTAB(ONPAMT,I)+PRGAMT(I)    
        END DO

        CALL WRITEW(FDB,1,RDFREC,ST)                                              
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,RDF),2,ST,1)                            
        CALL USRCLOS1(1)                                                             


	WRITE(5,903) IAM(),COUNT(1),(SFNAMES(K,VLF),K=1,5),
     *	             IAM(),COUNT(2),(SFNAMES(K,VLC),K=1,5),
     *	             IAM(),COUNT(3),(SFNAMES(K,CTP),K=1,5),
     *	             IAM(),COUNT(4),(SFNAMES(K,UTP),K=1,5)

	CALL GSTOP(GEXIT_SUCCESS)

C
C     =================== Format Statements =================
C
901	FORMAT(1X,A,1X,A8,I1.1,' purge table overflow index> ',I6)
902	FORMAT(1X,A,' Posting purge data to ',5A4)
903	FORMAT(//,1X,A,1X,I7,' records read from ',5A4,/,
     *	          1X,A,1X,I7,' records written to ',5A4,/,
     *	          1X,A,1X,I7,' records written to ',5A4,/,
     *	          1X,A,1X,I7,' records written to ',5A4,//)
904     FORMAT(1X,A,'Purge draw ',I4,' for ',4A4)
C

	END
