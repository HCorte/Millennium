C PROGRAM BLDSYS
C
C V81 13-MAY-2021 SCML New Terminals Project (COMOLM)
C V80 01-APR-2015 SCML M16 PROJECT: added EUSPFIR, EUSPICA
C                      SPBLRP renamed to EUSPBIR
C                      EUSPREP renamed to EUSPGRR
C V79 20-JUL-2015 SCML Added support for IGS internal cancel flags
C V78 05-MAY-2013 SCML Placard Project
C V77 11-OCT-2013 SCML   Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C V76 01-NOV-2010 FJG Loto2 Batch: RFSS#0163 PRETTIM
C     30-NOV-2010 FJG DLTJPG BLDLTO GIND ARGUMENT
C V75 22-SEP-2010 MAC RFSS0145 - ASFIV FILE ADDED
C V74 01-JAN-2010 FJG ePASSIVE
C V73 16-MAR-2209 LRS EUMJOKER parameter added
C V72 21-MAR-2005 FRP VALCDULI added for CDU Enhancements.
C V71 16-MAY-2001 EPH ALLOW BIGGER VALUE FOR GLOBAL REDMAX
C                     ALLOW TO CONFIGURE PASSIVE REDMAX SEPARATE FROM OTHER GAMES
C V70 05-FEB-2001 EPH Include VALORDER, VALORDHI, VALPRZHI, DAYHDPHI, DAYHDPRG
c v69 15-DEC-2000 EPH Lotto system bets and base price factor 
C V68 03-DEC-2000 UXN MAXTGL,MINTGL ADDED.
C V67 17-OCT-2000 UXN Alpha baseline release.
C V66 06-OCT-2000 UXN FSE_SNON, CHKWRT added.
C V65 04-SEP-2000 UXN RUNSYS changes - TAPESW, DISKSW, PRMSTR added.
C V64 25-JUL-2000 UXN Value 5 allowed for MESLOG.
C V63 12-JUN-2000 UXN Cleaned up.
C V62 03-MAR-2000 OXK Allowed non regular Sports (Vakio changes)
C V61 29-FEB-2000 UXN SPTTCF added.
C V60 22-FEB-2000 OXK NRSPTWN, MINSJCK, MAXSJCK, MINSPT added.
C V59 15-FEB-2000 UXN REG_DRWPCK and ODD_DRWPCK added.
C V58 11-NOV-1999 RXK Parameter OGQLIM for oddset game ticket price confirmation
C                     question limit added. 
C V57 13-OCT-1999 RXK World Tour added.
C V56 07-JUL-1999 UXN MAXSCR added.
C V55 24-MAY-1999 UXN OUTPUT LUN CHNAGED TO 6.
C V54 10-MAY-1999 UXN Today's Triple changed to Today's Trio.
C                     MAXSCRS,MAXCPLS,MAXDBLS,MAXSTAKE added.
C                     SUPER TRIPLE GAME ADDED.
C V53 03-MAR-1997 RXK Input of reddef and redimn in marks 
C V53 18-FEB-1997 RXK RFSS #325:input of redmin,redmax per game commented out,
C                     values of REDDEF and REDIMN used for all games instead
C V52 05-FEB-1997 WPW GVTGPL parameter added.
C V51 30-JAN-1997 WPW BCHSIZ,VALPRNT,VALPAM and ACTPRNT added.
C V50 28-JAN-1997 RXK Update values of TIERLIM,GVTSUP,FWDCNT,PASSEXP added
C V49 16-JAN-1997 HXK Added IPD TCP address parameters
C V48 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V47 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V46 21-JAN-1996 HXK Added MAXDBL, MAXWIT, MAXCPL parameters
C V45 05-JAN-1996 PXB delete game number changed from 20 to 40
C V44 23-NOV-1995 PXB dded dbl and cpl games
C V43 26-JUL-1995 PXB Changed all displays of v65 to ravi.
C V42 17-JUL-1995 HXK Various bug fixes, etc. for Ravi batch
C V41 16-JUL-1995 HXK Set Sports game type to regular in Finland by default
C V40 24-APR-1995 HXK Merge of V5 development with March 10th 1995 bible
C V39 05-MAR-1995 HXK Further changes for V5
C V38 18-FEB-1995 HXK Changes for V5 game
C V37 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V36 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V35 26-APR-1994 JXP Included win holding amount and day limits
C V34 14-MAR-1994 HXK ENLARGED TSLIAB DISPLAY FIELD.
C V33 13-MAR-1994 HXK INCREASED MAXIMUM OF TSLIAB FROM 19999980 TO 30000000.
C V32 05-MAR-1994 JXP Included TSMXLI
C V31 02-MAR-1994 JXP INCLUDED TSMXOD PARAMETER
C V30 14-DEC-1993 SXH Added TVTIME parameter
C V29 27-OCT-1993 GXA Added CANDRW parameter.
C V28 17-OCT-1993 GXA Added Auto Invoice Flag parameter.
C V27 06-SEP-1993 SXH Set allowable values of CSHDAY to be 0 -> 366
C V26 16-AUG-1993 HXN Added Redemption minimum field.
C V25 06-AUG-1993 HXK CHANGED SPGRMX,SPPRMX TO MAXSPT,MAXPPP (NO MAXSPD)
C V24 19-JUL-1993 SXH Added MAXSPT and MAXSPD
C V23 16-JUL-1993 GXA Added parameters, TSLMAX,REDDEF,VSCINT,VSCTIM,MAXV65,
C                     WRNV65,SPPRMX,DSPBDET,TSWMAX.
C V22 07-JUL-1993 HXK ADDED RAVI, SPEDEN SCREENING FILES (AND OTHERS)
C V21 05-JUL-1993 HXK CHANGED KICKER GAME INPUT SO GAME CAN BE TURNED OFF
C V20 30-JUN-1993 GXA Added BLDTSL, BLDSCR and cleaned up Opt. 3 game entry to
C                     work for Oddset.
C V19 17-JUN-1993 SXH Added RAVI (V65)
C V18 08-JUN-1993 SXH Cosmetic change for SET JOKERI PARAMETERS
C V17 03-JUN-1993 HXK Added max fractions.
C V16 18-MAY-1993 SXH Use GTNAMES.DEF, not hard-coded gtypes local variable
C V15 04-MAY-1993 STUART No change.
C V14 03-MAY-1993 STUART Added Viking Lotto
C V13 27-APR-1993 STUART No change.
C V12 12-FEB-1993 EBD No code changes just putting back an old revision
C V11 11-FEB-1993 EBD Corrected some minor typos in format statements
C V10 08-FEB-1993 NJN Changed bldsys_movchr Lib$movc3 ( rxd)
C V09 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V08 02-MAR-1996 wsm Added COMPRESSED_LOAD, SUPMCP0, changed DSPPAR=54,
C                            added dummy argument to SPCDLL call.
C V07 31-MAY-1993 HJK  ADDED SPEDEN, OPINION POLL, INSTANT AND FRACTIONS.
C V06 02-JUL-1992 GCAN ADDED TOTO SELET LIABILITY PARAMETERS.
C                      SPLIT GLOBAL PARAMETER SCREEN INTI TWO.
C                      ADDED DESCRIPTIN LINE TO ALL GLOBAL PARAMETERS.
C                      ADDED FILE EXISTS FEATURE TO SYSTEM FILES OPTION.
C V05 11-MAY-1992 HDB  ADDED STATISTICS FILE (STF = 58)
C V04 06-APR-1992 HDB  ADDED: ODDSET MENU 15, VOLNAME CHANGES MENU 0       
C V03 26-FEB-1992 GCAN ADDED JACKPOT AND TV NEWS MESSAGE PARAMETERS.
C V02 07-OCT-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C PROGRAM TO SET GLOBAL SYSTEM/GAME PARAMETERS
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
        PROGRAM BLDSYS
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESPAR.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C
        INTEGER*4   NO_OF_OPTIONS
        PARAMETER  (NO_OF_OPTIONS = 22)  ! No of options on top level menu

        INTEGER*4   MAXROW 
        PARAMETER   (MAXROW=23) !Max # Rows on one screen with prompt line.

        INTEGER*4   DSPPAR, DSPPAR_EVEN
C       PARAMETER   (DSPPAR=100)         ! number of parameters     !V70
C       PARAMETER   (DSPPAR=104)         ! number of parameters      
C       PARAMETER   (DSPPAR=120)         ! number of parameters      
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
CC----+------------------------------------------------------------------
CC V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
CC----+------------------------------------------------------------------
C       PARAMETER   (DSPPAR=139)         ! number of parameters 
CC----+------------------------------------------------------------------
CC V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
CC----+------------------------------------------------------------------
CV80        PARAMETER   (DSPPAR=140)         ! number of parameters 
C        PARAMETER   (DSPPAR=142)         ! number of parameters                 !V80
        PARAMETER   (DSPPAR=143)         ! number of parameters                 !V81        
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        PARAMETER   (DSPPAR_EVEN=((DSPPAR-1)/2 + 1)*2)

        INTEGER*4   NRM_YES
        PARAMETER   (NRM_YES=1) !return from NRM_YESNO

        INTEGER*4   NRM_NO
        PARAMETER   (NRM_NO=2)  !return from NRM_YESNO

        INTEGER*4   FTLUN 
        PARAMETER   (FTLUN =2)  !Logical unit to use for testing file size.
C
        INTEGER*4   FDB(7)              !
        INTEGER*4   PNUM(DSPPAR_EVEN)   !
        INTEGER*4   PMAX(DSPPAR)        !
        INTEGER*4   PMIN(DSPPAR)        !
        INTEGER*4   FILE(5)             !
C
        INTEGER*4   GIND                !
        INTEGER*4   TC                  !
        INTEGER*4   CONT                !
        INTEGER*4   GNUM                !
        INTEGER*4   COM                 !
        INTEGER*4   FSIZE               !
        INTEGER*4   KNUM                !
        INTEGER*4   TMPNUM              !
        INTEGER*4   CNT                 !
        INTEGER*4   GTIND               !
        INTEGER*4   GTNUM               !
        INTEGER*4   KN                  !
        INTEGER*4   GI                  !
        INTEGER*4   GT                  !
        INTEGER*4   GAM                 !
        INTEGER*4   ERR                 !
        INTEGER*4   K                   !
        INTEGER*4   VAL                 !
        INTEGER*4   NUM                 !
        INTEGER*4   MAX                 !
        INTEGER*4   EXT                 !
        INTEGER*4   OPT                 !
        INTEGER*4   ST                  !
        INTEGER*4   BLANK               !
        INTEGER*4   I                   !
        INTEGER*4   J                   !
        INTEGER*4   PRMX                !
        INTEGER*4   FRMX                !Local fraction maximum.
        INTEGER*4   LIM                 !
        INTEGER*4   VSIZE               !
        INTEGER*4   FRMLIN              !List From Line.
        INTEGER*4   TOLINE              !List To Line.
        INTEGER*4   SFIL                !Starting File # on Page.
        INTEGER*4   EFIL                !Ending File # on Page.
        INTEGER*4   PAGE                !File Page # currently displayed.
        INTEGER*4   SIND
        INTEGER*4   FILSIZ(MAXFIL)      !Actual File Sizes.
        INTEGER*4   HLDLIM(3)           !Local win limit.
        INTEGER*4   HLDDAY(3)           !Local holding days.
        INTEGER*4   ANS
        INTEGER*2   ITMP
	INTEGER*4   RDMX
        INTEGER*4   AUXDUMMY
        INTEGER*4   DUMMY
	PARAMETER   (DUMMY=0)
C
        INTEGER*4   TCP_TEMP(15)
        INTEGER*4   TCP_INDEX
        INTEGER*4   TCP_LIMITS(14)
        DATA        TCP_LIMITS/9999,
     *                         9999,
     *                         9999,
     *                         9999,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255,
     *                          255/
C
        CHARACTER   PNAME(DSPPAR_EVEN)*8
        CHARACTER   SFILES(MAXFIL)*15
        CHARACTER   FNAMES(20,MAXFIL)
        CHARACTER   GFNAMES(20,MAXGAM)
        CHARACTER   GVNAMES(20,MAXGAM)
        CHARACTER   STYP(0:2)*7
        CHARACTER   PDESCR(DSPPAR_EVEN)*20 !Parameter Description Line.
        CHARACTER   EXFILC(MAXFIL)*3    !File Exist? in character.
        CHARACTER   YES_CHR*3           !Yes Character Representation.
        CHARACTER   NOP_CHR*3           !No  Character Representation.
        CHARACTER   FILNAM*20           !File Name for INQUIRE call.
        CHARACTER   TMPNAM(20)          !Temp. File Name.
C
        LOGICAL     EXFIL               !Does File Exist?
        LOGICAL     SCANED/.FALSE./     !Have I scaned for file sizes yet?
C

        EQUIVALENCE (FNAMES,SCFSFN)
        EQUIVALENCE (GFNAMES,SCFGFN)
        EQUIVALENCE (GVNAMES,SCFGVN)
C
        COMMON SCFREC
C
C
        DATA YES_CHR/'Yes'/
        DATA NOP_CHR/'No '/
C
        DATA STYP/'       ','regular','special'/
C
        DATA (SFILES(I),I=1,31)
     *      /'DAILY ACTIVITY ','AGENT SALES    ','SYSTEM MESSAGE ',
     *       'PRIMARY TMF    ','RESERVED       ','RESERVED       ',
     *       'BACKUP TMF     ','DAILY CARRYOVER',
     *       'DAILY CARY COPY','DAILY CARY WORK','VALIDATION     ',
     *       'VALID COPY     ','VALID WORK     ','CARRYOVER      ',
     *       'CARRY COPY     ','CARRY WORK     ','CHECKPOINT - 0 ',
     *       'CHECKPOINT - 1 ','CHECKPOINT - 2 ','CHECKPOINT - 3 ',
     *       'CHECKPOINT - 4 ','LTOPOL (RAMDSK)',
     *       'SPORTS-SYSTEM  ','LOTTO-SYSTEM   ','LOTTO-POOLS 1  ',
     *       'LOTTO-POOLS 2  ','LOTTO-OVER  1  ','LOTTO-OVER  2  ',
     *       'CASHED PURGE   ','UNCASHED PURGE ','CLERK ACCOUNT  '/
C
        DATA (SFILES(I),I=32,75)
     *      /'REPORTING      ','INSTANT VALID  ','X2X LOG FILE   ',
     *       'X2X GLOBAL     ',
     *       'X2X NET PORT   ','X2X LOCAL PORT ','X2X PORT CONF  ',
     *       'X2X REP CODE   ','X2X REP CLASS  ','X2X STN CLASS  ',
     *       'X2X STATION    ','X2X TERMINAL   ','X2X AUTO BUILD ',
     *       'X2X BROADCAST  ','X2X RELAY GRPS ','X2X TITAN      ',
     *       'GUTS REV. FILE ','GUTS STATS FILE','GUTS FAULT FILE',
     *       'GUTS SWAP FILE ','TKT MESSAGE    ','ELOG FILE      ',
     *       'PROMO/DISC FILE','DELETED AGENT  ','ROLL POOL FILE ',
     *       'DISTREP FILE   ','STATISTICS FILE','RESULTS-SYSTEM ',
     *       'WIN RESERVE    ','BALANCING FILE ','CASHED TICKETS ', 
     *       'PASSIVE VALID  ','PASSIVE WORK   ','PASSIVE PLAN   ',
     *       'PASSIVE TICKET ','PASSIVE ACCOUNT',                     !V75
     *     8*'               '/                                       !V75
C
        DATA PNAME/'SUPWAG  ','SUPCAN  ','SUPVAL  ','SUPSPE  ',
     *             'SUPCOM  ','SUPFIL  ','SUPSUM  ','SUPRPT  ',
     *             'ROMREV  ','CANTIM  ','DESFLG  ','MESLOG  ',
     *             'WRNPER  ','RUNBEG  ','RUNEND  ','MAXSPT  ',
     *             'CSHDAY  ','DESFLGT ','LCKPMEM ','SUPMSC  ',
     *             'ODSUPD  ','JAKUPD  ','TVNUPD  ','SUPTSP  ',
     *             'TSPMIN  ','TSPPER  ','TSLIAB  ','TSLWRN  ',
     *             'TSTLIM  ','TSLMAX  ','REDDEF  ',
     *             'DSPBDET ','TSWMAX  ','AUTOINV ','CANDRW  ',
     *             'TVTIME  ','TSMXOD  ','TSMXLI  ','SUPREM  ',
     *             'MAXDBL  ','MAXCPL  ','MAXWIT  ',
     *             'COMPRESS','SUPMCP0 ','GVTREV  ','GVTEST  ',
     *             'GVTGPL  ','GVTRST  ','GVTDFL  ','GVTTIM  ',
     *             'SUPFPT  ','FPTTIM  ','SUPINS  ','TIERLIM ',
     *             'GVTSUP  ','FWDCNT  ','PASSEXP ','BCHSIZ  ',
     *             'VALPRNT ','VALPAMT ','ACTPRNT ','GVTSAV  ',
     *             'REDIMN  ','CHKTIM  ','MAXTRP  ','MAXSSC  ',
     *             'MAXSSN  ','SSCINB  ','MAXTRPS ','MAXSSCS ',
     *             'EUROCR  ','MAXKSYS ','MINKJCK ','MAXKJCK ',
     *             'MAXFAV  ','MINPRB  ','MAXSTD  ',
     *             'MAXSCRS ','MAXCPLS ','MAXDBLS ','MAXSTAKE',
     *             'MAXSTR  ','MAXSTRS ','MAXSCR  ',
     *             'OGQLIM  ','REG_DRWP','ODD_DRWP','NRSPTWN ',
     *             'MINSJCK ','MAXSJCK ','MINSPT  ','TAPESW  ',
     *             'DISKSW  ','PRMSTR  ','FSE_SNON','CHKWRT  ',
     *             'MINTGL  ','MAXTGL  ','PRFACTOR','VALORDER',
     *             'VALORDHI','VALPRZHI','DAYHDPHI','DAYHDPRG',
     *             'VALCR1  ','VALCR2  ','VALCT1  ','VALCT2  ',
     *             'VALCT3  ','VALCDULI','EUMJOKER','PCANTIM ',
     *             'PMAXRTM ','PMAXCAN ','PMAXLOP ','PMAXSEK ',
     *             'PASTHRO ','PDAYRSL ','SUPRET  ','PRETTIM ',
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C     *             'OPGENFLG','        '/ ! V78 Commented
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C
C V78 - Start
     *             'OPGENFLG','EUMILF  ','EUSPWAG ','EUSPCAN ',
CV80     *             'EUSPVAL ','EUSPREP ','EUTIMOUT','EUFINTO ',
CV80     *             'SPBLRP  ','IGSCONF ','IGSPPLA ','IGSTOUT ',
     *             'EUSPVAL ','EUSPGRR ','EUTIMOUT','EUFINTO ',                 !V80
     *             'EUSPBIR ','IGSCONF ','IGSPPLA ','IGSTOUT ',                 !V80
     *             'IGSPWAG ','IGSPCAN ','IGSPVAL ','IGSPREP ',
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *             'IGSPFIN ','IGSPRNT ','PLAFINTO','        '/
CV80     *             'IGSPFIN ','IGSPRNT ','PLAFINTO','IGSPICAN'/
     *             'IGSPFIN ','IGSPRNT ','PLAFINTO','IGSPICAN',                 !V80
CV81     *             'EUSPFIR ','EUSPICA '/                                       !V80
     *             'EUSPFIR ','EUSPICA ','OLMCONF ','        '/                 !V81     
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V78 - End
C
        DATA PNUM/SUPWAG,SUPCAN,SUPVAL,SUPSPE,
     *            SUPCOM,SUPFIL,SUPSUM,SUPRPT,
     *            ROMREV,CANTIM,DESFLG,MESLOG,
     *            WRNPER,RUNBEG,RUNEND,MAXSPT,
     *            CSHDAY,DESFLG_TYPE, LOCK_PAGES_MEM,SUPMSC,
     *            ODSUPD,JAKUPD,TVNUPD,SUPTSP,
     *            TSPMIN,TSPPER,TSLIAB,TSLWRN,
     *            TSTLIM,TSLMAX,REDDEF,
     *            DSPBDET,TSWMAX,AUTOINV,CANDRW,
     *            TVTIME,TSMXODD,TSMXLI,REM_LOG,
     *            MAXDBL,MAXCPL,MAXWIT,COMPRESSED_LOAD,SUPMCP0,
     *            GVTREV,GVTEST,GVTGPL,GVTRST,GVTDFL,GVTTIM,
     *            SUPFPT,FPTTIM,SUPINS,TIERLIM,
     *            GVTSUP,FWDCNT,PASSEXP,BCHSIZ,VALPRNT,VALPAMT,
     *            ACTPRNT,GVTSAV,
     *            REDIMN,CHKTIM,MAXTRP,MAXSSC,
     *            MAXSSN,SSCINB,MAXTRPS,MAXSSCS,
     *            EUROCR,MAXKSYS,MINKJCK,MAXKJCK,
     *            MAXFAV,MINPRB,MAXSTD,
     *            MAXSCRS,MAXCPLS,MAXDBLS,MAXSTAKE,
     *            MAXSTR,MAXSTRS,MAXSCR,
     *            OGQLIM,REG_DRWPCK,ODD_DRWPCK,NRSPTWN,
     *		  MINSJCK,MAXSJCK,MINSPT,TAPESW,
     *            DISKSW,PRMSTR,FSE_SNON,CHKWRT,
     *            MINTGL,MAXTGL,PRFACTOR,VALORDER,
     *            VALORDHI,VALPRZHI,DAYHDPHI,DAYHDPRG,
     *            VALCR1, VALCR2, 
     *            VALCT1, VALCT2, VALCT3, 
     *            VALCDULI,EUMJOKER,PCANTIM,PMAXRTM,
     *            PMAXCAN,PMAXLOP,PMAXSEK,PASTHRO,PDAYRSL,SUPRET,
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C     *            PRETTIM,OPGENFLG,0/ ! V78 Commented
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C
C V78 - Start
     *            PRETTIM,OPGENFLG,EUMILF,EUSPWAG,EUSPCAN,EUSPVAL,
CV80     *            EUSPREP,EUTIMOUT,EUFINTO,SPBLRP,IGSCONF,IGSPPLA,
     *            EUSPGRR,EUTIMOUT,EUFINTO,EUSPBIR,IGSCONF,IGSPPLA,             !V80
     *            IGSTOUT,IGSPWAG,IGSPCAN,IGSPVAL,IGSPREP,IGSPFIN,
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *            IGSPRNT,PLAFINTO,0/
CV80     *            IGSPRNT,PLAFINTO,IGSPICAN/
Cv81     *            IGSPRNT,PLAFINTO,IGSPICAN,EUSPFIR,EUSPICA/                    !V80
     *            IGSPRNT,PLAFINTO,IGSPICAN,EUSPFIR,EUSPICA,                     !V81 
     *            OLMCONF,0/    

C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V78 - End
C
        DATA (PMAX(I),I=1,DSPPAR)/   
     *            1, 1, 1, 1,                 ! SUPWAG, SUPCAN, SUPVAL, SUPSPE
     *            1, 1, 1, 1,                 ! SUPCOM, SUPFIL, SUPSUM, SUPRPT
     *            0,                          ! ROMREV
     *            99999,                      ! CANTIM
     *            1, 5,                       ! DESFLG, MESLOG
     *            9999999,                    ! WRNPER
     *            2400, 2400,                 ! RUNBEG, RUNEND
     *            999999,                     ! MAXSPT
     *            366,                        ! CSHDAY
     *            DESFLG_SOFT,
     *            NOLOCK_PAGES_MEM_VALUE,
     *            1,                          ! SUPMSC
     *            99999, 99999, 99999,        ! ODSUPD, JAKUPD, TVNUPD
     *            1,                          ! SUPTSP
     *            1999980,                    ! TSPMIN
     *            100,                        ! TSPPER
     *            30000000,                   ! TSLIAB
     *            19999980,                   ! TSLWRN
     *            99999,                      ! TSTLIM
     *            1999980,                    ! TSLMAX
     *            1999999980,                 ! REDDEF
     *            1,                          ! DSPBDET
     *            19999980,                   ! TSWMAX
     *            1,                          ! AUTOINV
     *            1440, 1440,                 ! CANDRW, TVTIME
     *            1000000,                    ! TSMXOD
     *            1999999980,                 ! TSMXLI
     *            1,                          ! REM_LOG (SUPREM)
     *            1999999980,                 ! MAXDBL
     *            1999999980,                 ! MAXCPL
     *            1999999980,                 ! MAXWIT
     *            1,1,                        ! COMPRESSED_LOAD, SUPMCP0
     *            0,                          ! GVTREV
     *            1,                          ! GVTEST
     *            65000,                      ! GVTGPL
     *            10,                         ! GVTRST
     *            32,                         ! GVTDFL
     *            60,                         ! GVTTIM
     *            1,                          ! SUPFPT
     *            18000,                      ! FPTTIM
     *            1,                          ! SUPINS
     *            600,                        ! TIERLIM
     *            65000,                      ! GVTSUP
     *            7,                          ! FWDCNT
     *            10000,                      ! PASSEXP
     *            28,                         ! BCHSIZ
     *            1,                          ! VALPRNT
     *            128000,                     ! VALPAMT
     *            5,                          ! ACTPRNT
     *            1,                          ! GVTSAV
     *            1999999980,                 ! REDIMN
     *            600000,                     ! CHKTIM (not limited)
     *            1999999980,                 ! MAXTRP
     *            1999999980,                 ! MAXSSC
     *            16777216,                   ! MAXSSN
     *            65535,                      ! SSCINB
     *            1999999980,                 ! MAXTRPS
     *            1999999980,                 ! MAXSSCS
     *            9999999,                    ! EUROCR  
     *            1000,                       ! MAXKSYS
     *            1999999980,                 ! MINKJCK
     *            1999999980,                 ! MAXKJCK
     *            13,                         ! MAXFAV
     *            34000,                      ! MINPRB
     *            50,                         ! MAXSTD
     *            1999999980,                 ! MAXSCRS
     *            1999999980,                 ! MAXCPLS
     *            1999999980,                 ! MAXDBLS
     *            50,                         ! MAXSTAKE
     *            1999999980,                 ! MAXSTR
     *            1999999980,                 ! MAXSTRS
     *            1999999980,                 ! MAXSCR
     *            1999999980,                 ! OGQLIM
     *            1999999980,                 ! REG_DRWPCK
     *            1999999980,                 ! ODD_DRWPCK
     *            1999999980,                 ! NRSPTWN
     *            1999999980,                 ! MINSJCK
     *            1999999980,                 ! MAXSJCK
     *            1999999980,		      ! MINSPT
     *            2,	  		      ! TAPESW
     *            1,	  		      ! DISKSW
     *            2,	  		      ! PRMSTR
     *            1,	  		      ! FSE_SNON 
     *            1,	  		      ! CHKWRT 
     *            1999999980,		      ! MINTGL
     *            1999999980, 		      ! MAXTGL
     *            100,                        ! PRFACTOR
     *            1999999980,                 ! VALORDER
     *            1999999980,                 ! VALORDHI
     *            1999999980,                 ! VALPRZHI
     *            30,                         ! DAYHDPHI
     *            30,                         ! DAYHDPRG
     *            10000, 5000,                ! VALCR1, VALCR2
     *            10000, 10000, 10000,        ! VALCT1, VALCT2, VALCT3
     *            1999999980,                 ! VALCDULI
     *            1,                          ! EUMJOKER
     *            120,                        ! PCANTIM
     *            3600,                       ! PMAXRTM
     *            100,                        ! PMAXCAN
     *            100000,                     ! PMAXLOP     
     *            3,                          ! PMAXSEK
     *            1024,                       ! PASTHRO
     *            200,                        ! PDAYRSL     
     *            1,                          ! SUPRET
     *            120,                        ! PRETTIM
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
     *            1,                          ! OPGENFLG
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C
C V78 - Start
     *            1,                          ! EUMILF
     *            1,                          ! EUSPWAG
     *            1,                          ! EUSPCAN
     *            1,                          ! EUSPVAL
     *            1,                          ! EUSPGRR
     *            50,                         ! EUTIMOUT
     *            50,                         ! EUFINTO
     *            1,                          ! EUSPBIR
     *            1,                          ! IGSCONF
     *            1,                          ! IGSPPLA
     *            50,                         ! IGSTOUT
     *            1,                          ! IGSPWAG
     *            1,                          ! IGSPCAN
     *            1,                          ! IGSPVAL
     *            1,                          ! IGSPREP
     *            1,                          ! IGSPFIN
     *            1,                          ! IGSPRNT
     *            50,                         ! PLAFINTO
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
CV80     *            1/                          ! IGSPICAN
     *            1,                          ! IGSPICAN                        !V80
     *            1,                          ! EUSPFIR                         !V80
CV81     *            1/                          ! EUSPICA                         !V80
     *            1,                          ! EUSPICA                         !V81   
     *            1/                          ! OLMCONF                         !V81
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V78 - End
C
        DATA (PMIN(I),I=1,DSPPAR)/
     *            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     *            0,                          !     CSHDAY
     *            0,                          ! V04 DESFLG_TYPE
     *            0,                          ! V04 LOCK_PAGES_MEM
     *            0,                          !     SUPMSC
     *            0,                          !     ODSUPD
     *            0,                          !     JAKUPD
     *            0,                          !     TVNUPD
     *            0,                          !     SUPTSP
     *            0,                          !     TSPMIN
     *            0,                          !     TSPPER
     *            0,                          !     TSLIAB
     *            0,                          !     TSLWRN
     *            0,                          !     TSTLIM
     *            0,                          !     TSLMAX
     *            0,                          !     REDDEF
     *            0,                          !     DSPBDET
     *            1,                          !     TSWMAX
     *            0,                          !     AUTOINV
     *            0,                          !     CANDRW
     *            1,                          !     TVTIME
     *            0,                          !     TSMXOD
     *            0,                          !     TSMXLI
     *            0,                          !     REM_LOG (SUPREM)
     *            0,                          !     MAXDBL
     *            0,                          !     MAXCPL
     *            0,                          !     MAXWIT
     *            0,0,                        !     COMPRESSED_LOAD,SUPMCP0
     *            0,                          !     GVTREV
     *            0,                          !     GVTEST
     *            0,                          !     GVTGPL
     *            0,                          !     GVTRST
     *            0,                          !     GVTDFL
     *            10,                         !     GVTTIM
     *            0,                          !     SUPFPT
     *            10,                         !     FPTTIM
     *            0,                          !     SUPINS
     *            0,                          !     TIERLIM
     *            0,                          !     GVTSUP
     *            1,                          !     FWDCNT
     *            0,                          !     PASSEXP
     *            1,                          !     BCHSIZ
     *            0,                          !     VALPRNT
     *            0,                          !     VALPAMT
     *            1,                          !     ACTPRNT
     *            1,                          !     GVTSAV
     *            0,                          !     REDIMN
     *            2400,                       !     CHKTIM (min=5min)
     *            0,                          !     MAXTRP
     *            0,                          !     MAXSSC
     *            1,                          !     MAXSSN
     *            4,                          !     SSCINB
     *            0,                          !     MAXTRPS
     *            0,                          !     MAXSSCS
     *            0,                          !     EUROCR      
     *            0,                          !     MAXKSYS
     *            0,                          !     MINKJCK
     *            0,                          !     MAXKJCK
     *            0,                          !     MAXFAV
     *            0,                          !     MINPRB
     *            0,                          !     MAXSTD
     *            0,                          !     MAXSCRS
     *            0,                          !     MAXCPLS
     *            0,                          !     MAXDBLS
     *            0,                          !     MAXSTAKE
     *            0,                          !     MAXSTR
     *            0,                          !     MAXSTRS
     *            0,                          !     MAXSCR
     *            0,                          !     OGQLIM
     *            0,                          !     REG_DRWPCK
     *            0,                          !     ODD_DRWPCK
     *            0,			      !     NRSPTWN
     *            0,			      !	    MINSJCK
     *            0,	                      !	    MAXSJCK
     *            0,                          !     MINSPT
     *            0,                          !     TAPESW
     *            0,                          !     DISKSW
     *            0,                          !     PRMSTR
     *            0,                          !     FSE_SNON
     *            0,                          !     CHKWRT
     *            0,                          !     MINTGL
     *            0,                          !     MAXTGL
     *            1,			      !     PRFACTOR
     *            0,                          !     VALORDER
     *            0,                          !     VALORDHI
     *            0,                          !     VALPRZHI
     *            0,                          !     DAYHDPHI
     *            0,                          !     DAYHDPRG
     *            0,                          !     VALCR1
     *            0,                          !     VALCR2
     *            0,                          !     VALCT1
     *            0,                          !     VALCT2
     *            0,                          !     VALCT3
     *            0,                          !     VALCDULI
     *            0,                          !     EUMJOKER
     *            0,                          !     PCANTIM
     *            120,                        !     PMAXRTM
     *            0,                          !     PMAXCAN
     *            50,                         !     PMAXLOP     
     *            1,                          !     PMAXSEK
     *            0,                          !     PASTHRO   
     *            0,                          !     PDAYRSL     
     *            0,                          !     SUPRET            
     *            0,                          !     PRETTIM
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
     *            0,                          !     OPGENFLG
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C
C V78 - Start
     *            0,                          !     EUMILF
     *            0,                          !     EUSPWAG
     *            0,                          !     EUSPCAN
     *            0,                          !     EUSPVAL
     *            0,                          !     EUSPGRR
     *            1,                          !     EUTIMOUT
     *            1,                          !     EUFINTO
     *            0,                          !     EUSPBIR
     *            0,                          !     IGSCONF
     *            0,                          !     IGSPPLA
     *            1,                          !     IGSTOUT
     *            0,                          !     IGSPWAG
     *            0,                          !     IGSPCAN
     *            0,                          !     IGSPVAL
     *            0,                          !     IGSPREP
     *            0,                          !     IGSPFIN
     *            0,                          !     IGSPRNT
     *            1,                          !     PLAFINTO
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
CV80     *            0/                          !     IGSIPCAN
     *            0,                          ! IGSPICAN                        !V80
     *            0,                          ! EUSPFIR                         !V80
CV81     *            0/                          ! EUSPICA                         !V80
     *            0,                          ! EUSPICA                         !V81
     *            0/                          ! OLMCONF                         !V81
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V78 - End
C
        DATA BLANK/'    '/
C
        DATA (PDESCR(I),I=1,DSPPAR_EVEN)
     *              /'Wager     Supression','Cancel    Supression',
     *               'ValidationSupression','Spec. Fun Supression',
     *               'Comm.     Supression','File      Supression',
     *               'Checksum  Supression','Report    Supression',
     *               'Altura Rom Revision ','Cancel Time Limit   ',
     *               'DES Encryption Flag ','Message Logging levl',
     *               'NBR Liability Limit ','Earliest RUNSYS Time',
     *               'Last RUNSYS Time    ','Max Simple Rows Bet ',
     *               'Days Cashed In VLF  ','DES Encryption Type ',
     *               'Pages Locked in Mem.','Matrix S. Supression',
     *               'Odds Update Time    ','Jackpot Update Time ',
     *               'TV-News Update Time ','Toto Sel Pool Supres',
     *               'Min Sales for liab. ','% Sales Liab. Limit ',
     *               'Toto Sel Liab. Limit','Toto Sel. Liab Warn ',
     *               'TotoSel Warn Tim Lim','Toto Sel Max Bet    ',
     *               'Redem. Max. Default ',
     *               'Reprint Bet Detailes','Toto Sel Max Wag    ',
     *               'Auto Invoice Flag   ','Cancel Time Past Drw',
     *               'TV Text Tim Interval','Pitka MAX Odds limit',
     *               'Pitka AMT Liab limit','Remote Log Suppresn.',
     *               'Max Double Wager    ',
     *               'Max Couple Wager    ','Max W Tip Wager     ',
     *               'Compressed download ','Suppress MCP0       ',
     *               'GVT Rom Revision    ','GVt establ.messages ',
     *               'GVT min to call bck ','Time GVT will reset ',
     *               'GVT bitmap for estab','GVT timer interval  ',
     *               'FPT supression      ','FPT time interval   ',
     *               'Instant supression  ','GVT low tier limit  ',
     *               'GVT supression      ','GVT forward count   ',
     *               'N days to pw expirat','Batch size          ',
     *               'GVT valid supp flag ','GVT val min prnt amt',
     *               'GVT # val copies    ','GVT save phone# flag',
     *               'Redem. Min Def.     ','Checkpoint time     ',
     *               'Max Trio Simpl      ','Max SScore Simpl    ',
     *               'Max SScore System # ','Init.# of buck.(SSC)',
     *               'Max Trio Syst.      ','Max SScore Syst.    ',
     *               'EURO exchange rate  ','Max Jokeri System # ',
     *               'Min Jok Jackpot     ','Max Jok Jackpot     ',
     *               'Max # of Favorites  ',
     *               'Min probab. for WQP ','Max diff.of statist.',
     *               'Max Score Syst.     ','Max Couple Sys.     ',
     *               'Max Double Sys.     ','Maximum stake       ',
     *               'Max Triple Simp     ','Max Triple Syst.    ',
     *               'Max Score  Simp     ',
     *               'Odds.g.quest.Lim    ','Regular draw pack   ',
     *               'Oddset draw pack    ','SPT Jackpot rule lim',
     *               'Min SPT Jackpot     ','Max SPT Jackpot     ',
     *		     'Min Sport prize     ','Backup tape logging ',
     *		     'Backup disk logging ','Instant system conn ',
     *		     'FSE sign-on checking','Check writer supp.  ',
     *		     'Min TotoGolo prize  ','Max TotoGolo Simp.  ',
     *               'Factor to Base Price','Value to generate OP',
     *               'Value to Hi-Tier OP ','Big Prize value     ',
     *               'Big Prize hold days ','OP Purging hold days',
     *               'C.Recep. value      ','C.Recep. val p/agent',
     *               'Transp. value <=25KM','Transp. value 25/75 ',
     *               'Transp. value >75KM ','Amount Limit for CDU',
     *               'EuM Jocker allowed  ','Pas Max CAN Minutes ',
     *               'Pas Max RES Seconds ','Pas Max CANx Agt/Day',     
     *               'Pas Max Loops Search','Pas Max Num Search  ',      
     *               'Pas Trans Threshold ','Pas NumDays CanRSold',           
     *               'Pas Supress Returns ','Pas RET aftClose min',
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C     *               'OPs Generate Flag   ','                    '/ ! V78 Commented
C----+------------------------------------------------------------------
C V77| Added OP Generation Flag: 0 - do not generate OPs; 1 - generate OPs
C----+------------------------------------------------------------------
C
C V78 - Start
     *               'OPs Generate Flag   ','EM System Connect   ',
     *               'EM Suppress Wager   ','EM Suppress Cancel  ',
CV80     *               'EM Suppr Validation ','EM Suppress Report  ',
     *               'EM Suppr Validation ','EM Suppr Game Report',             !V80
     *               'EM Message Time Out ','EM Fin Report TimOut',
     *               'EM Suppr Bill Report','IGS System Connect  ',
     *               'IGS Suppress Placard','IGS Message Time Out',
     *               'IGS Suppress Wager  ','IGS Suppress Cancel ',
     *               'IGS Suppr Validation','IGS Suppress Report ',
     *               'IGS Suppr Fin Report','IGS Suppress Reprint',
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *               'Placrd FinRep TimOut','                    '/
CV80     *               'Placrd FinRep TimOut','IGS Suppr Int Cancel'/
     *               'Placrd FinRep TimOut','IGS Suppr Int Cancel',             !V80
CV81     *               'EM Suppr Fin Report ','EM Suppr Int Cancel '/             !V80
     *               'EM Suppr Fin Report ','EM Suppr Int Cancel ',             !V81
     *               'OLM System Connect  ','                    '/             !V81          
C----+------------------------------------------------------------------
C V79| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C V78 - End
C
        CALL COPYRITE
C
        CALL OPENX(1,'SCF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,SCFSEC*256)
        IF(ST.NE.0) THEN
          TYPE*,'SCF.FIL open error > ',ST
          CALL GPAUSE
        ENDIF
        CALL READW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) THEN
          TYPE*,'SCF.FIL read error > ',ST
          CALL GPAUSE
        ENDIF
C
10      CONTINUE
        CALL CLRSCR(6)
        FRMLIN = 0
        WRITE(6,900)
        CALL INPNUM('Enter option ',OPT,0,NO_OF_OPTIONS,EXT)
	IF(EXT.EQ.-12) GOTO 32000
        IF(EXT.LT.0) GOTO 31000
        IF(OPT.EQ.0) GOTO 500
        IF(OPT.EQ.1) GOTO 1000
        IF(OPT.EQ.2) GOTO 2000
        IF(OPT.EQ.3) GOTO 3000
        IF(OPT.EQ.4) GOTO 4000
        IF(OPT.EQ.5) GOTO 5000
        IF(OPT.EQ.6) GOTO 6000
	IF(OPT.EQ.7) GOTO 6500
        IF(OPT.EQ.8) GOTO 7000
        IF(OPT.EQ.9) GOTO 8000
        IF(OPT.EQ.10) GOTO 9000
        IF(OPT.EQ.11) GOTO 10000
        IF(OPT.EQ.12) GOTO 11000
        IF(OPT.EQ.13) GOTO 12000
        IF(OPT.EQ.14) GOTO 13000
        IF(OPT.EQ.15) GOTO 14000
        IF(OPT.EQ.16) GOTO 15000
        IF(OPT.EQ.17) GOTO 16000
        IF(OPT.EQ.18) GOTO 17000
        IF(OPT.EQ.19) GOTO 18000
        IF(OPT.EQ.20) GOTO 19000
        IF(OPT.EQ.21) GOTO 20000
        IF(OPT.EQ.22) GOTO 21000
        GOTO 10
C
C CHANGE GLOBAL VOLUME NAME
C
500     CONTINUE
        CALL CHNGVOL
        SCANED = .FALSE.                !Force rescanning of disk file sizes.
        GOTO 10

C
C CHANGE A GLOBAL SYSTEM PARAMETER
C
1000    CONTINUE
        TOLINE = FRMLIN - 1
        IF(TOLINE.LT.0) TOLINE = 0
1005    CONTINUE
        DO I=1,DSPPAR_EVEN
            IF(PNUM(I).EQ.MAXTRP.OR.PNUM(I).EQ.MAXSSC.OR.
     *         PNUM(I).EQ.MAXTRPS.OR.PNUM(I).EQ.MAXSSCS.OR.
     *         PNUM(I).EQ.MAXDBL.OR.PNUM(I).EQ.MAXCPL.OR.
     *         PNUM(I).EQ.MAXWIT.OR.
     *         PNUM(I).EQ.MAXSCRS.OR.PNUM(I).EQ.MAXCPLS.OR.
     *         PNUM(I).EQ.MAXDBLS.OR.PNUM(I).EQ.MAXSTAKE.OR.
     *         PNUM(I).EQ.MAXSTR.OR.PNUM(I).EQ.MAXSTRS.OR.
     *         PNUM(I).EQ.MAXSCR.OR.PNUM(I).EQ.OGQLIM.OR.
     *	       PNUM(I).EQ.MINSPT.OR.PNUM(I).EQ.MAXTGL.OR.PNUM(I).EQ.MINTGL)THEN
                 SCFPAR(PNUM(I)) = SCFPAR(PNUM(I))*DYN_BETUNIT/DOLL_BASE
            ENDIF
        ENDDO
        CALL CLRSCR(6)
        FRMLIN = TOLINE + 1
        TOLINE = TOLINE + (MAXROW*2)
        DO 1010 I = FRMLIN,TOLINE,2 
           IF(I.GT.DSPPAR) GOTO 1020
           IF(PNUM(I).LT.1) GOTO 1020
           MAX = I + 1
           IF(PNUM(I).EQ.ROMREV.OR.PNUM(I).EQ.WRNPER.OR.
     *        PNUM(I+1).EQ.ROMREV.OR.PNUM(I+1).EQ.WRNPER.OR.
     *        PNUM(I).EQ.GVTREV.OR.PNUM(I+1).EQ.GVTREV.OR.
     *        PNUM(I).EQ.EUROCR.OR.PNUM(I+1).EQ.REG_DRWPCK.OR.
     *        PNUM(I).EQ.REDIMN.OR.PNUM(I).EQ.REDDEF.OR.
     *        PNUM(I).EQ.ODD_DRWPCK) THEN
C
	    IF(PNUM(I).EQ.ODD_DRWPCK.AND.I.EQ.DSPPAR) THEN
	      WRITE(6,90111) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I)
            ELSEIF(PNUM(I).EQ.ROMREV.OR.PNUM(I).EQ.GVTREV.OR.
     *         PNUM(I).EQ.ODD_DRWPCK) THEN
              WRITE(6,901) I,PNAME(I),SCFPAR(PNUM(I)),
     *        PDESCR(I),I+1,PNAME(I+1), SCFPAR(PNUM(I+1)),PDESCR(I+1)   
            ELSEIF(PNUM(I+1).EQ.ROMREV.OR.PNUM(I+1).EQ.GVTREV.OR.
     *         PNUM(I+1).EQ.REG_DRWPCK) THEN
              WRITE(6,9021) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *        I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
            ENDIF
C
            IF(PNUM(I).EQ.WRNPER) THEN
              WRITE(6,902) I,PNAME(I),DISPER(SCFPAR(PNUM(I))),PDESCR(I),
     *                     I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
            ELSEIF(PNUM(I+1).EQ.WRNPER) THEN
              WRITE(6,9022) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                      I+1,PNAME(I+1),DISPER(SCFPAR(PNUM(I+1))),
     *                      PDESCR(I+1)
            ENDIF
            IF(PNUM(I).EQ.EUROCR) THEN
              WRITE(6,90201) I,PNAME(I),DFLOAT(SCFPAR(PNUM(I)))/1.D3,
     *                     PDESCR(I),
     *                     I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
            ELSEIF(PNUM(I+1).EQ.EUROCR) THEN
              WRITE(6,90202) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                      I+1,PNAME(I+1),
     *                      DFLOAT(SCFPAR(PNUM(I+1)))/1.D3,
     *                      PDESCR(I+1)
            ELSEIF(PNUM(I).EQ.REDIMN) THEN
              WRITE(6,9030) I,PNAME(I),DFLOAT(SCFPAR(PNUM(I)))/1.D2,
     *                      PDESCR(I)(1:16),
     *                      I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
            ELSEIF(PNUM(I+1).EQ.REDIMN) THEN
              WRITE(6,9031) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                      I+1,PNAME(I+1),
     *                      DFLOAT(SCFPAR(PNUM(I+1)))/1.D2,
     *                      PDESCR(I+1)
            ELSEIF(PNUM(I).EQ.REDDEF) THEN
              WRITE(6,9030) I,PNAME(I),DFLOAT(SCFPAR(PNUM(I)))/1.D2,
     *                     PDESCR(I),
     *                     I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
            ELSEIF(PNUM(I+1).EQ.REDDEF) THEN
              WRITE(6,9031) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                      I+1,PNAME(I+1),
     *                      DFLOAT(SCFPAR(PNUM(I+1)))/1.D2,
     *                      PDESCR(I+1)
            ENDIF
C
           ELSEIF(PNUM(I).EQ.SUPTSP.OR.PNUM(I).EQ.TSTLIM) THEN
            WRITE(6,9023) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                    I+1,PNAME(I+1),
     *                    SCFPAR(PNUM(I+1))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.TSLWRN.OR.PNUM(I).EQ.TSLMAX) THEN
            WRITE(6,9024) I,PNAME(I),
     *                    SCFPAR(PNUM(I))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I),
     *                    I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.TSLIAB) THEN
            WRITE(6,9025) I,PNAME(I),
     *                    SCFPAR(PNUM(I))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I),
     *                    I+1,PNAME(I+1),
     *                    SCFPAR(PNUM(I+1))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.TSPMIN) THEN
            WRITE(6,9026) I,PNAME(I),
     *                    SCFPAR(PNUM(I))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I),
     *                    I+1,PNAME(I+1),SCFPAR(PNUM(I+1))/10,
     *                    MOD(SCFPAR(PNUM(I+1)),10),PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.TSPPER) THEN
            WRITE(6,9027) I,PNAME(I),SCFPAR(PNUM(I)),
     *                    MOD(SCFPAR(PNUM(I+1)),10),PDESCR(I),
     *                    I+1,PNAME(I+1),
     *                    SCFPAR(PNUM(I+1))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.TSMXLI) THEN
            WRITE(6,9029) I,PNAME(I),
     *                    SCFPAR(PNUM(I)),
     *                    PDESCR(I),
     *                    I+1,PNAME(I+1),
     *                    SCFPAR(PNUM(I+1)),
     *                    PDESCR(I+1)
           ELSEIF(PNUM(I).EQ.MINKJCK.OR.PNUM(I).EQ.MAXKJCK.OR.
     *            PNUM(I).EQ.MINSJCK.OR.PNUM(I).EQ.MAXSJCK) THEN
            WRITE(6,90241) I,PNAME(I),
     *                    SCFPAR(PNUM(I))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I),
     *                    I+1,PNAME(I+1),
     *                    SCFPAR(PNUM(I+1))*DYN_BETUNIT/DOLL_BASE,
     *                    PDESCR(I+1)
           ELSE
             IF(PNUM(I).GT.0.AND.PNUM(I+1).GT.0) THEN
                WRITE(6,9011) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I),
     *                    I+1,PNAME(I+1),SCFPAR(PNUM(I+1)),PDESCR(I+1)
             ELSE
                WRITE(6,9011) I,PNAME(I),SCFPAR(PNUM(I)),PDESCR(I)
             ENDIF
           ENDIF
1010    CONTINUE
C
        DO I=1,DSPPAR_EVEN
            IF(PNUM(I).EQ.MAXTRP.OR.PNUM(I).EQ.MAXSSC.OR.
     *         PNUM(I).EQ.MAXTRPS.OR.PNUM(I).EQ.MAXSSCS.OR.
     *         PNUM(I).EQ.MAXDBL.OR.PNUM(I).EQ.MAXCPL.OR.
     *         PNUM(I).EQ.MAXWIT.OR.
     *         PNUM(I).EQ.MAXSCRS.OR.PNUM(I).EQ.MAXCPLS.OR.
     *         PNUM(I).EQ.MAXSTR.OR.PNUM(I).EQ.MAXSTRS.OR.
     *         PNUM(I).EQ.MAXDBLS.OR.PNUM(I).EQ.MAXSTAKE.OR.
     *         PNUM(I).EQ.MAXSCR.OR.PNUM(I).EQ.OGQLIM.OR.
     *         PNUM(I).EQ.MINSPT.OR.PNUM(I).EQ.MAXTGL.OR.
     *         PNUM(I).EQ.MINTGL) THEN
               SCFPAR(PNUM(I)) = SCFPAR(PNUM(I))*DOLL_BASE/DYN_BETUNIT
            ENDIF
        ENDDO
        CALL INPNUM('Enter number to change, M-more, E-exit ',
     *              NUM,1,MAX,EXT)
        IF(EXT.EQ.0) GOTO 1025          !Get entry.
        IF(EXT.EQ.-1) GOTO  10          !EXIT
        GOTO 1005                       !Continue Listing.                      
C
C
1020    CONTINUE
        DO I=1,DSPPAR_EVEN
            IF(PNUM(I).EQ.MAXTRP.OR.PNUM(I).EQ.MAXSSC.OR.
     *         PNUM(I).EQ.MAXTRPS.OR.PNUM(I).EQ.MAXSSCS.OR.
     *         PNUM(I).EQ.MAXDBL.OR.PNUM(I).EQ.MAXCPL.OR.
     *         PNUM(I).EQ.MAXWIT.OR.
     *         PNUM(I).EQ.MAXSCRS.OR.PNUM(I).EQ.MAXCPLS.OR.
     *         PNUM(I).EQ.MAXSTR.OR.PNUM(I).EQ.MAXSTRS.OR.
     *         PNUM(I).EQ.MAXDBLS.OR.PNUM(I).EQ.MAXSTAKE.OR.
     *         PNUM(I).EQ.MAXSCR.OR.PNUM(I).EQ.OGQLIM.OR.
     *         PNUM(I).EQ.MINSPT.OR.PNUM(I).EQ.MAXTGL.OR.
     *         PNUM(I).EQ.MINTGL) THEN
                 SCFPAR(PNUM(I)) = SCFPAR(PNUM(I))*DOLL_BASE/DYN_BETUNIT
            ENDIF
        ENDDO
        CALL INPNUM('Enter number to change, E-exit ',NUM,1,MAX,EXT)
        IF(EXT.LT.0) GOTO 10
1025    CONTINUE
        IF(PNUM(NUM).EQ.ROMREV) THEN
          CALL WIMG(6,'Enter new value ')
          READ(5,903) VAL
        ELSEIF(PNUM(NUM).EQ.GVTREV) THEN
          CALL WIMG(6,'Enter new value ')
          READ(5,903) VAL
        ELSEIF(PNUM(NUM).EQ.REG_DRWPCK.OR.
     *         PNUM(NUM).EQ.ODD_DRWPCK) THEN
          CALL WIMG(6,'Enter new location ')
          READ(5,903) VAL
        ELSEIF(PNUM(NUM).EQ.WRNPER) THEN
          CALL INPPER('Enter new value ',VAL,EXT)
          IF(EXT.LT.0) GOTO 1000
        ELSE
          I=PNUM(NUM)
          IF(I.EQ.TSPMIN.OR.I.EQ.TSLIAB.OR.I.EQ.TSLWRN.OR.
     *       I.EQ.TSLMAX) THEN
             WRITE(6,9028) IAM()
          ENDIF
          CALL INPNUM('Enter new value ',VAL,PMIN(NUM),PMAX(NUM),EXT)
          IF(EXT.LT.0) GOTO 1000
        ENDIF
        IF(PNUM(NUM).EQ.MAXTRP.OR.PNUM(NUM).EQ.MAXSSC.OR.
     *     PNUM(NUM).EQ.MAXTRPS.OR.PNUM(NUM).EQ.MAXSSCS.OR.
     *     PNUM(NUM).EQ.MAXDBL.OR.PNUM(NUM).EQ.MAXCPL.OR.
     *     PNUM(NUM).EQ.MAXKJCK.OR.PNUM(NUM).EQ.MINKJCK.OR.
     *     PNUM(NUM).EQ.MAXWIT.OR.
     *     PNUM(NUM).EQ.MAXSCRS.OR.PNUM(NUM).EQ.MAXCPLS.OR.
     *     PNUM(NUM).EQ.MAXSTR.OR.PNUM(NUM).EQ.MAXSTRS.OR.
     *     PNUM(NUM).EQ.MAXDBLS.OR.PNUM(NUM).EQ.MAXSTAKE.OR.
     *     PNUM(NUM).EQ.MAXSCR.OR.PNUM(NUM).EQ.OGQLIM.OR.
     *     PNUM(NUM).EQ.MAXSJCK.OR.PNUM(NUM).EQ.MINSJCK.OR.
     *	   PNUM(NUM).EQ.MINSPT.OR.PNUM(NUM).EQ.MAXTGL.OR.
     *     PNUM(NUM).EQ.MINTGL) THEN
           VAL = (VAL*DOLL_BASE)/DYN_BETUNIT
        ENDIF
        SCFPAR(PNUM(NUM))=VAL
        IF(PNUM(NUM) .EQ. REDDEF) THEN        !Reddef is euros
          DO K = 1, MAXGAM                 
            GT = SCFGNT(GAMTYP,K)
            IF (GT.NE.TPAS) THEN     !DO NOT USE GLOBAL REDDEF FOR PASSIVE. CONFIGURE IT ON CHANGE GAME OPTION
               SCFRED(K) = SCFPAR(PNUM(NUM)) 
            ENDIF
          ENDDO
        ENDIF
        IF(PNUM(NUM) .EQ. REDIMN) THEN        !Redimn is in euros
          DO K = 1, MAXGAM            
            SCFRMI(K) = SCFPAR(PNUM(NUM))
          ENDDO
        ENDIF
        GOTO 1000
C
C CHANGE SYSTEM FILE SIZES/VOLUME NAMES
C
2000    CONTINUE
D       CALL LIB$INIT_TIMER()
        IF(.NOT.SCANED) THEN
           WRITE(6,917)
           SCANED = .TRUE.
           MAX = 0
           DO 2010 I=1,MAXFIL
              FILSIZ(I) = 0
              EXFILC(I) = NOP_CHR
              IF(SCFSFN(1,I).EQ.0) CALL FASTSET(BLANK,SCFSFN(1,I),5)
              IF(SFILES(I).EQ.'               '.AND.MAX.EQ.0) MAX = I-1
              WRITE(FILNAM,'(20A1)') (FNAMES(J,I),J=1,20)
CCCC          CALL CHKNAM(FILNAM,ST)
              IF(ST.EQ.0) THEN
                 INQUIRE(FILE=FILNAM,EXIST=EXFIL)
                 IF(EXFIL) THEN
CCCC                INQUIRE(FILE=FILNAM,RECL=FILSIZ(I))
                    CALL OPENX(FTLUN,FILNAM,4,0,0,ST)
                    CALL VAXGETFSIZ(FTLUN,FILSIZ(I))
                    FILSIZ(I) = FILSIZ(I) + 1           !To get same as 'DIR'
                    CLOSE(FTLUN)
                    EXFILC(I) = YES_CHR 
                 ENDIF
              ENDIF
2010       CONTINUE
D          CALL LIB$SHOW_TIMER()
        ENDIF
        NUM = 0
        PAGE = 1
2015    CONTINUE
        SFIL = (PAGE-1) * 18 + 1
        EFIL = (SFIL + 17)
        IF(EFIL.GT.MAX) EFIL = MAX
        CALL CLRSCR(6)
        WRITE(6,904)
        DO 2020 I = SFIL,EFIL
        WRITE(6,905) I,SFILES(I),(FNAMES(K,I),K=1,4),EXFILC(I),
     *               (FNAMES(K,I),K=6,20),SCFFSZ(I),FILSIZ(I)
        IF(I.EQ.EFIL) THEN
          CALL INPNUM('Enter file number or More or Exit ',NUM,SFIL,
     *       EFIL,ERR)
          IF(ERR.EQ.-1) GOTO 10         !'E' Entered.
          IF(ERR.EQ.0) GOTO 2035        !File # Entered.
          PAGE = PAGE + 1
          IF(EFIL.EQ.MAX) PAGE = 1
          GOTO 2015
        ENDIF
2020    CONTINUE
C
C
2030    CONTINUE
        CALL INPNUM('Enter file number, E-exit ',NUM,1,MAX,EXT)
        IF(EXT.LT.0) GOTO 10
2035    CONTINUE
        CALL WIMG(6,
     *       'Enter disk volume name (RETURN no change) ')
        READ(5,906) (TMPNAM(K),K=1,4)
        ITMP = 4
        IF(TMPNAM(1).NE.' ') 
     *      CALL LIB$MOVC3(ITMP,%REF(TMPNAM(1)),%REF(FNAMES(1,NUM)))
        FNAMES(5,NUM)=':'
C
        CALL WIMG(6,'Enter file name (RETURN no change) ')
        READ(5,907) (TMPNAM(K),K=6,20) 
        ITMP = 14
        IF(TMPNAM(6).NE.' ') 
     *      CALL LIB$MOVC3(ITMP,%REF(TMPNAM(6)),%REF(FNAMES(6,NUM)))
C
        CALL INPNUM('Enter file size (E-no change, ?-Disk size) ',
     *              TMPNUM,0,9999999,EXT)
        IF(EXT.EQ.-1) GOTO 2040             !EXIT Entered.
        IF(EXT.EQ.-11.AND.FILSIZ(NUM).GT.0) SCFFSZ(NUM) = FILSIZ(NUM) !S Enterd
        IF(EXT.EQ.0.AND.TMPNUM.NE.SCFFSZ(NUM)) SCFFSZ(NUM) = TMPNUM
C
2040    CONTINUE
        WRITE(FILNAM,'(20A1)') (FNAMES(J,NUM),J=1,20)
        ST = 0  !!!!!!
CCCCCCC CALL CHKNAM(FILNAM,ST)
        IF(ST.EQ.0) THEN
           EXFIL = .FALSE.
           INQUIRE(FILE=FILNAM,EXIST=EXFIL)
           IF(EXFIL) THEN
CCCC          INQUIRE(FILE=FILNAM,RECL=FILSIZ(NUM))
              CALL OPENX(FTLUN,FILNAM,4,0,0,ST)
              CALL VAXGETFSIZ(FTLUN,FILSIZ(NUM))
              FILSIZ(NUM) = FILSIZ(NUM) + 1         !To get same as 'DIR'
              CLOSE(FTLUN)
              EXFILC(NUM) = YES_CHR 
           ELSE
              EXFILC(NUM) = NOP_CHR 
              FILSIZ(NUM) = 0
           ENDIF
        ELSE
           TYPE*,IAM(),' NONE VALID FILE NAME ENTERED....-->',FILNAM,'<--'
           CALL XWAIT(2,2,ST)
        ENDIF
        GOTO 2015
C
C DEFINE/CHANGE A GAME
C
3000    CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter game number ',GAM,1,MAXGAM,EXT)
        IF(EXT.LT.0) GOTO 10
3010    CONTINUE
        GT=SCFGNT(GAMTYP,GAM)
        GI=SCFGNT(GAMIDX,GAM)
        SIND = 0
        IF(GT.EQ.TSPT) THEN
           SIND = SCFSTP(GI)
        ENDIF
        KN=SCFKGN(GAM)
        CALL CLRSCR(6)
        WRITE(6,908) GAM,GTNAMES(GT),STYP(SIND),GI,KN,
     *               (SCFLGN(K,GAM),K=1,4),SCFSGN(GAM),
     *               (GFNAMES(K,GAM),K=6,20),(GFNAMES(K,GAM),K=1,4),
     *               SCFGSZ(GAM),DISPER(SCFCOG(GAM)),
     *               CMONY(SCFRED(GAM),11,VALUNIT),
     *               CMONY(SCFRMI(GAM),9,VALUNIT),
     *               SCFPRG(GAM),SCFFRC(GAM),
     *               CMONY(SCF_HLDLIM(GAM,1),11,VALUNIT) ,SCF_HLDDAY(GAM,1),
     *               CMONY(SCF_HLDLIM(GAM,2),11,VALUNIT) ,SCF_HLDDAY(GAM,2),
     *               CMONY(SCF_HLDLIM(GAM,3),11,VALUNIT) ,SCF_HLDDAY(GAM,3)
    
        LIM=19
        IF(GT.EQ.TSCR.OR.GT.EQ.TWIT.OR.GT.EQ.TTSL.OR.
     *     GT.EQ.TDBL.OR.GT.EQ.TCPL.OR.GT.EQ.TSSC.OR.
     *     GT.EQ.TTRP.OR.GT.EQ.TSTR) THEN
           WRITE(6,9081) (GVNAMES(K,GAM),K=6,20),(GVNAMES(K,GAM),K=1,4),
     *                    SCFGVS(GAM)
           LIM=22
        ELSE IF(GT.EQ.TPAS) THEN
           WRITE(6,9082) DISPER(SCFRETCOM(GI))
           LIM=20
        ENDIF
C***    LIM=13
C***    IF(GT.EQ.TSCR.OR.GT.EQ.TWIT.OR.GT.EQ.TTSL) LIM=16
        CALL INPNUM('Enter number to change, E-exit',NUM,1,LIM,EXT)
        IF(EXT.LT.0) GOTO 10
        CALL CLRSCR(6)
C
C GAME TYPE/INDEX CHANGE
C
        IF(NUM.EQ.1.OR.NUM.EQ.2) THEN
          WRITE(6,9091)
          DO I = 1, MAXTYP
              IF(GTNAMES(I).NE.'        ') WRITE(6,909)I,GTNAMES(I)
          END DO
          CALL INPNUM('Enter game type number ',GTNUM,1,MAXTYP,EXT)
          IF(EXT.LT.0) GOTO 3010
          IF(GTNUM.EQ.TINS) THEN
            TYPE*,'Sorry, game type not currently available'
            CALL XWAIT(2,2,ST)
            GOTO 3010
          ENDIF
          CALL INPNUM('Enter game index ',GTIND,1,MAXIND,EXT)
          IF(EXT.LT.0) GOTO 3010
          IF(SCFGTN(GTNUM,GTIND).NE.0) THEN
            TYPE*,'Sorry, game type/index already assigned '
            CALL XWAIT(2,2,ST)
            GOTO 3010
          ENDIF
          IF(GT.NE.0.AND.GI.NE.0) SCFGTN(GT,GI)=0
          SCFGNT(GAMTYP,GAM)=GTNUM
          SCFGNT(GAMIDX,GAM)=GTIND
          SCFGTN(GTNUM,GTIND)=GAM
          IF(GTNUM.EQ.TKIK) SCFKGN(GAM)=GAM
          SIND=0
          IF(GTNUM.EQ.TSPT) THEN
            SIND=1
C            CALL INPNUM('Enter sports type [1=regular, 2=special] ',
C     *      SIND,1,2,EXT)
             SCFSTP(GTIND)=SIND  ! PORTUGAL: REGULAR TYPE
          ENDIF
          GOTO 3010
        ENDIF
C
C KICKER GAME NUMBER
C
        IF(NUM.EQ.3) THEN
          IF(SCFGNT(GAMTYP,GAM).EQ.TKIK.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TNBR.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TSCR.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TWIT.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TDBL.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TCPL.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TSSC.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TTRP.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TSTR.OR.
CCC     *      SCFGNT(GAMTYP,GAM).EQ.TTSL.OR.
     *      SCFGNT(GAMTYP,GAM).EQ.TINS) THEN
            TYPE*,'Sorry, can''t be changed '
            CALL XWAIT(2,2,ST)
            GOTO 3010
          ENDIF
          CNT=0
          DO 3020 I=1,MAXGAM
          IF(SCFGNT(GAMTYP,I).EQ.TKIK) THEN
            CNT=CNT+1
            WRITE(6,910) I,(SCFLGN(K,I),K=1,4)
          ENDIF
3020      CONTINUE
          IF(CNT.EQ.0) THEN
            TYPE*,'Sorry, no joker games currently defined'
            CALL XWAIT(2,2,EXT)
            GOTO 3010
          ENDIF
          CALL INPNUM('Enter joker game number: ',KNUM,0,MAXGAM,EXT)
          IF(EXT.LT.0) GOTO 3010
          IF(KNUM.EQ.0) THEN
            SCFKGN(GAM)=0    !REMOVED KICKER GAME
            GOTO 3010
          ENDIF
          IF(SCFGNT(GAMTYP,KNUM).NE.TKIK) THEN
            TYPE*,'Sorry, not a valid joker game number '
            CALL XWAIT(2,2,ST)
            GOTO 3010
          ENDIF
          SCFKGN(GAM)=KNUM
          GOTO 3010
        ENDIF
C
C GAME NAME
C
        IF(NUM.EQ.4) THEN
          CALL WIMG(6,'Enter long game name ')
          READ(5,911) (SCFLGN(K,GAM),K=1,4)
          GOTO 3010
        ENDIF
        IF(NUM.EQ.5) THEN
          CALL WIMG(6,'Enter short game name ' )
          READ(5,911) SCFSGN(GAM)
          GOTO 3010
        ENDIF
C
C FILE NAME
C
        IF(NUM.EQ.6) THEN
          CALL WIMG(6,'Enter game file name ')
          READ(5,907) (GFNAMES(K,GAM),K=6,20)
          GOTO 3010
        ENDIF
C
C FILE VOLUME
C
        IF(NUM.EQ.7) THEN
          CALL WIMG(6,
     *     'Enter disk volume name (RETURN for system volume) ')
          READ(5,906) (GFNAMES(K,GAM),K=1,4)
          GFNAMES(5,GAM)=':'
          GOTO 3010
        ENDIF
C
C FILE SIZE
C
        IF(NUM.EQ.8) THEN
          CALL INPNUM('Enter game file size ',FSIZE,1,99999,EXT)
          IF(EXT.EQ.0) SCFGSZ(GAM)=FSIZE
          GOTO 3010
        ENDIF
C
C SALES COMMISSION RATE
C
        IF(NUM.EQ.9) THEN
          CALL INPPER('Enter sales commission rate ',COM,EXT)
          IF(EXT.EQ.0) SCFCOG(GAM)=COM
          GOTO 3010
        ENDIF
C
C REDMAX CHANGE
C
        IF (NUM.EQ.10) THEN
           IF (GT.EQ.TPAS) THEN    !PASSIVE USES IT'S OWN REDMAX (NOT GLOBAL REDMAX)
              CALL INPMONY('Enter redmax for this game ',RDMX,VALUNIT,EXT)
              IF(EXT.EQ.0) SCFRED(GAM)=RDMX
           ELSE
              TYPE*,IAM(),'Change of redemption max for all games is in option 1'
              CALL XWAIT(2,2,ST)
           ENDIF
           GOTO 3010
        ENDIF

C
C REDMIN CHANGE
C
        IF(NUM.EQ.11) THEN
CRXK      CALL INPMONY('Enter redmin for this game ',RDMIN,VALUNIT,EXT)
CRXK      IF(EXT.EQ.0) SCFRMI(GAM)=RDMIN
        TYPE*,IAM(),'Change of redemption min for all games is in option 1'
        CALL XWAIT(2,2,ST)
        GOTO 3010
        ENDIF

C
C CASH DAY CHANGE
C
        IF(NUM.EQ.12) THEN
          CALL INPNUM('Enter maximum # of days to cash ',PRMX,1,400,EXT)
          IF(EXT.EQ.0) SCFPRG(GAM)=PRMX
          GOTO 3010
        ENDIF
C
C MAXIMUM FRACTIONS
C
        IF(NUM.EQ.13) THEN
           CALL INPNUM('Enter maximum # of fractions ',FRMX,0,10,EXT)
           IF(EXT.EQ.0) SCFFRC(GAM)=FRMX
           GOTO 3010
        ENDIF
C
C LIMIT 1
C
        IF(NUM.EQ.14) THEN
           CALL INPMONY('Enter Limit 1',HLDLIM(1),VALUNIT,EXT)
           IF(EXT.EQ.0) SCF_HLDLIM(GAM,1)=HLDLIM(1)
           GOTO 3010
        ENDIF
C
C DAYS 1
C
        IF(NUM.EQ.15) THEN
          CALL INPNUM('Enter # Of holding days for limit 1 ',HLDDAY(1),0,365,EXT)
          IF(EXT.EQ.0) SCF_HLDDAY(GAM,1)=HLDDAY(1)
          GOTO 3010
        ENDIF
C
C LIMIT 2
C
        IF(NUM.EQ.16) THEN
           CALL INPMONY('Enter Limit 2',HLDLIM(2),VALUNIT,EXT)
           IF(EXT.EQ.0) SCF_HLDLIM(GAM,2)=HLDLIM(2)
           GOTO 3010
        ENDIF
C
C DAYS 2
C
        IF(NUM.EQ.17) THEN
          CALL INPNUM('Enter # Of holding days for limit 2 ',HLDDAY(2),0,365,EXT)
          IF(EXT.EQ.0) SCF_HLDDAY(GAM,2)=HLDDAY(2)
          GOTO 3010
        ENDIF
C
C LIMIT 3
C
        IF(NUM.EQ.18) THEN
           CALL INPMONY('Enter Limit 3',HLDLIM(3),VALUNIT,EXT)
           IF(EXT.EQ.0) SCF_HLDLIM(GAM,3)=HLDLIM(3)
           GOTO 3010
        ENDIF
C
C DAYS 3
C
        IF(NUM.EQ.19) THEN
          CALL INPNUM('Enter # Of holding days for limit 3 ',HLDDAY(3),0,365,EXT)
          IF(EXT.EQ.0) SCF_HLDDAY(GAM,3)=HLDDAY(3)
          GOTO 3010
        ENDIF
C+++++++DEPENDING ON GAME
        IF(NUM.EQ.20) THEN
           IF(GT.EQ.TPAS) THEN
C
C SALES COMMISSION RATE
C
              CALL INPPER('Enter returns commission rate ',COM,EXT)
              IF(EXT.EQ.0) SCFRETCOM(GI)=COM
              GOTO 3010
           ELSE
c
C VERIFY FILE NAME
C
             CALL WIMG(6,'Enter game verify file name ')
             READ(5,907) (GVNAMES(K,GAM),K=6,20)
             GOTO 3010
           ENDIF
        ENDIF
C
C VERFIY FILE VOLUME
C
        IF(NUM.EQ.21) THEN
           CALL WIMG(6,
     *     'Enter verify disk volume name (RETURN for system volume) ')
           READ(5,906) (GVNAMES(K,GAM),K=1,4)
           GVNAMES(5,GAM)=':'
           GOTO 3010
        ENDIF
C
C VERIFY FILE SIZE
C
        IF(NUM.EQ.22) THEN
           CALL INPNUM('Enter game verify file size ',VSIZE,1,99999,EXT)
           IF(EXT.EQ.0) SCFGVS(GAM)=VSIZE
           GOTO 3010
        ENDIF
        GOTO 3010
C
C DELETE A GAME
C
4000    CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter game number to delete, E-exit ',GNUM,
     *              1,40,EXT)
        IF(EXT.LT.0) GOTO 10
        GT=SCFGNT(GAMTYP,GNUM)
        GI=SCFGNT(GAMIDX,GNUM)
        IF(GT.LT.1) THEN
          TYPE*,'Sorry, game ',GNUM,' not configured'
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
        SCFGNT(GAMTYP,GNUM)=0
        SCFGNT(GAMIDX,GNUM)=0
        SCFGTN(GT,GI)=0
        SCFKGN(GNUM)=0
        SCFCOG(GNUM)=0
        CALL FASTSET(BLANK,SCFLGN(1,GNUM),4)
        SCFSGN(GNUM)=BLANK
        CALL FASTSET(BLANK,SCFGFN(1,GNUM),5)
        SCFGSZ(GNUM)=0
        TYPE*,'Game number ',GNUM,' deleted'
        CALL XWAIT(2,2,ST)
        GOTO 10
C
C DISPLAY EXISTING GAMES
C
5000    CONTINUE
        CALL CLRSCR(6)
        WRITE(6,912)
        CNT=0
        DO 5010 I=1,MAXGAM
        IF(SCFGNT(GAMTYP,I).EQ.0) GOTO 5010
        CNT=CNT+1
        GT=SCFGNT(GAMTYP,I)
        GI=SCFGNT(GAMIDX,I)
        WRITE(6,913) I,GTNAMES(GT),GI,(SCFLGN(K,I),K=1,4),
     *               (GFNAMES(K,I),K=6,20),
     *               (GFNAMES(K,I),K=1,4),
     *               SCFGSZ(I),SCFKGN(I)
5010    CONTINUE
        IF(CNT.EQ.0) TYPE*,'Sorry, no games currently configured'
        CALL WIMG(6,'Hit return to continue')
        READ(5,903) CONT
        GOTO 10
C
C CHANGE TICKET CHARGE
C
6000    CONTINUE
        CALL CLRSCR(6)
C        IF(NOTKC.NE.0) THEN
C          TYPE*,'Sorry, function not available on this system'
C          CALL XWAIT(2,2,ST)
C          GOTO 10
C        ENDIF
        DO 6010 I=1,MAXGAM
        IF(SCFGNT(1,I).EQ.0) GOTO 6010
        WRITE(6,915) I,(SCFLGN(K,I),K=1,4),CMONY(SCFTKC(I),5,BETUNIT)
6010    CONTINUE
C
C
        CALL INPNUM('Enter game number (E-exit) ',GAM,1,MAXGAM,EXT)
        IF(EXT.LT.0) GOTO 10
        IF(SCFGNT(1,GAM).EQ.0) THEN
          TYPE*,'Game ',GAM,' not currently active '
          CALL XWAIT(2,2,ST)
          GOTO 6000
        ENDIF
C
C
        CALL INPMONY('Enter new ticket charge ',TC,BETUNIT,EXT)
        IF(EXT.LT.0) GOTO 6000
        SCFTKC(GAM)=TC
        GOTO 6000

C
C SET LOTTO SYSTEM BET DEFINITIONS
C
C
6500    CONTINUE
        CALL CLRSCR(5)
        CALL FASTMOV(SCFSFN(1,LSF),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL LSYS(FILE)
        GOTO 10

C
C SET LOTTO GAME PARAMETERS
C
7000    CONTINUE
        CALL CLRSCR(6)
        ! V1.7
        CALL PRMNUM('Enter LOTTO game index ',GIND,1,NUMLTO,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM=SCFGTN(TLTO,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
              TYPE*,'Sorry, Lotto ',GIND,' not active '
              CALL XWAIT(2,2,ST)
              GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL BLDLTO(FILE,SCFLGN(1,GNUM),GIND)
        GOTO 10
C
C SET SPORTS SYSTEMS
C
8000    CONTINUE
        CALL CLRSCR(6)
        CALL FASTMOV(SCFSFN(1,SSF),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL SPTSYS(FILE)
        GOTO 10
C
C SET SPORTS GAME PARAMETERS
C
9000    CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter sports index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM=SCFGTN(TSPT,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE*,'Sorry, selected game not active'
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL BLDSPT(FILE,SCFLGN(1,GNUM))
        GOTO 10
C
C ANALYSE SPORTS SYSTEMS
C
10000   CONTINUE
        CALL CLRSCR(6)
        CALL FASTMOV(SCFSFN(1,SSF),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL SPTANL(FILE)
        GOTO 10
C
C SET KICKER GAME PARAMETERS
C
11000   CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter joker index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM=SCFGTN(TKIK,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE*,'Sorry, selected game not active'
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL BLDKIK(FILE,SCFLGN(1,GNUM))
        GOTO 10
C
C DISPLAY ALTURA LOAD DEFINITIONS
C
12000   CONTINUE
        CALL CLRSCR(6)
        CALL SPCDLL(AUXDUMMY)
        GOTO 10
C
C SET GLOBAL COMMISSION/TAX RATES
C
13000   CONTINUE
        CALL CLRSCR(6)
        WRITE(6,916) DISPER(SCFCOT(TVAL)),CMONY(SCFCOT(TRET),11,BETUNIT),
     *               DISPER(SCFHVR),CMONY(SCFHVL,11,VALUNIT),
     *               DISPER(SCFTAR),CMONY(SCFTAL,11,VALUNIT),
     *               DISPER(SCFCTX)
        CALL INPPER('Enter validation commission rate ',COM,EXT)
        IF(EXT.EQ.0) SCFCOT(TVAL)=COM
        CALL INPMONY('Enter claim commission (amt per returns) ',COM,
     *               BETUNIT,EXT)
        IF(EXT.EQ.0) SCFCOT(TRET)=COM
        CALL INPPER('Enter high tier winner commission rate ',COM,EXT)
        IF(EXT.EQ.0) THEN
          SCFHVR=COM
          CALL INPMONY('Enter high tier winner commission prize level ',
     *                 COM,VALUNIT,EXT)
          IF(EXT.EQ.0) SCFHVL=COM
        ENDIF
        CALL INPPER('Enter high tier winner tax rate ',COM,EXT)
        IF(EXT.EQ.0) THEN
          SCFTAR=COM
          CALL INPMONY('Enter high tier winner tax prize level ',
     *                 COM,VALUNIT,EXT)
          IF(EXT.EQ.0) SCFTAL=COM
        ENDIF
        CALL INPPER('Enter commission tax rate ',COM,EXT)
        IF(EXT.EQ.0) SCFCTX=COM
        GOTO 10
C
C SET NUMBERS GAME PARAMETERS
C
14000   CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter numbers game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM=SCFGTN(TNBR,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE*,'Sorry, Numbers ',GIND,' not active '
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL BLDNBR(FILE,SCFLGN(1,GNUM))
        GOTO 10
C
C CHANGE ODDSET GAME PARAMETERS
C
15000   CONTINUE
        CALL CLRSCR(6)
        TYPE*,'< ODDSET Game Parameter Change Menu >'
        TYPE*,' '
        DO I = 1,MAXTYP
           IF(I.EQ.TTSL.OR.I.EQ.TWIT.OR.I.EQ.TSCR.OR.
     *        I.EQ.TDBL.OR.I.EQ.TCPL.OR.I.EQ.TSSC.OR.
     *        I.EQ.TTRP.OR.I.EQ.TSTR)
     *        WRITE(6,909) I,GTNAMES(I)
        END DO
        CALL INPNUM('Enter Game Type number ',GTNUM,1,MAXTYP,EXT)
        IF(EXT.LT.0) GOTO 10
        IF(GTNUM.NE.TTSL.AND.GTNUM.NE.TWIT.AND.GTNUM.NE.TSCR.AND.
     *     GTNUM.NE.TDBL.AND.GTNUM.NE.TCPL.AND.
     *     GTNUM.NE.TSSC.AND.GTNUM.NE.TTRP.AND.
     *     GTNUM.NE.TSTR) THEN
           TYPE*,'Pick one from the displayed list (ODDSET)'
           CALL XWAIT(2,2,ST)
           GOTO 15000
        ENDIF
        CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM=SCFGTN(GTNUM,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE*,'Sorry, game index ',GIND,' not active '
          CALL XWAIT(2,2,ST)
          GOTO 15000
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        IF(GTNUM.EQ.TWIT) CALL BLDWIN(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TSCR) CALL BLDSCR(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TTSL) CALL BLDTSL(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TDBL) CALL BLDDBL(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TCPL) CALL BLDCPL(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TSSC) CALL BLDSSC(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TTRP) CALL BLDTRP(FILE,SCFLGN(1,GNUM))
        IF(GTNUM.EQ.TSTR) CALL BLDSTR(FILE,SCFLGN(1,GNUM))
        GOTO 10
C
C CHANGE MISCELLANEOUS PARAMETERS
C
16000   CONTINUE
        CALL CLRSCR(6)
        CALL BLDMIS
        GOTO 10
C
C CHANGE BINGO GAME PARAMETERS
C
17000   CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter BINGO game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
        GNUM = SCFGTN(TBNG,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE*,'Sorry, BINGO ',GIND,' not active '
          CALL XWAIT(2,2,ST)
          GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM),FILE,5)
        IF(FILE(1).EQ.BLANK) CALL SYSVOL(FILE)
        CALL BLDBNG(FILE,SCFLGN(1,GNUM))
        GOTO 10
C
C TCPASST IP ADDRESS PARAMETERS ARE HANDLED HERE
C
18000   CONTINUE
        CALL FASTMOV(SCF_TCPPORTS,TCP_TEMP(1),4)
        CALL FASTMOV(SCF_TCPPREFIX,TCP_TEMP(5),2)
        CALL FASTMOV(SCF_TCPSUFFIX,TCP_TEMP(7),8)
20100   CONTINUE
        CALL CLRSCR(6)
        WRITE(6,952) (TCP_TEMP(K),K=1,14)
        CALL INPNUM('Enter the number to change [E - No Change]',
     *              TCP_INDEX,1,15,EXT)
        IF(EXT.LT.0) THEN
          WRITE(6,953)
          CALL YESNO(ANS)
          IF(ANS.EQ.1) GOTO 10
          GOTO 20100
        ENDIF
        IF(TCP_INDEX.EQ.15) THEN
          CALL FASTMOV(TCP_TEMP(1),SCF_TCPPORTS,4)
          CALL FASTMOV(TCP_TEMP(5),SCF_TCPPREFIX,2)
          CALL FASTMOV(TCP_TEMP(7),SCF_TCPSUFFIX,8)
          GOTO 10
        ENDIF
        CALL INPNUM('Enter the new value [E - No Change]',
     *              TCP_TEMP(TCP_INDEX),0,TCP_LIMITS(TCP_INDEX),EXT)
c...    IF(EXT.LT.0) GOTO 10
        GOTO 20100
C
C SET RESULTS SYSTEN BET DEFINITIONS
C
19000   CONTINUE
        CALL CLRSCR(6)
	CALL INPNUM('Enter game index',GIND,1,NUMTGL,EXT)
	IF(EXT.NE.0) GOTO 10
        CALL FASTMOV(SCFSFN(1,TGSF), FILE, 5)
        IF(FILE(1) .EQ. BLANK) CALL SYSVOL(FILE)
        CALL TGLSYS(FILE,GIND)
        GOTO 10
C
C SET RESULTS GAME PARAMETERS
C
20000   CONTINUE
        CALL CLRSCR(6)
        CALL INPNUM('Enter results index ', GIND, 1, MAXIND, EXT)
        IF(EXT .LT. 0) GOTO 10
        GNUM = SCFGTN(TTGL, GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
          TYPE *, ' '
          TYPE *, 'Sorry, Selected Game Not Active'
          TYPE *, ' '
          CALL XWAIT(2, 2, ST)
          GOTO 10
        ENDIF
        CALL FASTMOV(SCFGFN(1,GNUM), FILE, 5)
        IF(FILE(1) .EQ. BLANK) CALL SYSVOL(FILE)
        CALL BLDTGL(FILE, SCFLGN(1,GNUM))
        GOTO 10
C
C SET RESULTS SYSTEN BET DEFINITIONS
C
21000   CONTINUE
        CALL CLRSCR(6)
	CALL INPNUM('Enter game index',GIND,1,NUMTGL,EXT)
	IF(EXT.NE.0) GOTO 10
        CALL FASTMOV(SCFSFN(1,TGSF), FILE, 5)
        IF(FILE(1) .EQ. BLANK) CALL SYSVOL(FILE)
        CALL TGLANL(FILE,GIND)
        GOTO 10
C
C BLDSYS EXIT with saving changes.
C
31000   CONTINUE
        CALL CLRSCR(6)
        TYPE*,'Posting changes to SCF.FIL '
        CALL WRITEW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) THEN
          TYPE*,'SCF.FIL write error > ',ST
          CALL GPAUSE
        ENDIF
        CALL CLOSEFIL(FDB)
        CALL GSTOP(GEXIT_SUCCESS)
C
C BLDSYS QUIT without saving SCF.FIL
C
32000   CONTINUE
        CALL CLRSCR(6)
        CALL CLOSEFIL(FDB)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
900    FORMAT(' BLDSYS options: ',//,
     * '  0 - Change a global volume name',
     *                        9X, '20 - Set results system bet definitions',/,
     * '  1 - Change a global system parameter',   
     *                        4X, '21 - Set RESULTS game parameters ',/,
     * '  2 - Change system file sizes/volumes',
     *                        4X, '22 - Analyze results system bets',/,
     * '  3 - Define/change a game ',
     *                       14X,'  E - Exit with saving SCF.FIL',/,
     * '  4 - Delete an existing game',
     *                       12X,'  Q - Quit without saving SCF.FIL',/,
     * '  5 - Display existing games',/,
     * '  6 - Change ticket charge amounts',/,
     * '  7 - Set LOTTO System Bets ',/,
     * '  8 - Set LOTTO game parameters ',/,
     * '  9 - Set sports system bet definitions',/,
     * ' 10 - Set SPORTS game parameters ',/,
     * ' 11 - Analyze sports system bets ',/,
     * ' 12 - Set JOKERI game parameters ',/,
     * ' 13 - Display Altura LOAD definitions ',/,
     * ' 14 - Set global commission/tax rates ',/,
     * ' 15 - Set NUMBERS game parameters',/,
     * ' 16 - Set ODDSET game parameters',/,
     * ' 17 - Set Miscellaneous parameters',/,
     * ' 18 - Set BINGO game parameters',/,
     * ' 19 - Set TCPASST IP address  parameters ',/)
C
9011    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A7,I7,1X,A20)
901     FORMAT(1X,I3.3,'>',A8,2X,A4,1X,A20,2X,I3.3,'>',A8,I6,1X,A20)
90111   FORMAT(1X,I3.3,'>',A8,2X,A4,1X,A20)
9021    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A8,2X,A4,1X,A20)
902     FORMAT(1X,I3.3,'>',A8,F6.2,1X,A20,2X,I3.3,'>',A8,I6,1X,A20)
90201   FORMAT(1X,I3.3,'>',A7,F7.3,1X,A20,2X,I3.3,'>',A8,I6,1X,A20)
90202   FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A7,F7.3,1X,A20)
9022    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A8,F6.2,1X,A20)
9023    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A8,I6,1X,A20)
9024    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A8,I6,1X,A20)
90241   FORMAT(1X,I3.3,'>',A6,I8,1X,A20,2X,I3.3,'>',A6,I8,1X,A20)
9025    FORMAT(1X,I3.3,'>',A8,I7,1X,A20,1X,I3.3,'>',A8,I6,1X,A20)
9026    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A8,
     *         I4,'.',I1.1,1X,A20)
9028    FORMAT(1X,A,'For money precede the value with "$".')
9027    FORMAT(1X,I3.3,'>',A8,I4,'.',I1.1,1X,A20,2X,I3.3,'>',
     *         A8,I6,1X,A20)
9029    FORMAT(1X,I3.3,'>',A6,I10,1X,A18,2X,I3.3,'>',A8,I6,1X,A20)
9030    FORMAT(1X,I3.3,'>',A7,F11.2,1X,A16,2X,I3.3,'>',A8,I6,1X,A20)
9031    FORMAT(1X,I3.3,'>',A8,I6,1X,A20,2X,I3.3,'>',A7,F7.2,1X,A20)
903     FORMAT(A4)
904     FORMAT(1X,'File',6X,'File',12X,'Disk',3X,'File',3X,
     *            'File'16X,'File',8X,'On',/,        
     *         1X,'Number Description       Volume  Exist  Name',16X,
     *            'Size       Disk',/)
905     FORMAT(1X,I2.2,4X,A15,4X,4A1,4X,A3,3X,15A1,1X,I9,2X,I9)
906     FORMAT(4A1)
907     FORMAT(15A1)
908     FORMAT(' Parameters for game number ',I2.2,/,
     *         '  1 - Game type ..............',A8,1X,A7,/,
     *         '  2 - Game index..............',I2.2,/,
     *         '  3 - joker game number......',I2.2,/,
     *         '  4 - Game name (long)........',4A4,/,
     *         '  5 - Game name (short).......',A4,/,
     *         '  6 - File name...............',15A1,/,
     *         '  7 - File volume.............',4A1,/,
     *         '  8 - File size...............',I5.5,/,
     *         '  9 - Sales commission........',F5.2,/,
     *         ' 10 - Redemption maximum......',A11,/,
     *         ' 11 - Redemption minimum......',A9,/,
     *         ' 12 - Maximum cashing days....',I3,/,
     *         ' 13 - Maximum fractions.......',I3,/,
     *         ' 14 - Limit 1........',A11,6x'15 - Holding Days 1....',I4,/
     *         ' 16 - Limit 2........',A11,6x'17 - Holding Days 2....',I4,/
     *         ' 18 - Limit 3........',A11,6x'19 - Holding Days 3....',I4)

9081    FORMAT(' 20 - Verify file name........',15A1,/,
     *         ' 21 - Verify file volume......',4A1,/,
     *         ' 22 - Verify file size........',I5.5/)
9082    FORMAT(' 20 - Returns comission.......',F5.2,/)    
9091    FORMAT(1X,'Game types',/)
909     FORMAT(2X,I2,' - ',A8)
910     FORMAT(1X,I2,2X,4A4)
911     FORMAT(4A4)
912     FORMAT(1X,'Num Type',5X,'Index Name',13X,'File',13X,
     *         'Volume  Size    Joker',/)
913     FORMAT(1X,I2.2,2X,A8,1X,I1,5X,4A4,1X,15A1,2X,4A1,4X,
     *         I5.5,3X,I2.2)
915     FORMAT(' Game ',I2,1X,4A4,' current ticket charge ',A5)
916     FORMAT(' Validation commission ............ ',6X,F5.2,/,
     *         ' Claim commission (amt per claim).. ',A11,/,
     *         ' High tier winner commission....... ',6X,F5.2,/,
     *         ' High tier winner prize level...... ',A11,/,
     *         ' High tier winner tax rate......... ',6X,F5.2,/,
     *         ' High tier winner tax prize level.. ',A11,/,
     *         ' Commission tax rate............... ',6X,F5.2,/)
917     FORMAT(/,' Scanning for File Atributes...... ')
952     FORMAT(/,1X,' 1) Primary System Port A : ',I4.4,/,
     *           1X,' 2) Backup  System Port A : ',I4.4,/,
     *           1X,' 3) Primary System Port B : ',I4.4,/,
     *           1X,' 4) Backup  System Port B : ',I4.4,//,
     *           1X,' 5) Both Systems IP Prefix 1: ',I3.3,/,
     *           1X,' 6) Both Systems IP Prefix 2: ',I3.3,//,
     *           1X,' 7) Primary System IP Suffix 1, Primary Lan: ',I3.3,/,
     *           1X,' 8) Primary System IP Suffix 2, Primary Lan: ',I3.3,/,
     *           1X,' 9) Backup  System IP Suffix 1, Primary Lan: ',I3.3,/,
     *           1X,'10) Backup  System IP Suffix 2, Primary Lan: ',I3.3,/,
     *           1X,'11) Primary System IP Suffix 1, Backup  Lan: ',I3.3,/,
     *           1X,'12) Primary System IP Suffix 2, Backup  Lan: ',I3.3,/,
     *           1X,'13) Backup  System IP Suffix 1, Backup  Lan: ',I3.3,/,
     *           1X,'14) Backup  System IP Suffix 2, Backup  Lan: ',I3.3,//,
     *           1X,'15) Save Changes and Exit',/)
953     FORMAT(/,1X,'Do you want to exit without saving changes?')
        END
