$! starts with removing the extra COMOLM in the current directory 
$ PROCESS = "OLM"
$ JUMPTO=1
$ OPEN/WRITE HFILE GXOLM:TESTAUX.DAT
$! read all the results from the search
$ NUMFILES = 0
$LOOPS:
$ FILE = F$SEARCH("COMOLM.EXE;*") 
$! WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHEND
$ NUMFILES = NUMFILES+1
$ GOTO LOOPS
$SEARCHEND:
$ CLOSE HFILE
$! WAIT 00:00:01
$ OPEN/READ HRFILE GXOLM:TESTAUX.DAT
$read_loop:
$ read/end_of_file=done_loop HRFILE opt1
$! WRITE SYS$OUTPUT "''opt1'"
$ goto read_loop
$! WAIT 00:00:01
$!dir COMOLM.EXE;0
$!file = f$search("COMOLM.EXE")
$!sh time
$!READ SYS$INPUT 
$done_loop:
$ close HRFILE
$ WRITE SYS$OUTPUT NUMFILES
$ IF NUMFILES .GT. 2 
$ THEN
$   INQUIRE OPTION "There are more then 2 ''PROCESS' remove the extra ones (Y/N)?" 
$   WRITE SYS$OUTPUT OPTION
$   IF OPTION .EQ. "Y" .OR. OPTION .EQ. "y" 
$   THEN
$!     WRITE SYS$OUTPUT "so is it (OLM or IGS)???---> ''PROCESS'"
$     WRITE SYS$OUTPUT (PROCESS .EQ. "OLM") 
$     IF PROCESS .EQS. "OLM" 
$     THEN
$       PURGE COMOLM.EXE /log/keep=2
$       WRITE SYS$OUTPUT "******Removed OLM files with Success***"
$       PROCESS="IGS"
$       JUMPTO=1
$     ELSE
$       IF PROCESS .EQS. "IGS" 
$       THEN
$           WRITE SYS$OUTPUT "will purge IGS files"
$!           SET DEF GXIGS
$           PURGE GXIGS:INIGS.EXE /log/keep=2
$           PURGE GXIGS:OUTIGS.EXE /log/keep=2
$           PURGE GXIGS:COMIGS.EXE /log/keep=2
$           WRITE SYS$OUTPUT "******Removed IGS files with Success***"
$           JUMPTO=3
$           PROCESS="UTL"
$       ELSE
$           IF PROCESS .EQS. "UTL" 
$           THEN
$               WRITE SYS$OUTPUT "will purge UTL files"
$               PURGE GXUTL:AGTSUP.EXE /log/keep=2 
$               PURGE GXUTL:ASYNC_ASF.EXE /log/keep=2 
$               PURGE GXUTL:BSIM.EXE /log/keep=2 
$               PURGE GXUTL:BWTEST.EXE /log/keep=2 
$               PURGE GXUTL:CHECKLIST.EXE /log/keep=2 
$               PURGE GXUTL:COMMGT.EXE /log/keep=2 
$               PURGE GXUTL:CPYMTMF.EXE /log/keep=2 
$               PURGE GXUTL:DISP_SSC.EXE /log/keep=2 
$               PURGE GXUTL:DMPTOOL.EXE /log/keep=2 
$               PURGE GXUTL:EUROTMIR.EXE /log/keep=2 
$               PURGE GXUTL:FIXOPS.EXE /log/keep=2 
$               PURGE GXUTL:FIXTCF.EXE /log/keep=2 
$               PURGE GXUTL:FIXTMF.EXE /log/keep=2 
$               PURGE GXUTL:FIXVPF.EXE /log/keep=2 
$               PURGE GXUTL:GENAGTMIL.EXE /log/keep=2 
$               PURGE GXUTL:GENCODS.EXE /log/keep=2 
$               PURGE GXUTL:GNPASSAP.EXE /log/keep=2 
$               PURGE GXUTL:IGSSIM.EXE /log/keep=2 
$               PURGE GXUTL:IPSSIM.EXE /log/keep=2 
$               PURGE GXUTL:LOAD_TEXT.EXE /log/keep=2 
$               PURGE GXUTL:LODSPTSYS.EXE /log/keep=2 
$               PURGE GXUTL:LSIM.EXE /log/keep=2 
$               PURGE GXUTL:MAKPAS.EXE /log/keep=2 
$               PURGE GXUTL:MODOPS.EXE /log/keep=2 
$               PURGE GXUTL:PDATE.EXE /log/keep=2 
$               PURGE GXUTL:PROMORPT.EXE /log/keep=2 
$               PURGE GXUTL:RESULTREP.EXE /log/keep=2 
$               PURGE GXUTL:SCANDAYS.EXE /log/keep=2 
$               PURGE GXUTL:SCANVPF.EXE /log/keep=2 
$               PURGE GXUTL:SETDEBUG.EXE /log/keep=2 
$               PURGE GXUTL:SETSUPWEL.EXE /log/keep=2 
$               PURGE GXUTL:SETTEST.EXE /log/keep=2 
$               PURGE GXUTL:SETTKF.EXE /log/keep=2 
$               PURGE GXUTL:SETTRACE.EXE /log/keep=2 
$               PURGE GXUTL:SPY.EXE /log/keep=2 
$               PURGE GXUTL:STOPSIM.EXE /log/keep=2 
$               PURGE GXUTL:SWINS.EXE /log/keep=2 
$               PURGE GXUTL:SYSFILES.EXE /log/keep=2 
$               PURGE GXUTL:TERSIM.EXE /log/keep=2 
$               PURGE GXUTL:TESTLASTJCKPOTDRW.EXE /log/keep=2 
$               PURGE GXUTL:TSTGAM.EXE /log/keep=2 
$               PURGE GXUTL:TSTLOG.EXE /log/keep=2 
$               PURGE GXUTL:TSTVAL.EXE /log/keep=2 
$               PURGE GXUTL:TVERIFY.EXE /log/keep=2 
$               PURGE GXUTL:UPDASF_GSC.EXE /log/keep=2 
$               PURGE GXUTL:UPDPAS8_ASF.EXE /log/keep=2 
$               PURGE GXUTL:VALDIS.EXE /log/keep=2 
$               PURGE GXUTL:VALSTS.EXE /log/keep=2 
$               PURGE GXUTL:VSYSTAP.EXE /log/keep=2 
$               WRITE SYS$OUTPUT "******Removed UTL files with Success***"
$               JUMPTO=4
$               PROCESS="TSK"
$           ELSE
$               IF PROCESS .EQS. "TSK" 
$               THEN 
$                   WRITE SYS$OUTPUT "will purge TSK files"
$                   PURGE GXTSK:ABPAUDIT.EXE /log/keep=2 
$                   PURGE GXTSK:ADJRPT.EXE /log/keep=2 
$                   PURGE GXTSK:AGTACT.EXE /log/keep=2 
$                   PURGE GXTSK:AGTCTM_PROC.EXE /log/keep=2 
$                   PURGE GXTSK:AGTLST.EXE /log/keep=2 
$                   PURGE GXTSK:AGTPSW.EXE /log/keep=2 
$                   PURGE GXTSK:ALLDLL.EXE /log/keep=2 
$                   PURGE GXTSK:ALT_SIGNON.EXE /log/keep=2 
$                   PURGE GXTSK:ANLDRW.EXE /log/keep=2 
$                   PURGE GXTSK:ANLTCF.EXE /log/keep=2 
$                   PURGE GXTSK:ANLTMF.EXE /log/keep=2 
$                   PURGE GXTSK:ANLTPF.EXE /log/keep=2 
$                   PURGE GXTSK:ANLVLF.EXE /log/keep=2 
$                   PURGE GXTSK:ANLVPF.EXE /log/keep=2 
$                   PURGE GXTSK:APPGTP.EXE /log/keep=2 
$                   PURGE GXTSK:APULOG.EXE /log/keep=2 
$                   PURGE GXTSK:ASF2ESC.EXE /log/keep=2 
$                   PURGE GXTSK:ASFIPSINV.EXE /log/keep=2 
$                   PURGE GXTSK:ASFREP.EXE /log/keep=2 
$                   PURGE GXTSK:ASYHARD.EXE /log/keep=2 
$                   PURGE GXTSK:ASYSOFT.EXE /log/keep=2 
$                   PURGE GXTSK:ASYULT.EXE /log/keep=2 
$                   PURGE GXTSK:AUDITRPT.EXE /log/keep=2 
$                   PURGE GXTSK:AUDITSYS.EXE /log/keep=2 
$                   PURGE GXTSK:AUDIT_LOGTMF.EXE /log/keep=2 
$                   PURGE GXTSK:BALANS.EXE /log/keep=2 
$                   PURGE GXTSK:BIGWAF.EXE /log/keep=2 
$                   PURGE GXTSK:BINGOSCAN.EXE /log/keep=2 
$                   PURGE GXTSK:BKKREP.EXE /log/keep=2 
$                   PURGE GXTSK:BLDEMIS.EXE /log/keep=2 
$                   PURGE GXTSK:BLDEURSYS.EXE /log/keep=2 
$                   PURGE GXTSK:BLDPLAN.EXE /log/keep=2 
$                   PURGE GXTSK:BLDSYS.EXE /log/keep=2 
$                   PURGE GXTSK:BLDX2X.EXE /log/keep=2 
$                   PURGE GXTSK:BNGDRWSCN.EXE /log/keep=2 
$                   PURGE GXTSK:BNGFHPRG.EXE /log/keep=2 
$                   PURGE GXTSK:BNGPRO.EXE /log/keep=2 
$                   PURGE GXTSK:BNGRES.EXE /log/keep=2 
$                   PURGE GXTSK:BNGTMFSCN.EXE /log/keep=2 
$                   PURGE GXTSK:BNGTOPC.EXE /log/keep=2 
$                   PURGE GXTSK:BSAGTCTL.EXE /log/keep=2 
$                   PURGE GXTSK:BWINSEL.EXE /log/keep=2 
$                   PURGE GXTSK:BWINTSK.EXE /log/keep=2 
$                   PURGE GXTSK:CANPRO.EXE /log/keep=2 
$                   PURGE GXTSK:CANREP.EXE /log/keep=2 
$                   PURGE GXTSK:CANREPJOK.EXE /log/keep=2 
$                   PURGE GXTSK:CARACT.EXE /log/keep=2 
$                   PURGE GXTSK:CARRPT.EXE /log/keep=2 
$                   PURGE GXTSK:CCHKPNT.EXE /log/keep=2 
$                   PURGE GXTSK:CDC_TO_DATE.EXE /log/keep=2 
$                   PURGE GXTSK:CDC_TO_LDATE.EXE /log/keep=2 
$                   PURGE GXTSK:CFILES.EXE /log/keep=2 
$                   PURGE GXTSK:CHGLVL.EXE /log/keep=2 
$                   PURGE GXTSK:CHKFIL.EXE /log/keep=2 
$                   PURGE GXTSK:CHKPNT.EXE /log/keep=2 
$                   PURGE GXTSK:CHKPNTSIZ.EXE /log/keep=2
$                   PURGE GXTSK:CLKREP.EXE /log/keep=2
$                   PURGE GXTSK:CLRCRK.EXE /log/keep=2
$                   PURGE GXTSK:CLRFIL.EXE /log/keep=2
$                   PURGE GXTSK:CMDPRO.EXE /log/keep=2
$                   PURGE GXTSK:CNVASF.EXE /log/keep=2
$                   PURGE GXTSK:CNVCONC.EXE /log/keep=2
$                   PURGE GXTSK:CNVDAF.EXE /log/keep=2
$                   PURGE GXTSK:CNVDKK.EXE /log/keep=2
$                   PURGE GXTSK:CNVRWF.EXE /log/keep=2
$                   PURGE GXTSK:CNVSCF.EXE /log/keep=2
$                   PURGE GXTSK:CNVSTF.EXE /log/keep=2
$                   PURGE GXTSK:CNVTKM.EXE /log/keep=2
$                   PURGE GXTSK:CNVX2X.EXE /log/keep=2
$                   PURGE GXTSK:COMMGR.EXE /log/keep=2 
$                   PURGE GXTSK:CONVERT_OPS.EXE /log/keep=2
$                   PURGE GXTSK:COPVPF.EXE /log/keep=2
$                   PURGE GXTSK:CREATE_OPS_FILES.EXE /log/keep=2
$                   PURGE GXTSK:CRSPRO.EXE /log/keep=2
$                   PURGE GXTSK:CRSSIM.EXE /log/keep=2
$                   PURGE GXTSK:CRTTCC.EXE /log/keep=2
$                   PURGE GXTSK:CRTTCW.EXE /log/keep=2
$                   PURGE GXTSK:CRTVLC.EXE /log/keep=2
$                   PURGE GXTSK:CSHPAS.EXE /log/keep=2
$                   PURGE GXTSK:CSHREP.EXE /log/keep=2
$                   PURGE GXTSK:CSHREP1.EXE /log/keep=2
$                   PURGE GXTSK:CSHWEEK.EXE /log/keep=2
$                   PURGE GXTSK:CTLPRO.EXE /log/keep=2
$                   PURGE GXTSK:CWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:CWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:DAFRPT.EXE /log/keep=2
$                   PURGE GXTSK:DATE_TO_CDC.EXE /log/keep=2
$                   PURGE GXTSK:DAYDWN.EXE /log/keep=2
$                   PURGE GXTSK:DAYEND.EXE /log/keep=2
$                   PURGE GXTSK:DCNPRO.EXE /log/keep=2
$                   PURGE GXTSK:DELAGT.EXE /log/keep=2
$                   PURGE GXTSK:DETRUN.EXE /log/keep=2
$                   PURGE GXTSK:DISNET.EXE /log/keep=2
$                   PURGE GXTSK:DISPAT.EXE /log/keep=2
$                   PURGE GXTSK:DITTO.EXE /log/keep=2
$                   PURGE GXTSK:DLLREAD.EXE /log/keep=2
$                   PURGE GXTSK:DMPLVL.EXE /log/keep=2
$                   PURGE GXTSK:DMPUSER.EXE /log/keep=2
$                   PURGE GXTSK:DUMP_OPS.EXE /log/keep=2
$                   PURGE GXTSK:DWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:DWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:DWNSEL.EXE /log/keep=2
$                   PURGE GXTSK:ELOG.EXE /log/keep=2
$                   PURGE GXTSK:EMITED_OPS.EXE /log/keep=2
$                   PURGE GXTSK:ENABONUS.EXE /log/keep=2
$                   PURGE GXTSK:ENABUN.EXE /log/keep=2
$                   PURGE GXTSK:ENALIB.EXE /log/keep=2
$                   PURGE GXTSK:ENAPRM.EXE /log/keep=2
$                   PURGE GXTSK:ENCPRO.EXE /log/keep=2
$                   PURGE GXTSK:ENCPROI.EXE /log/keep=2
$                   PURGE GXTSK:ERRLOG.EXE /log/keep=2
$                   PURGE GXTSK:EUROCONFIG.EXE /log/keep=2
$                   PURGE GXTSK:EWATCH.EXE /log/keep=2
$                   PURGE GXTSK:EXPEDICAO_OPS.EXE /log/keep=2
$                   PURGE GXTSK:EXWINRPT.EXE /log/keep=2
$                   PURGE GXTSK:FAULTANL.EXE /log/keep=2
$                   PURGE GXTSK:FBIGWIN.EXE /log/keep=2
$                   PURGE GXTSK:FCANWIN.EXE /log/keep=2
$                   PURGE GXTSK:FILREP.EXE /log/keep=2
$                   PURGE GXTSK:FINDSSODDS.EXE /log/keep=2
$                   PURGE GXTSK:FORMAT_OPS_TO_PRINT.EXE /log/keep=2
$                   PURGE GXTSK:FPTPRO.EXE /log/keep=2
$                   PURGE GXTSK:FTP_SCAN.EXE /log/keep=2
$                   PURGE GXTSK:GAMINI.EXE /log/keep=2
$                   PURGE GXTSK:GAMSET.EXE /log/keep=2
$                   PURGE GXTSK:GAMTOT.EXE /log/keep=2
$                   PURGE GXTSK:GBDAILY.EXE /log/keep=2
$                   PURGE GXTSK:GCLOSE.EXE /log/keep=2
$                   PURGE GXTSK:GENPAS.EXE /log/keep=2
$                   PURGE GXTSK:GENPAS2.EXE /log/keep=2
$                   PURGE GXTSK:GENPASS.EXE /log/keep=2
$                   PURGE GXTSK:GENPSORC.EXE /log/keep=2
$                   PURGE GXTSK:GERPS2.EXE /log/keep=2
$                   PURGE GXTSK:GETCHK.EXE /log/keep=2
$                   PURGE GXTSK:GETOFSAL.EXE /log/keep=2
$                   PURGE GXTSK:GET_OFFPRIZES.EXE /log/keep=2
$                   PURGE GXTSK:GET_OFFPRIZES_12.EXE /log/keep=2
$                   PURGE GXTSK:GET_OFFPRIZES_ANT.EXE /log/keep=2
$                   PURGE GXTSK:GET_OFF_WINNERS.EXE /log/keep=2
$                   PURGE GXTSK:GET_OPS_PAID.EXE /log/keep=2
$                   PURGE GXTSK:GNINVALL.EXE /log/keep=2
$                   PURGE GXTSK:GOSUPER14.EXE /log/keep=2
$                   PURGE GXTSK:GSALTSK.EXE /log/keep=2
$                   PURGE GXTSK:GSDRWDIV.EXE /log/keep=2
$                   PURGE GXTSK:GSDRWDRW.EXE /log/keep=2
$                   PURGE GXTSK:GSDRWMAP.EXE /log/keep=2
$                   PURGE GXTSK:GSDRWRES.EXE /log/keep=2
$                   PURGE GXTSK:GSONLGAM.EXE /log/keep=2
$                   PURGE GXTSK:GSONLTRN.EXE /log/keep=2
$                   PURGE GXTSK:GSPAYORD.EXE /log/keep=2
$                   PURGE GXTSK:GSPOPAID.EXE /log/keep=2
$                   PURGE GXTSK:GUILINK.EXE /log/keep=2
$                   PURGE GXTSK:GUIMGR.EXE /log/keep=2
$                   PURGE GXTSK:GUIWORK.EXE /log/keep=2
$                   PURGE GXTSK:GUI_RELOAD.EXE /log/keep=2
$                   PURGE GXTSK:GVTALL.EXE /log/keep=2
$                   PURGE GXTSK:GVTDWN.EXE /log/keep=2
$                   PURGE GXTSK:GVTESTAB.EXE /log/keep=2
$                   PURGE GXTSK:HASF.EXE /log/keep=2
$                   PURGE GXTSK:HDRINI.EXE /log/keep=2
$                   PURGE GXTSK:HDRRPT.EXE /log/keep=2
$                   PURGE GXTSK:INCPRO.EXE /log/keep=2
$                   PURGE GXTSK:INMGR.EXE /log/keep=2
$                   PURGE GXTSK:INSOUT.EXE /log/keep=2
$                   PURGE GXTSK:INSPRO.EXE /log/keep=2
$                   PURGE GXTSK:INSSIM.EXE /log/keep=2
$                   PURGE GXTSK:INVCLC.EXE /log/keep=2
$                   PURGE GXTSK:INVRPT.EXE /log/keep=2
$                   PURGE GXTSK:KIKNUM.EXE /log/keep=2
$                   PURGE GXTSK:LANPRO.EXE /log/keep=2
$                   PURGE GXTSK:LIABLE.EXE /log/keep=2
$                   PURGE GXTSK:LISTCKFIL.EXE /log/keep=2
$                   PURGE GXTSK:LNCNVFIL.EXE /log/keep=2
$                   PURGE GXTSK:LNKCH.EXE /log/keep=2
$                   PURGE GXTSK:LOADSCF.EXE /log/keep=2
$                   PURGE GXTSK:LOAD_BANKS.EXE /log/keep=2
$                   PURGE GXTSK:LOAD_BRANCHES.EXE /log/keep=2
$                   PURGE GXTSK:LOD1X2.EXE /log/keep=2
$                   PURGE GXTSK:LOD1X2POL.EXE /log/keep=2
$                   PURGE GXTSK:LODAGT.EXE /log/keep=2
$                   PURGE GXTSK:LODAINV.EXE /log/keep=2
$                   PURGE GXTSK:LODEMIS.EXE /log/keep=2
$                   PURGE GXTSK:LODFIL.EXE /log/keep=2
$                   PURGE GXTSK:LODIMG.EXE /log/keep=2
$                   PURGE GXTSK:LODIMG2.EXE /log/keep=2
$                   PURGE GXTSK:LODIMG3.EXE /log/keep=2
$                   PURGE GXTSK:LODIMG4.EXE /log/keep=2
$                   PURGE GXTSK:LODIMG5.EXE /log/keep=2
$                   PURGE GXTSK:LODIMGS2.EXE /log/keep=2
$                   PURGE GXTSK:LODIMGS3.EXE /log/keep=2
$                   PURGE GXTSK:LODIMGSS.EXE /log/keep=2
$                   PURGE GXTSK:LODLTO.EXE /log/keep=2
$                   PURGE GXTSK:LODPLAN.EXE /log/keep=2
$                   PURGE GXTSK:LODSTA.EXE /log/keep=2
$                   PURGE GXTSK:LOGGER.EXE /log/keep=2
$                   PURGE GXTSK:LOGTMF.EXE /log/keep=2
$                   PURGE GXTSK:LOOPBACK.EXE /log/keep=2
$                   PURGE GXTSK:LOPPAS.EXE /log/keep=2
$                   PURGE GXTSK:LSTCRDB.EXE /log/keep=2
$                   PURGE GXTSK:LSTTCKFIL.EXE /log/keep=2
$                   PURGE GXTSK:LTODIS.EXE /log/keep=2
$                   PURGE GXTSK:LTOST39.EXE /log/keep=2
$                   PURGE GXTSK:MAAUDIT.EXE /log/keep=2
$                   PURGE GXTSK:MAILTAPE.EXE /log/keep=2
$                   PURGE GXTSK:MAKFIL.EXE /log/keep=2
$                   PURGE GXTSK:MAKFIL2.EXE /log/keep=2
$                   PURGE GXTSK:MDCMNG.EXE /log/keep=2
$                   PURGE GXTSK:MESMNT.EXE /log/keep=2
$                   PURGE GXTSK:MILLRECON.EXE /log/keep=2
$                   PURGE GXTSK:MKTMES.EXE /log/keep=2
$                   PURGE GXTSK:MLCOPVLF.EXE /log/keep=2
$                   PURGE GXTSK:MLMRGTCF.EXE /log/keep=2
$                   PURGE GXTSK:MLMRGVLF.EXE /log/keep=2
$                   PURGE GXTSK:MODX2XTPRO.EXE /log/keep=2
$                   PURGE GXTSK:MONGOLS.EXE /log/keep=2
$                   PURGE GXTSK:MRGTCF.EXE /log/keep=2
$                   PURGE GXTSK:MRGTSK.EXE /log/keep=2
$                   PURGE GXTSK:MRGVLF.EXE /log/keep=2
$                   PURGE GXTSK:MSCMGR.EXE /log/keep=2
$                   PURGE GXTSK:MSCSTATS.EXE /log/keep=2
$                   PURGE GXTSK:MSCTEST.EXE /log/keep=2
$                   PURGE GXTSK:MULTITCF.EXE /log/keep=2
$                   PURGE GXTSK:MULTIVLF.EXE /log/keep=2
$                   PURGE GXTSK:MULTIWIN.EXE /log/keep=2
$                   PURGE GXTSK:MXSRV.EXE /log/keep=2
$                   PURGE GXTSK:MXVISION.EXE /log/keep=2
$                   PURGE GXTSK:NBRPRO.EXE /log/keep=2
$                   PURGE GXTSK:NETDUMP.EXE /log/keep=2
$                   PURGE GXTSK:NETLOG.EXE /log/keep=2
$                   PURGE GXTSK:NETMGR.EXE /log/keep=2
$                   PURGE GXTSK:NETMON.EXE /log/keep=2
$                   PURGE GXTSK:ODDINF.EXE /log/keep=2
$                   PURGE GXTSK:ODDPRO.EXE /log/keep=2
$                   PURGE GXTSK:ODSPRO.EXE /log/keep=2
$                   PURGE GXTSK:OUTMGR.EXE /log/keep=2
$                   PURGE GXTSK:OVERPRO.EXE /log/keep=2
$                   PURGE GXTSK:PAGAMENTO_MANUAL.EXE /log/keep=2
$                   PURGE GXTSK:PASPRO.EXE /log/keep=2
$                   PURGE GXTSK:PASPURGE.EXE /log/keep=2
$                   PURGE GXTSK:PASUPD.EXE /log/keep=2
$                   PURGE GXTSK:PASVAL.EXE /log/keep=2
$                   PURGE GXTSK:PAYUPD.EXE /log/keep=2
$                   PURGE GXTSK:PCLOG.EXE /log/keep=2
$                   PURGE GXTSK:PC_START.EXE /log/keep=2
$                   PURGE GXTSK:PC_STOP.EXE /log/keep=2
$                   PURGE GXTSK:PJPRIZES.EXE /log/keep=2
$                   PURGE GXTSK:PJRESULTS.EXE /log/keep=2
$                   PURGE GXTSK:PMXTRACT.EXE /log/keep=2
$                   PURGE GXTSK:POOLBLD.EXE /log/keep=2
$                   PURGE GXTSK:POOLMGR.EXE /log/keep=2
$                   PURGE GXTSK:POOLPRO.EXE /log/keep=2
$                   PURGE GXTSK:POOLSEE.EXE /log/keep=2
$                   PURGE GXTSK:PRGTSK.EXE /log/keep=2
$                   PURGE GXTSK:PRININVOFF.EXE /log/keep=2
$                   PURGE GXTSK:PRIZES.EXE /log/keep=2
$                   PURGE GXTSK:PRIZEVER.EXE /log/keep=2
$                   PURGE GXTSK:PROC_OPS_PAID.EXE /log/keep=2
$                   PURGE GXTSK:PROC_ORCPAS.EXE /log/keep=2
$                   PURGE GXTSK:PROMOTION.EXE /log/keep=2
$                   PURGE GXTSK:PURGE_OPS.EXE /log/keep=2
$                   PURGE GXTSK:PURWIN.EXE /log/keep=2
$                   PURGE GXTSK:QLIKTRAN.EXE /log/keep=2
$                   PURGE GXTSK:REL5000_OPS.EXE /log/keep=2
$                   PURGE GXTSK:RELAT_OPS_PAID.EXE /log/keep=2
$                   PURGE GXTSK:RELAT_OPS_TO_BANK.EXE /log/keep=2
$                   PURGE GXTSK:RELCONC.EXE /log/keep=2
$                   PURGE GXTSK:REPCAN.EXE /log/keep=2
$                   PURGE GXTSK:REPROX.EXE /log/keep=2
$                   PURGE GXTSK:RESET.EXE /log/keep=2
$                   PURGE GXTSK:RESULTS.EXE /log/keep=2
$                   PURGE GXTSK:RESVER.EXE /log/keep=2
$                   PURGE GXTSK:ROLPOL.EXE /log/keep=2
$                   PURGE GXTSK:ROMCHK.EXE /log/keep=2
$                   PURGE GXTSK:SAVDRW.EXE /log/keep=2
$                   PURGE GXTSK:SAY.EXE /log/keep=2
$                   PURGE GXTSK:SBRUN.EXE /log/keep=2
$                   PURGE GXTSK:SCANRETR.EXE /log/keep=2
$                   PURGE GXTSK:SCANVLF.EXE /log/keep=2
$                   PURGE GXTSK:SEECOM.EXE /log/keep=2
$                   PURGE GXTSK:SELECT_2VIA.EXE /log/keep=2
$                   PURGE GXTSK:SELECT_OPS_TO_PRINT.EXE /log/keep=2
$                   PURGE GXTSK:SEND_OPS_PAID_SAP.EXE /log/keep=2
$                   PURGE GXTSK:SEND_OPS_TO_BANK.EXE /log/keep=2
$                   PURGE GXTSK:SETAGTMXT_PROC.EXE /log/keep=2
$                   PURGE GXTSK:SETBEFDRWS.EXE /log/keep=2
$                   PURGE GXTSK:SETCCC.EXE /log/keep=2
$                   PURGE GXTSK:SETCLOSE.EXE /log/keep=2
$                   PURGE GXTSK:SETLOTSHDR.EXE /log/keep=2
$                   PURGE GXTSK:SETLVL.EXE /log/keep=2
$                   PURGE GXTSK:SETMULTI.EXE /log/keep=2
$                   PURGE GXTSK:SETPOOL.EXE /log/keep=2
$                   PURGE GXTSK:SETUP_SYSTEM.EXE /log/keep=2
$                   PURGE GXTSK:SET_CALLBACK.EXE /log/keep=2
$                   PURGE GXTSK:SHARECLC.EXE /log/keep=2
$                   PURGE GXTSK:SHARERPT.EXE /log/keep=2
$                   PURGE GXTSK:SHAREUPD.EXE /log/keep=2
$                   PURGE GXTSK:SHCOMMON.EXE /log/keep=2
$                   PURGE GXTSK:SHRTSK.EXE /log/keep=2
$                   PURGE GXTSK:SIGNRPT.EXE /log/keep=2
$                   PURGE GXTSK:SLOCHK.EXE /log/keep=2
$                   PURGE GXTSK:SOUPFM.EXE /log/keep=2
$                   PURGE GXTSK:SOUPWIN.EXE /log/keep=2
$                   PURGE GXTSK:SOUPWINALL.EXE /log/keep=2
$                   PURGE GXTSK:SPESRV.EXE /log/keep=2
$                   PURGE GXTSK:SPESRVF.EXE /log/keep=2
$                   PURGE GXTSK:SPMODJACKPOT.EXE /log/keep=2
$                   PURGE GXTSK:SPMODWEEKCNT.EXE /log/keep=2
$                   PURGE GXTSK:SPTCANEVENTS.EXE /log/keep=2
$                   PURGE GXTSK:SPTDIS.EXE /log/keep=2
$                   PURGE GXTSK:SPTSTA.EXE /log/keep=2
$                   PURGE GXTSK:SSWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:SSWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:STARTER.EXE /log/keep=2
$                   PURGE GXTSK:STATANL.EXE /log/keep=2
$                   PURGE GXTSK:STSYSTEM.EXE /log/keep=2
$                   PURGE GXTSK:STWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:STWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:SWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:SWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:SYSTOT.EXE /log/keep=2
$                   PURGE GXTSK:TAPLOG.EXE /log/keep=2
$                   PURGE GXTSK:TAXCONFIG.EXE /log/keep=2
$                   PURGE GXTSK:TAXMNG.EXE /log/keep=2
$                   PURGE GXTSK:TCFCPY.EXE /log/keep=2
$                   PURGE GXTSK:TCFSUM.EXE /log/keep=2
$                   PURGE GXTSK:TCFXFR.EXE /log/keep=2
$                   PURGE GXTSK:TCPASST.EXE /log/keep=2
$                   PURGE GXTSK:TCPCNTRL.EXE /log/keep=2
$                   PURGE GXTSK:TCPDEBUG.EXE /log/keep=2
$                   PURGE GXTSK:TCPSIM.EXE /log/keep=2
$                   PURGE GXTSK:TELL.EXE /log/keep=2
$                   PURGE GXTSK:TIMER.EXE /log/keep=2
$                   PURGE GXTSK:TMFDMP.EXE /log/keep=2
$                   PURGE GXTSK:TMFMON.EXE /log/keep=2
$                   PURGE GXTSK:TMFREP.EXE /log/keep=2
$                   PURGE GXTSK:TMIR.EXE /log/keep=2
$                   PURGE GXTSK:TMIR_BEF_5626.EXE /log/keep=2
$                   PURGE GXTSK:TMREPS.EXE /log/keep=2
$                   PURGE GXTSK:TMSCAN.EXE /log/keep=2
$                   PURGE GXTSK:TOOLS.EXE /log/keep=2
$                   PURGE GXTSK:TRWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:TRWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:TSLSUP.EXE /log/keep=2
$                   PURGE GXTSK:TSTENC.EXE /log/keep=2
$                   PURGE GXTSK:TWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:TWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:UNMESS.EXE /log/keep=2
$                   PURGE GXTSK:UNPAID_OPS.EXE /log/keep=2
$                   PURGE GXTSK:UNSPRO.EXE /log/keep=2
$                   PURGE GXTSK:UPDASF.EXE /log/keep=2
$                   PURGE GXTSK:UPDJACKPOT.EXE /log/keep=2
$                   PURGE GXTSK:UPDTSK.EXE /log/keep=2
$                   PURGE GXTSK:UPDVPF.EXE /log/keep=2
$                   PURGE GXTSK:USERPASS.EXE /log/keep=2
$                   PURGE GXTSK:VALPRO.EXE /log/keep=2
$                   PURGE GXTSK:VAX_SEND.EXE /log/keep=2
$                   PURGE GXTSK:VISION.EXE /log/keep=2
$                   PURGE GXTSK:VKSBLD.EXE /log/keep=2
$                   PURGE GXTSK:VLF2VLC.EXE /log/keep=2
$                   PURGE GXTSK:VLFCPY.EXE /log/keep=2
$                   PURGE GXTSK:VLFREP.EXE /log/keep=2
$                   PURGE GXTSK:VLFTSK.EXE /log/keep=2
$                   PURGE GXTSK:VMIR.EXE /log/keep=2
$                   PURGE GXTSK:VPJMC.EXE /log/keep=2
$                   PURGE GXTSK:VPURGE.EXE /log/keep=2
$                   PURGE GXTSK:W9LSYS.EXE /log/keep=2
$                   PURGE GXTSK:WAGPRO.EXE /log/keep=2
$                   PURGE GXTSK:WDRSAP.EXE /log/keep=2
$                   PURGE GXTSK:WEEKSHR.EXE /log/keep=2
$                   PURGE GXTSK:WEEKTSHR.EXE /log/keep=2
$                   PURGE GXTSK:WINCLR.EXE /log/keep=2
$                   PURGE GXTSK:WINPAS.EXE /log/keep=2
$                   PURGE GXTSK:WINRPT.EXE /log/keep=2
$                   PURGE GXTSK:WINSEL.EXE /log/keep=2
$                   PURGE GXTSK:WINTSK.EXE /log/keep=2
$                   PURGE GXTSK:WINUPD.EXE /log/keep=2
$                   PURGE GXTSK:WPASAP.EXE /log/keep=2
$                   PURGE GXTSK:WWINSEL.EXE /log/keep=2
$                   PURGE GXTSK:WWINTSK.EXE /log/keep=2
$                   PURGE GXTSK:X21HARD.EXE /log/keep=2
$                   PURGE GXTSK:X21SOFT.EXE /log/keep=2
$                   PURGE GXTSK:X21ULT.EXE /log/keep=2
$                   PURGE GXTSK:X2BADADR.EXE /log/keep=2
$                   PURGE GXTSK:X2BLDGRP.EXE /log/keep=2
$                   PURGE GXTSK:X2BLDHLP.EXE /log/keep=2
$                   PURGE GXTSK:X2BLDMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2BLDNET.EXE /log/keep=2
$                   PURGE GXTSK:X2BLDPRT.EXE /log/keep=2
$                   PURGE GXTSK:X2BRO.EXE /log/keep=2
$                   PURGE GXTSK:X2BROMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2CHKMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2CHK_DIST.EXE /log/keep=2
$                   PURGE GXTSK:X2CLRPRT.EXE /log/keep=2
$                   PURGE GXTSK:X2CLRSTA.EXE /log/keep=2
$                   PURGE GXTSK:X2CNVASF.EXE /log/keep=2
$                   PURGE GXTSK:X2DEBUG.EXE /log/keep=2
$                   PURGE GXTSK:X2DELTER.EXE /log/keep=2
$                   PURGE GXTSK:X2EXTCHK.EXE /log/keep=2
$                   PURGE GXTSK:X2GAMSTART.EXE /log/keep=2
$                   PURGE GXTSK:X2GBLMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2GRPMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2LDSTRN.EXE /log/keep=2
$                   PURGE GXTSK:X2LINREP.EXE /log/keep=2
$                   PURGE GXTSK:X2LODPRT.EXE /log/keep=2
$                   PURGE GXTSK:X2LPCMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2MON.EXE /log/keep=2
$                   PURGE GXTSK:X2NETCTL.EXE /log/keep=2
$                   PURGE GXTSK:X2NPCMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2PRTREP.EXE /log/keep=2
$                   PURGE GXTSK:X2RCDMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2RCLMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2RELLOC.EXE /log/keep=2
$                   PURGE GXTSK:X2RELOAD.EXE /log/keep=2
$                   PURGE GXTSK:X2RNKREP.EXE /log/keep=2
$                   PURGE GXTSK:X2ROMREVCHK.EXE /log/keep=2
$                   PURGE GXTSK:X2SCLMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2SPCMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2STNBCD.EXE /log/keep=2
$                   PURGE GXTSK:X2STNMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2STNRPT.EXE /log/keep=2
$                   PURGE GXTSK:X2STOPSEGMSGS.EXE /log/keep=2
$                   PURGE GXTSK:X2TERMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2TERMSTA.EXE /log/keep=2
$                   PURGE GXTSK:X2TERMSTAX.EXE /log/keep=2
$                   PURGE GXTSK:X2TERMSTAY.EXE /log/keep=2
$                   PURGE GXTSK:X2TESNET.EXE /log/keep=2
$                   PURGE GXTSK:X2TTNMEN.EXE /log/keep=2
$                   PURGE GXTSK:X2XBRO_DUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2XDEBUG.EXE /log/keep=2
$                   PURGE GXTSK:X2XDUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2XGBL_DUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2XMGR.EXE /log/keep=2
$                   PURGE GXTSK:X2XNPC_DUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2XPRO.EXE /log/keep=2
$                   PURGE GXTSK:X2XRAPP.EXE /log/keep=2
$                   PURGE GXTSK:X2XREL.EXE /log/keep=2
$                   PURGE GXTSK:X2XSCL_DUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2XTTN_DUMP.EXE /log/keep=2
$                   PURGE GXTSK:X2_SORT_EVSN.EXE /log/keep=2
$                   PURGE GXTSK:XAGTLST.EXE /log/keep=2
$                   PURGE GXTSK:YEAREND.EXE /log/keep=2
$                   PURGE GXTSK:YEARRPT.EXE /log/keep=2
$                   JUMPTO=2
$               ENDIF   
$           ENDIF
$       ENDIF
$     ENDIF
$   ELSE
$       WRITE SYS$OUTPUT "Files where not removed then exit"     
$       JUMPTO=2     
$   ENDIF
$ ELSE 
$   WRITE SYS$OUTPUT "current process is: ''PROCESS'"
$   IF PROCESS .EQS. "OLM" THEN PROCESS="IGS"
$   IF PROCESS .EQS. "IGS" 
$   THEN 
$      PROCESS="UTL"
$      JUMPTO=3
$   ENDIF
$   IF PROCESS .EQS. "UTL" 
$   THEN 
$       PROCESS="TSK"
$       JUMPTO=4
$   ENDIF
$   WRITE SYS$OUTPUT "will change to different directory: ''PROCESS'"
$ ENDIF
$
$ WRITE SYS$OUTPUT "   There Will be a jumpto: ''JUMPTO'"
$ IF JUMPTO .EQ. 1 THEN GOTO remove_igs
$ IF JUMPTO .EQ. 2 THEN GOTO exit_step
$ IF JUMPTO .EQ. 3 THEN GOTO remove_lgs
$ IF JUMPTO .EQ. 4 THEN GOTO remove_tsk
$
$exit_step:
$ EXIT
$
$WRITE_ERROR:
$ CLOSE HFILE 
$ WRITE SYS$OUTPUT "******Finished with Error in Write***"
$ EXIT
$!
$!Remove now the extra INIGS, OUTIGS & COMIGS in GXIGS
$remove_igs:
$ OPEN/WRITE HFILE GXIGS:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCH:
$ FILE = F$SEARCH("GXIGS:INIGS.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDIGS
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCH
$SEARCHENDIGS:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXIGS:TESTAUX.DAT
$read_loopigs:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_loopigs
$
$
$remove_lgs:
$ OPEN/WRITE HFILE GXUTL:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCHUTL:
$ FILE = F$SEARCH("GXUTL:AGTSUP.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDUTL
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCHUTL
$SEARCHENDUTL:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXUTL:TESTAUX.DAT
$read_looputl:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_looputl
$
$
$remove_tsk:
$ OPEN/WRITE HFILE GXTSK:TESTAUX.DAT
$ NUMFILES = 0
$LOOPSEARCHTSK:
$ FILE = F$SEARCH("GXTSK:ABPAUDIT.EXE;*") 
$ WRITE SYS$OUTPUT FILE
$ WRITE/ERROR=WRITE_ERROR HFILE FILE
$ IF FILE .EQS. "" THEN GOTO SEARCHENDTSK
$ NUMFILES = NUMFILES+1
$ GOTO LOOPSEARCHTSK
$SEARCHENDTSK:
$ CLOSE HFILE
$ OPEN/READ HRFILE GXTSK:TESTAUX.DAT
$read_looptsk:
$ read/end_of_file=done_loop HRFILE opt1
$ goto read_looptsk



