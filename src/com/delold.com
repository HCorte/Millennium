$!--------------------------------------------------------------------------
$!   DELOLD.COM
$!   Autor: Luiz Simiao, Alterado por Raul Costa 
$!-  Apaga, a partir da data corrente, ficheiros antigos
$!   em vários directórios 
$!-  Executa um PURGE no GXTSK, aos ficheiros FOR*.DAT, OPS.FIL, SCF.FIL
$!-  And so on ...
$!-  ALTERACAO POR RAUL COSTA EM 21-MAY-2008:
$!-  "APAGA AS TELAS DO VISION COM 7 DIAS NA AREA SAVREP"
$!--------------------------------------------------------------------------
$ set on
$ on control_Y then exit
$ on error then continue
$ say := "write sys$output"
$ esc[0,8] = %x1b
$ bold 	   = "''esc'[1m"
$ normal   = "''esc'[0m"
$
$!--------------------------------------------------------------------------
$ Draw_e_Savdsk:
$ Qt_dias = 14
$ date     = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,Serão apagados todos os ficheiros anteriores a ''date_old' "
$ Inquire/nopun ret "do disco ''bold'DRAW''normal', Confirma [Y/N] [Y]: "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_Draw_e_Savdsk
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos no disco DRAW ...''normal'"
$ ARQ_DEL = "popl*.FIL;*, clas*.fil;*,gext*.fil;*,golo*.fil;*,jker*.fil;*,lot*.FIL;*,text*.fil;*,tot*.fil;*"
$ if f$sear("draw:*.*") .nes. "" then -
         delete/exclu=*.dir/log/noconf/befor='date_old' draw:'ARQ_DEL'
$! if f$sear("savdsk:*.*") .nes. "" then -
        delete/exclu=*.dir/log/noconf/befor='date_old' savdsk:'ARQ_DEL'
$ Fim_Draw_e_Savdsk:
$!--------------------------------------------------------------------------
$ Sysx_integr:
$ Qt_dias = 14
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,Serão apagados todos os ficheiros exp/imp anteriores a ''date_old' "
$ Inquire/nopun ret "do disco ''bold'SYSX''normal' e ''bold'COD*.OLD*,*.ASC, ......''normal', Confirma [Y/N] [Y]: "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_Sysx_integr
$ SAY ""
$! SAY "%DELOLD-I,''bold'A apagar ficheiros antigos no disco DRAW e SAVDSK ...''normal'"
$ ARQ_DEL = "odj00*.*;*,off*.*;*,opsg*.*;*,orcpas*.*;*,paso*.*;*,pass*.*;*,cod*.old*;*,*.asc;*,lbo*.*;*,mtxagt*.fil;*,sapdrwamt_*.fil;*"
$!
$ delete/exclu=*.dir/log/conf/befor='date_old' SYSX:'ARQ_DEL' 
$!
$ Fim_Sysx_integr:
$!--------------------------------------------------------------------------
$ Sysx_integr:
$ Qt_dias = 30
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,Serão apagados todos os ficheiros exp/imp anteriores a ''date_old' "
$ Inquire/nopun ret "do disco ''bold'SYSX''normal' e ''bold'COD*.OLD*,*.ASC, ......''normal', Confirma [Y/N] [Y]: "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_Sysx_integr
$ SAY ""
$! SAY "%DELOLD-I,''bold'A apagar ficheiros antigos no disco DRAW e SAVDSK ...''normal'"
$ ARQ_DEL = "opsp*.lis;*,invl*.dat;*"
$!
$ delete/exclu=*.dir/log/conf/befor='date_old' SYSX:'ARQ_DEL' 
$!
$ Fim_Sysx_integr:
$!--------------------------------------------------------------------------
$ Sysx_integr:
$ Qt_dias = 180
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,Serão apagados todos os ficheiros exp/imp anteriores a ''date_old' "
$ Inquire/nopun ret "do disco ''bold'SYSX''normal' e ''bold'COD*.OLD*,*.ASC, ......''normal', Confirma [Y/N] [Y]: "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_Sysx_integr
$ SAY ""
$! SAY "%DELOLD-I,''bold'A apagar ficheiros antigos no disco DRAW e SAVDSK ...''normal'"
$ ARQ_DEL = "*cashops*.lis;*,*unops*.lis;*"
$!
$ delete/exclu=*.dir/log/conf/befor='date_old' SYSX:'ARQ_DEL' 
$!
$ Fim_Sysx_integr:
$!--------------------------------------------------------------------------
$ Elog:
$ Qt_dias = 7
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'Serão apagados todos os ficheiros de LOG anteriores a ''date_old' "
$ Inquire/nopun ret "do GXTSK, Confirma [Y/N] [Y]:''normal' "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_Elog
$ SAY ""
$ SAY "%DELOLD-I,A apagar ficheiros de LOG antigos  ..."
$ ARQ_DEL = "ELOG-*.LOG;*, MONGOLS-*.log;*, LIVE*.LOG;*"
$ delete/log/noconf/befor='date_old' GXTSK:'ARQ_DEL' 
$ Fim_Elog:
$!--------------------------------------------------------------------------
$ gsales:
$ Qt_dias = 1
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'Serão apagados todos os ficheiros de GSALES/VPF***_BEF.FIL anteriores a ''date_old' "
$ Inquire/nopun ret "do GXTSK, Confirma [Y/N] [Y]:''normal' "
$ if ret .eqs. "N" .or. ret .eqs. "n" then goto Fim_gsales
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros de GSALES antigos  ...''normal'"
$ ARQ_DEL = "200*.fin;*,200*.fil;*,*_BEF*.FIL;*"
$ delete/log/noconf/befor='date_old' VALX:'ARQ_DEL' 
$ Fim_gsales:
$!--------------------------------------------------------------------------
$ gxlog:
$ Qt_dias = 0
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros de GXLOG antigos  ...''normal'"
$ ARQ_DEL = "*.OLD;*"
$ delete/log/noconf/befor='date_old' GXLOG:'ARQ_DEL' 
$ RENAME/NOLOG/NOCONF GXLOG:*.LOG GXLOG:*.OLD
$ Fim_Elog:
$!----------------------------------------------------------------
$ Purg_file:
$ SET MESSAGE /NOFACIL/NOIDENT/NOSEVERITY/NOTEXT
$ SAY ""
$ SAY "%DELOLD-I,''bold'A executar PURGE nos ficheiros SYSX:*.DAT,OPS.FIL,SCF.FIL ...''normal'"
$ purg/nolog/noconf gxtsk:*.dat,ops.fil,scf.fil
$ SET MESSAGE /FACIL/IDENT/SEVERITY/TEXT
$!----------------------------------------------------------------
$ Qt_dias = 60
$ date	   = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos (''QT_DIAS' dias) da area SAVREP''normal'"
$ Arq_DEL = "*.*;*"
$ if f$sear("savrep:*.*") .nes. "" then -
  delete/log/NOconf/befor='date_old'/EXCLU=(*.dir,CSH*.*;*,PURWIN*.*;*) savrep:'arq_del' 
$!----------------------------------------------------------------
$ Qt_dias = 295
$ date     = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos (''QT_DIAS' dias) da area SYSX:TPF***.FIL''normal'"
$ Arq_DEL = "TPF*.FIL;*"
$ delete/log/conf/befor='date_old'/EXCLU=(*.dir) SYSX:'arq_del'
$!----------------------------------------------------------------
$ Qt_dias = 185
$ date     = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos (''QT_DIAS' dias) da area VALX:VPF***.FIL''normal'"
$ Arq_DEL = "VPF*.FIL;*"
$ delete/log/conf/befor='date_old'/EXCLU=(*.dir) VALX:'arq_del'
$!----------------------------------------------------------------
$ Qt_dias = 7
$ date     = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos (''QT_DIAS' dias) da area SAVREP:VIS_*.*;*''normal'" 
$ Arq_DEL = "VIS_*.*;*"
$ delete/log/conf/befor='date_old'/EXCLU=(*.dir) SAVREP:'arq_del'
$!----------------------------------------------------------------
$ Qt_dias = 365
$ date     = f$cvtime("today","comparison","date")
$ date_old = f$cvtime("today-''Qt_dias'-00:00:00","absolute","date")
$ SAY ""
$ SAY "%DELOLD-I,''bold'A apagar ficheiros antigos (''QT_DIAS' dias) da area SAVREP:CSH*.*, PURWIN*.*''normal'"
$ Arq_DEL = "CSH*.*;*,PURWIN*.*;*"
$ delete/log/conf/befor='date_old'/EXCLU=(*.dir) SAVREP:'arq_del'
$!----------------------------------------------------------------
$ Fim:
$ SAY ""
$ SAY "''bold'%DELOLD-I,Fim ...''normal'"
$ SAY ""
$ exit
