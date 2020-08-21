$ Inicio:                                                    
$ SAY :== "WRITE SYS$OUTPUT"
$ esc[0,8] = %x1b
$ bold 	   = "''esc'[1m"
$ blink    = "''esc'[5m"
$ normal   = "''esc'[0m"
$ wsys := "Millennium"
$ ERRO:=FALSE
$ set def gxtsk
$
$ set on
$ on control_Y then goto alldone
$ if f$getdvi("tt:","devnam") .nes. "_OPA0:" then set term/wid=80
$ ON ERROR THEN CONTINUE
$
$ get_arq:
$ say "------------------------------------------------------"
$ say "''bold'JOGOS SANTA CASA - ''f$trn("sys$node")'"
$ say "FTP-RELAT: Transferência de Relatórios''normal' "
$ say "------------------------------------------------------"
$ say  "''bold'1 -''normal' PREVISAO (Loto;Joker / Loto2 / Totobola / Totobola Extra1)"   
$ say  "''bold'2 -''normal' PLANOREAL ; PREMIOS  (Clássica / Popular)" 
$ say  "''bold'3 -''normal' AUDITRPT"
$ say  "''bold'4 -''normal' SHAREs / PRGNOTOP"
$ say  "''bold'5 -''normal' CONCs ; INVLs"
$ say  "''bold'6 -''normal' UNPAID_OPS / REL5000"
$ say  "''bold'7 -''normal' CSHWEEK" 
$ say  "''bold'8 -''normal' INVRPTOFF"
$ say  "''bold'9 -''normal' VENCIMENTOS"
$ say  "''bold'10-''normal' TELAS VISION"
$ say  "''bold'11-''normal' AJUSTES"
$ say  "''bold'12-''normal' EXPEDIÇÃO_OPS"
$ say  "''bold'13-''normal' INTEGRAÇÃO / BNKRPT"
$ say  "" 
$ say  "''bold'E -''normal' EXIT" 
$ say ""
$ inquire/nopun px "''bold'Entre com o numero da opção ou pressione <E> p/exit: ''normal'"
$ if px .eqs. "E" .or. px .eqs. "e" then exit
$ if f$int(px) .eq. 0 .or. px .gt. 13 then goto inicio
$ gosub 'px'
$ goto inicio
$!
$!
$!---------------------------------------------------------------------------
$! PREVISAO
$!---------------------------------------------------------------------------
$!
$ 1:
$  data     = f$cvtime("today","comparison","date")
$  Data_ant = f$cvtime("today-1-00:00:00","absolute","date")
$  say  "" 
$  IF F$SEAR("GXTSK:PREVISAO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO PREVISAO **"
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  RETURN
$  ELSE
$  SAY "''blink'EXISTE RELATORIO PREVISAO''normal'"
$  say  ""
$  ENDIF
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF a .NES. "E" .AND. A .NES. "e"
$  then 
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;* ENDIF
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  WRITE XFTPRELAT "put PREVISAO.REP PREVISAO_''Data_ant'.TXT"
$  WRITE XFTPRELAT "close"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  endif
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! PLANOREAL ; PREMIOS
$!---------------------------------------------------------------------------
$!
$ 2:
$  EXISTE=0
$  data     = f$cvtime("today","comparison","date")
$  say  ""
$  IF F$SEAR("GXTSK:PLANOREAL_POPL.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO PLANO REAL DA LOTARIA POPULAR **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO PLANO REAL DA LOTARIA POPULAR''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:PLANOREAL_CLAS.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO PLANO REAL DA LOTARIA CLASSICA **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO PLANO REAL DA LOTARIA CLASSICA''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:1_PREMIO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO DO 1_PREMIO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO DO 1_PREMIO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:2_PREMIO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO DO 2_PREMIO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO DO 2_PREMIO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:3_PREMIO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO DO 3_PREMIO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO DO 3_PREMIO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:4_PREMIO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO DO 4_PREMIO ** (Só gerado na Lotaria Popular)"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO DO 4_PREMIO''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  THEN
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:PLANOREAL_POPL.REP") .NES. "" THEN WRITE XFTPRELAT "put PLANOREAL_POPL.REP PLANOREAL_POPL_''data'.TXT"
$  IF F$SEAR("GXTSK:PLANOREAL_CLAS.REP") .NES. "" THEN WRITE XFTPRELAT "put PLANOREAL_CLAS.REP PLANOREAL_CLAS_''data'.TXT"
$  IF F$SEAR("GXTSK:1_PREMIO.REP") .NES. "" THEN WRITE XFTPRELAT "put 1_PREMIO.REP PREMIO_1_''data'.TXT"
$  IF F$SEAR("GXTSK:2_PREMIO.REP") .NES. "" THEN WRITE XFTPRELAT "put 2_PREMIO.REP PREMIO_2_''data'.TXT"
$  IF F$SEAR("GXTSK:3_PREMIO.REP") .NES. "" THEN WRITE XFTPRELAT "put 3_PREMIO.REP PREMIO_3_''data'.TXT"
$  IF F$SEAR("GXTSK:4_PREMIO.REP") .NES. "" THEN WRITE XFTPRELAT "put 4_PREMIO.REP PREMIO_4_''data'.TXT"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! AUDITRPT
$!---------------------------------------------------------------------------
$!
$ 3:
$  say  ""
$  EXISTE=0
$  IF F$SEAR("GXTSK:AUDITRPT_A*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO AUDITRPT_A **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO AUDITRPT_A''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:AUDITRPT_B*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO AUDITRPT_B **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO AUDITRPT_B''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:AUDITRPT_C*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO AUDITRPT_C **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO AUDITRPT_C''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;* 
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:AUDITRPT_A*.REP") .NES. "" THEN WRITE XFTPRELAT "mput AUDITRPT_A*.REP"
$  IF F$SEAR("GXTSK:AUDITRPT_B*.REP") .NES. "" THEN WRITE XFTPRELAT "mput AUDITRPT_B*.REP"
$  IF F$SEAR("GXTSK:AUDITRPT_C*.REP") .NES. "" THEN WRITE XFTPRELAT "mput AUDITRPT_C*.REP"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! SHAREs
$!---------------------------------------------------------------------------
$!
$ 4:
$  EXISTE=0
$  data     = f$cvtime("today","comparison","date")
$  Data_ant = f$cvtime("today-1-00:00:00","absolute","date")
$  say  ""
$  IF F$SEAR("GXTSK:LO1SHARE.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO LO1SHARE **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO LO1SHARE''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:LO2SHARE.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO LO2SHARE **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO LO2SHARE''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:JO1SHARE.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO JO1SHARE **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO JO1SHARE''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:TB1SHARE.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO TB1SHARE **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO TB1SHARE''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:TB3SHARE.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO TB3SHARE **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO TB3SHARE''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:PRGNOTOP.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO PRGNOTOP **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO PRGNOTOP''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:LO1SHARE.REP") .NES. "" THEN WRITE XFTPRELAT "put LO1SHARE.REP LO1SHARE_''data'.TXT"
$  IF F$SEAR("GXTSK:LO2SHARE.REP") .NES. "" THEN WRITE XFTPRELAT "put LO2SHARE.REP LO2SHARE_''data'.TXT"
$  IF F$SEAR("GXTSK:JO1SHARE.REP") .NES. "" THEN WRITE XFTPRELAT "put JO1SHARE.REP JO1SHARE_''data'.TXT"
$  IF F$SEAR("GXTSK:TB1SHARE.REP") .NES. "" THEN WRITE XFTPRELAT "put TB1SHARE.REP TB1SHARE_''data'.TXT"
$  IF F$SEAR("GXTSK:TB3SHARE.REP") .NES. "" THEN WRITE XFTPRELAT "put TB3SHARE.REP TB3SHARE_''data'.TXT"
$  IF F$SEAR("GXTSK:PRGNOTOP.REP") .NES. "" THEN WRITE XFTPRELAT "put PRGNOTOP.REP PRGNOTOP_''data'.TXT"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! CONCs ; INVLs
$!---------------------------------------------------------------------------
$!
$ 5:
$  EXISTE=0
$  data     = f$cvtime("today","comparison","date")
$  say  ""
$  IF F$SEAR("GXTSK:L1CONC.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO L1CONC **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO L1CONC''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:L2CONC.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO L2CONC **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO L2CONC''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:J1CONC.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO J1CONC **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO J1CONC''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:S1CONC.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO S1CONC **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO S1CONC''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:S3CONC.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO S3CONC **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO S3CONC''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:INVLLOTO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INVLLOTO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INVLLOTO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:INVLLOT2.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INVLLOT2 **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INVLLOT2''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:INVLTOTO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INVLTOTO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INVLTOTO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:INVLTOT1.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INVLTOT1 **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INVLTOT1''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:INVLLOTOS_*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INVLLOTOS ** (Relatorio Totoloto com Loto2)"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INVLLOTOS''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:L1CONC.REP") .NES. "" THEN WRITE XFTPRELAT "put L1CONC.REP L1CONC_''data'.TXT"
$  IF F$SEAR("GXTSK:L2CONC.REP") .NES. "" THEN WRITE XFTPRELAT "put L2CONC.REP L2CONC_''data'.TXT"
$  IF F$SEAR("GXTSK:J1CONC.REP") .NES. "" THEN WRITE XFTPRELAT "put J1CONC.REP J1CONC_''data'.TXT"
$  IF F$SEAR("GXTSK:S1CONC.REP") .NES. "" THEN WRITE XFTPRELAT "put S1CONC.REP S1CONC_''data'.TXT"
$  IF F$SEAR("GXTSK:S3CONC.REP") .NES. "" THEN WRITE XFTPRELAT "put S3CONC.REP S3CONC_''data'.TXT"
$  IF F$SEAR("GXTSK:INVLLOTO.REP") .NES. "" THEN WRITE XFTPRELAT "put INVLLOTO.REP INVLLOTO_''data'.TXT"
$  IF F$SEAR("GXTSK:INVLLOT2.REP") .NES. "" THEN WRITE XFTPRELAT "put INVLLOT2.REP INVLLOT2_''data'.TXT"
$  IF F$SEAR("GXTSK:INVLTOTO.REP") .NES. "" THEN WRITE XFTPRELAT "put INVLTOTO.REP INVLTOTO_''data'.TXT"
$  IF F$SEAR("GXTSK:INVLTOT1.REP") .NES. "" THEN WRITE XFTPRELAT "put INVLTOT1.REP INVLTOT1_''data'.TXT"
$  IF F$SEAR("GXTSK:INVLLOTOS_*.REP") .NES. "" THEN WRITE XFTPRELAT "mput INVLLOTOS_*.REP"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT                            
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! UNPAID_OPS / REL5000
$!---------------------------------------------------------------------------
$!
$ 6:
$  EXISTE=0
$  say  ""
$  IF F$SEAR("GXTSK:UNPAID_OPS_*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO UNPAID_OPS **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO UNPAID_OPS''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:REL5000_*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO REL5000 **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO REL5000''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:UNPAID_OPS_*.REP") .NES. "" THEN WRITE XFTPRELAT "mput UNPAID_OPS_*.REP"
$  IF F$SEAR("GXTSK:REL5000*.REP") .NES. "" THEN WRITE XFTPRELAT "mput REL5000_*.REP"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! CSHWEEK
$!---------------------------------------------------------------------------
$!
$ 7:
$  say  ""
$  IF F$SEAR("GXTSK:CSHWEEK_*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO CSHWEEK **"
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  RETURN
$  ELSE
$  SAY "''blink'EXISTE RELATORIO CSHWEEK''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164"
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:CSHWEEK_*.REP") .NES. "" THEN WRITE XFTPRELAT "mput CSHWEEK_*.REP"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ENDIF
$  RETURN
$!
$!
$!---------------------------------------------------------------------------
$! INVRPTOFF
$!---------------------------------------------------------------------------
$!
$ 8:
$    IF F$SEAR("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$    IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;* ENDIF
$    OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$    WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$    WRITE XFTPRELAT "openftp"
$    WRITE XFTPRELAT "open 10.8.175.154"
$    WRITE XFTPRELAT "user ftpmillennium"
$    WRITE XFTPRELAT "millennium"
$    WRITE XFTPRELAT "cd EUROM"
$    WRITE XFTPRELAT "get INVRPTOFF.REP"
$    WRITE XFTPRELAT "close"
$    WRITE XFTPRELAT "open 123.0.5.20"
$    WRITE XFTPRELAT "user oce"
$    WRITE XFTPRELAT "oce"
$    WRITE XFTPRELAT "put INVRPTOFF.REP"
$    WRITE XFTPRELAT "bye"
$    CLOSE XFTPRELAT
$    @GXCOM:XFTPRELAT
$    inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$    DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$    RETURN
$!
$!
$!---------------------------------------------------------------------------
$! VENCIMENTOS
$!---------------------------------------------------------------------------
$!
$ 9:
$    IF F$SEAR("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$    IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;* ENDIF
$    OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$    WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$    WRITE XFTPRELAT "openftp"
$    WRITE XFTPRELAT "open 10.8.175.164"
$    WRITE XFTPRELAT "user ftprelatorios"
$    WRITE XFTPRELAT "relatorios"
$    WRITE XFTPRELAT "cd GESVEN"
$    WRITE XFTPRELAT "mget GVSRC0201.PRN*"
$    WRITE XFTPRELAT "bye"
$    CLOSE XFTPRELAT
$    @GXCOM:XFTPRELAT
$    inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$    DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$    RETURN
$!
$!
$!---------------------------------------------------------------------------
$! TELAS
$!---------------------------------------------------------------------------
$!
$ 10:
$  say  ""
$  IF F$SEAR("GXTSK:VIS_*.REP") .EQS. ""
$    THEN SAY "** NAO EXISTEM TELAS DO VISION **"
$    say  ""
$    inquire/nopun aa "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$    RETURN
$  ELSE
$    SAY "''blink'EXISTEM TELAS DO VISION''normal'"
$    say  ""
$  ENDIF
$  inquire/nopun aa "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF AA .NES. "E" .AND. AA .NES. "e"
$    then
$    inquire/nopun SISTEMA "''bold'Indique o Sistema (A/B/C) e <return> Para Continuar: ''normal'"
$    say  ""
$    inquire/nopun CONF "''bold'Quer continuar (Y/N): ''normal'"
$    say  ""
$    IF CONF .NES. "Y" .AND. CONF .NES. "y" then GOTO INICIO:
$    IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$    IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$    OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$    WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$    WRITE XFTPRELAT "openftp"
$    WRITE XFTPRELAT "open 10.8.175.166"
$    WRITE XFTPRELAT "user ftptelas"
$    WRITE XFTPRELAT "telas"
$    WRITE XFTPRELAT "cd ''SISTEMA'"
$    IF F$SEAR("GXTSK:VIS_*.REP") .NES. "" THEN WRITE XFTPRELAT "mput VIS_*.REP;*"
$    WRITE XFTPRELAT "bye"
$    CLOSE XFTPRELAT
$    @GXCOM:XFTPRELAT
$    inquire/nopun C "''bold'Pressione <return> p/sair: ''normal'"
$    DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ENDIF
$  RETURN
$!
$!
$!
$!---------------------------------------------------------------------------
$! AJUSTES
$!---------------------------------------------------------------------------
$!
$ 11:
$  say  ""
$  IF F$SEAR("GXTSK:AJUSTES*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO AJUSTES **"
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  RETURN
$  ELSE
$  SAY "''blink'EXISTE RELATORIO AJUSTES''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  inquire/nopun SEMANA "''bold'Introduza a Semana <SS>  : ''normal'"
$  say "Vai transferir o ficheiro AJUSTES''semana'.TXT"
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164"
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:AJUSTES*.REP") .NES. "" THEN WRITE XFTPRELAT "put AJUSTES.REP AJUSTES''SEMANA'.TXT"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ENDIF
$  RETURN
$!
$!
$!
$!---------------------------------------------------------------------------
$! EXPEDIÇAO_OPS
$!---------------------------------------------------------------------------
$!
$ 12:
$ say ""
$ say "------------------------------------------------------"
$ say "''bold'JOGOS SANTA CASA - ''f$trn("sys$node")'"
$ say "FTP-RELAT: 12-EXPEDIÇAO_OPS''normal' "
$ say "------------------------------------------------------"
$ say  "''bold'1 -''normal' IMPORTACAO"   
$ say  "''bold'2 -''normal' EXPORTACAO" 
$ say  "" 
$ say  "''bold'E -''normal' EXIT" 
$ say ""
$ inquire/nopun px1 "''bold'Entre com o numero da opção ou pressione <E> p/exit: ''normal'"
$ if px1 .eqs. "E" .or. px1 .eqs. "e" then goto inicio
$ if f$int(px1) .eq. 0 .or. px1 .gt. 2 then goto 12
$ if f$int(px1) .eq. 1 then gosub 12a
$ if f$int(px1) .eq. 2 then gosub 12b
$  RETURN
$
$ 12a:
$ 
$    IF F$SEAR("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT ENDIF
$    IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;* ENDIF
$    OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$    WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$    WRITE XFTPRELAT "openftp"
$    WRITE XFTPRELAT "open 10.8.175.154"
$    WRITE XFTPRELAT "user ftpmillennium"
$    WRITE XFTPRELAT "millennium"
$    WRITE XFTPRELAT "cd EUROM"
$    WRITE XFTPRELAT "mget EM_OPS_EXP_*.ASC"
$    WRITE XFTPRELAT "bye"
$    CLOSE XFTPRELAT
$    @GXCOM:XFTPRELAT
$    inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$    DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$    RETURN
$
$ 12b: 
$  EXISTE=0
$  say  ""
$  IF F$SEAR("GXTSK:EXP_OPS_*.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO EXP_OPS_AAAASS.REP **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO EXP_OPS_AAAASS.REP''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:EXP_OPS_*.TXT") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO EXP_OPS_AAAASS.TXT **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO EXP_OPS_AAAASS.TXT''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE .EQ. 1
$  THEN
$    IF a .NES. "E" .AND. A .NES. "e"
$    then
$      IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$      IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$      OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$      WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$      WRITE XFTPRELAT "openftp"
$      WRITE XFTPRELAT "open 10.8.175.164" 
$      WRITE XFTPRELAT "user ftprelatorios"
$      WRITE XFTPRELAT "relatorios"
$      WRITE XFTPRELAT "cd MILLENNIUM"
$      WRITE XFTPRELAT "type ascii"
$      IF F$SEAR("GXTSK:EXP_OPS_*.REP") .NES. "" THEN WRITE XFTPRELAT "mput EXP_OPS_*.REP"
$      IF F$SEAR("GXTSK:EXP_OPS_*.TXT") .NES. "" THEN WRITE XFTPRELAT "mput EXP_OPS_*.TXT"
$      WRITE XFTPRELAT "bye"
$      CLOSE XFTPRELAT
$      @GXCOM:XFTPRELAT
$      inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$      DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$    ENDIF
$  ENDIF
$  RETURN
$!
$!
$!
$!---------------------------------------------------------------------------
$! INTEGRAÇÃO / BNKRPT
$!---------------------------------------------------------------------------
$!
$ 13:
$  EXISTE=0
$  say  ""
$  IF F$SEAR("GXTSK:INTEGRACAO.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO INTEGRAÇÃO **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO INTEGRAÇÃO''normal'"
$  ENDIF
$  IF F$SEAR("GXTSK:BNKRPT.REP") .EQS. ""
$  THEN SAY "** NAO EXISTE RELATORIO BNKRPT **"
$  ELSE
$  EXISTE=1
$  SAY "''blink'EXISTE RELATORIO BNKRPT''normal'"
$  ENDIF
$  say  ""
$  inquire/nopun a "''bold'Pressione <return> Para Continuar  ou  <E> Para Exit: ''normal'"
$  say  ""
$  IF EXISTE.EQ.1
$  THEN
$  IF a .NES. "E" .AND. A .NES. "e"
$  then
$  IF F$TRNL("XPTPRELAT") .NES. "" THEN CLOSE XFTPRELAT
$  IF F$SEAR("GXCOM:XFTPRELAT.COM") .NES. "" THEN DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  OPEN/WRITE XFTPRELAT GXCOM:XFTPRELAT.COM
$  WRITE XFTPRELAT "$ openftp :== $openvms$ftp"
$  WRITE XFTPRELAT "openftp"
$  WRITE XFTPRELAT "open 10.8.175.164" 
$  WRITE XFTPRELAT "user ftprelatorios"
$  WRITE XFTPRELAT "relatorios"
$  WRITE XFTPRELAT "cd MILLENNIUM"
$  IF F$SEAR("GXTSK:INTEGRACAO.REP") .NES. "" THEN WRITE XFTPRELAT "put INTEGRACAO.REP INTEGRACAO.TXT"
$  IF F$SEAR("GXTSK:BNKRPT.REP") .NES. "" THEN WRITE XFTPRELAT "put BNKRPT.REP BNKRPT.TXT"
$  WRITE XFTPRELAT "bye"
$  CLOSE XFTPRELAT
$  @GXCOM:XFTPRELAT
$  inquire/nopun a "''bold'Pressione <return> p/sair: ''normal'"
$  DELETE/NOLOG GXCOM:XFTPRELAT.COM;*
$  ELSE
$  RETURN
$  ENDIF
$  ENDIF
$  RETURN
$!
$!
$!
$
                                               
                                                     