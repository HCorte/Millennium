$! documentation: http://www0.mi.infn.it/~calcolo/OpenVMS/ssb71/6489/6489p032.htm
$ OUT := "WRITE SYS$OUTPUT"
$ esc[0,8] = %x1b
$ bold 	   = "''esc'[1m"
$ normal   = "''esc'[0m"
$ blink    = "''esc'[5m"

$ green    = "''esc'[32m" 
$ red      = "''esc'[31m"
$ Cyan     = "''esc'[36m"
$ text="Text to be displayed"

$ OUT "Begin Installation Plan - New Terminals"
$ BASE_DISK = f$edit(F$TRNLNM("GXPROJ"),"collapse")
$ OUT BASE_DISK
$! validates that logical names exists and in correct tables
$ GXOLM_STATUS = f$edit(f$trnlnm("GXOLM","lnm$system"),"collapse")
$ OLMTSK_STATUS = f$edit(f$trnlnm("OLMTSK","lnm$system"),"collapse")
$ OLM_DMQCL_INI_FILE_STATUS = f$edit(f$trnlnm("OLM_DMQCL_INI_FILE","lnm$system"),"collapse")
$ OLM_ERR_LOG_STATUS = f$edit(f$trnlnm("OLM_ERR_LOG","lnm$system"),"collapse")
$ OLM_MESSQ_SEE_ATTACH_SCRIPT_STATUS = f$edit(f$trnlnm("OLM_MESSQ_SEE_ATTACH_SCRIPT","lnm$system"),"collapse")
$ OLM_MILCON_LOG_STATUS = f$edit(f$trnlnm("OLM_MILCON_LOG","lnm$system"),"collapse") 
$
$BEGINS:
$ CALL MENU
$ INQUIRE OPTION "Select the option to processed?"
$ OPTION = F$INTEGER(OPTION)
$! OPTION = F$INTEGER(OPTION) - F$INTEGER("0")
$! OUT F$INTEGER(OPTION)
$! OUT F$INTEGER("0")
$! OUT OPTION
$! GOTO (100,200,300) OPTION
$ IF OPTION .EQ. 1 THEN GOTO VALIDATE_PI 
$ IF OPTION .EQ. 2 THEN GOTO INSTALL_PI 
$ IF OPTION .EQ. 3 THEN GOTO GET_OUT 
$ GOTO INVALID
$
$!200:
$VALIDATE_PI:
$ CALL LOGICALEXISTS "''GXOLM_STATUS'" "GXPROJ:[TSK.OLM]" "GXOLM"
$! pipe write sys$output  "''esc'#3''GREEN'''text'''esc'#3"; write sys$output  "''esc'#4''GREEN'''text'''esc'#4"
$! IF (GXOLM_STATUS .EQS. "GXPROJ:[TSK.OLM]") 
$! THEN 
$!   OUT "GXOLM = ''esc'#6''GREEN'''GXOLM_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "GXOLM = ''esc'#6''RED'''GXOLM_STATUS' ''NORMAL'''esc'#6"
$! ENDIF


$ CALL LOGICALEXISTS "''OLMTSK_STATUS'" "GXPROJ:[TSK.OLM]" "GXTSK"
$! IF (OLMTSK_STATUS .EQS. "GXPROJ:[TSK.OLM]") 
$! THEN 
$!   OUT "GXTSK = ''esc'#6''GREEN'''OLMTSK_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "GXTSK = ''esc'#6''RED'''OLMTSK_STATUS' ''NORMAL'''esc'#6"
$! ENDIF


$ CALL LOGICALEXISTS "''OLM_DMQCL_INI_FILE_STATUS'" "GXOLM:DMQ.INI" "OLM_DMQCL_INI_FILE"

$! IF (OLM_DMQCL_INI_FILE_STATUS .EQS. "GXOLM:DMQ.INI") 
$! THEN 
$!   OUT "OLM_DMQCL_INI_FILE = ''esc'#6''GREEN'''OLM_DMQCL_INI_FILE_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "OLM_DMQCL_INI_FILE = ''esc'#6''RED'''OLM_DMQCL_INI_FILE_STATUS' ''NORMAL'''esc'#6"
$! ENDIF

$ CALL LOGICALEXISTS "''OLM_ERR_LOG_STATUS'" "GXOLM:ERR_MILLCON" "OLM_ERR_LOG"

$! IF (OLM_ERR_LOG_STATUS .EQS. "GXOLM:ERR_MILLCON") 
$! THEN 
$!   OUT "OLM_ERR_LOG = ''esc'#6''GREEN'''OLM_ERR_LOG_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "OLM_ERR_LOG = ''esc'#6''RED'''OLM_ERR_LOG_STATUS' ''NORMAL'''esc'#6"
$! ENDIF


$ CALL LOGICALEXISTS "''OLM_MESSQ_SEE_ATTACH_SCRIPT_STATUS'" "GXOLM:MESSAGEQCONNECTION.COM" "OLM_MESSQ_SEE_ATTACH_SCRIPT"

$! IF (OLM_MESSQ_SEE_ATTACH_SCRIPT_STATUS .EQS. "GXOLM:MESSAGEQCONNECTION.COM") 
$! THEN 
$!   OUT "OLM_MESSQ_SEE_ATTACH_SCRIPT = ''esc'#6''GREEN'''OLM_MESSQ_SEE_ATTACH_SCRIPT_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "OLM_MESSQ_SEE_ATTACH_SCRIPT = ''esc'#6''RED'''OLM_MESSQ_SEE_ATTACH_SCRIPT_STATUS' ''NORMAL'''esc'#6"
$! ENDIF

$ CALL LOGICALEXISTS "''OLM_MILCON_LOG_STATUS'" "GXOLM:MILLCON" "OLM_MILCON_LOG"

$! IF (OLM_MILCON_LOG_STATUS .EQS. "GXOLM:MILLCON") 
$! THEN 
$!   OUT "OLM_MILCON_LOG = ''esc'#6''GREEN'''OLM_MILCON_LOG_STATUS' ''NORMAL'''esc'#6"
$! ELSE
$!   OUT "OLM_MILCON_LOG = ''esc'#6''RED'''OLM_MILCON_LOG_STATUS' ''NORMAL'''esc'#6"
$! ENDIF
$

$
$ CALL FILE_EXISTS "GXOLM:COMOLM.EXE;0"
$! file_comolm = F$SEARCH("GXOLM:COMOLM.EXE;0")
$! IF file_comolm .EQS. "" 
$! THEN 
$!   OUT "'RED''COMOLM FILE MISSING IN GXOLM''NORMAL'"
$! ELSE
$!   OUT "COMOLM.EXE EXISTS: ''GREEN'''file_comolm'''NORMAL'"
$! ENDIF
$
$ CALL FILE_EXISTS "GXOLM:DMQ.INI;0"
$! file_dqmini = F$SEARCH("GXOLM:DMQ.INI;0")
$! IF file_dqmini .EQS. "" 
$! THEN 
$!   OUT "'RED''COMOLM FILE MISSING IN GXOLM''NORMAL'"
$! ELSE
$!   OUT "DMQ.INI EXISTS: ''GREEN'''file_dqmini'''NORMAL'"
$! ENDIF
$
$ CALL FILE_EXISTS "MESSAGEQCONNECTION.COM;0"
$! file_messcon = F$SEARCH("MESSAGEQCONNECTION.COM;0")
$! IF file_messcon .EQS. "" 
$! THEN 
$!   OUT "'RED''COMOLM FILE MISSING IN GXOLM''NORMAL'"
$! ELSE
$!   OUT "MESSAGEQCONNECTION.COM EXISTS: ''GREEN'''file_messcon'''NORMAL'"
$! ENDIF

$! SHOW SYMBOL file_comolm
$!
$! OUT F$LENGTH(GXOLM_STATUS)
$! Directory the Script/command procedure as been run
$ CURRENT_DIR = F$DIRECTORY()
$ OUT "Script run at: ''CURRENT_DIR'"
$ 
$! first step confirm that OLM directory exists inside TSK dir
$! OLM_DIR_EXISTS = F$SEARCH('GXOLM')
$ OUT "End Installation Plan - New Terminals"
$ EXIT 1
$!100:
$INSTALL_PI:
$
$ INQUIRE CONFIRM "ARE YOU SHURE YOU WANT TO PROCESSED WITH SCRIPT INSTALLATION (Y/N)" 
$ IF CONFIRM .EQ. "Y" .OR. CONFIRM .EQ. "y" 
$ THEN
$   OUT "PROCEDING..."
$ ELSE
$   GOTO BEGINS
$ ENDIF
$ EXIT 1
$ 
$!300: 
$GET_OUT:
$ OUT "Exiting Script..."
$ EXIT 1
$
$INVALID:
$ OUT "Invalid Option, try again"
$ GOTO BEGINS
$
$ FILE_EXISTS:SUBROUTINE
$ !------------------------------------------------------------------------------------------------
$   file_search = F$SEARCH(P1)
$   IF file_search .EQS. "" 
$   THEN 
$     OUT "''RED' ''file_search' FILE MISSING ''NORMAL'"
$   ELSE
$     OUT "''GREEN'''file_search' EXISTS''NORMAL'"
$   ENDIF
$ ENDSUBROUTINE
$
$
$ LOGICALEXISTS:SUBROUTINE
$    IF (P1 .EQS. P2) 
$    THEN 
$      OUT "''P3' = ''esc'#6''GREEN'''P1' ''NORMAL'''esc'#6"
$    ELSE
$      OUT "''P3' = ''esc'#6''RED'''P1' ''NORMAL'''esc'#6"
$    ENDIF
$ ENDSUBROUTINE
$
$ MENU:SUBROUTINE
$ OUT " 1 - VALIDATE IF INSTALLED CORRECTED THE NEW TERMINALS PI"
$ OUT " 2 - INSTALL BY THE SCRIPT THE NEW TERMINALS PI"
$ OUT " 3 - EXIT"
$ ENDSUBROUTINE
$
$! END:

