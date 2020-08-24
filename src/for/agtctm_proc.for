C
C PROGRAM AGTCTM_PROC
C $Log:   GXAFXT:[GOLS]AGTCTM_PROC.FOV  $

C ** Source - AGTCTM_PROC.FOR;1 **
C
C AGTCTM_PROC.FOR
C
C V03 15-MAR-2011 GPW NUMAGT=12288
C V02 20-OCT-2009 CANCELLATION TIME COMMAND SENT TO ALL SYSTEMS.
C V01 30-OCT-2008
C
C This program disables the parameter cancellation time of all agents
C that have the parameter enabled. A report file named 
C RELAT_PRAZO_CANCEL_<CDC>.REP is generated and contains the list of the
C agents whose parameter cancellation time was successfully disabled.
C If some parameter cancellation time was not possible to disable, 
C the related info of the agent is written to the error report file 
C named RELAT_PRAZO_CANCEL_<CDC>.ERR.
C
C 1. Builds the report name (template: RELAT_PRAZO_CANCEL_<CDC>.REP)
C 2. Searches for agents with the cancellation time parameter enabled
C    2.1 Writes agent related info into report file if agent found
C    2.2 Writes the total number
C 3. Asks the user if he wants to disable the cancellation time
C    parameter if agents were found
C	 3.1 Cancellation granted - updates memory and ASF file, and
C		reports the number of cancellations succeeded into report 
C		file. If an error occurs during cancellation, related
C		info is reported to RELAT_PRAZO_CANCEL_<CDC>.ERR: total
C		info number of failures and agent.
C	 3.2 Cancellation not granted - memory and ASF file update is
C		not done. Report file is updated.
C 4. End of Program
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C

C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM AGTCTM_PROC
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:X2XPRM.DEF'
!	INCLUDE 'INCLIB:VISCOM.DEF'

	INTEGER*4	LIST(NUMAGT), ERRLIST(NUMAGT)
	INTEGER*4	I,J,K,N
	INTEGER*4	ST,COUNT,TERNBR
	INTEGER*4	BITMAP
	INTEGER*4	FLAG,LUN

!	CHARACTER*11 PROGNAME/'AGTCTM_PROC'/
!	INTEGER*4 	CTIM(2)

!	INTEGER*4 	PAGE,REV
	INTEGER*2	VDAT(LDATE_LEN)
	INTEGER*4 	CTIM(2),CDAT(8),SYS,WEEK,YEAR
!	CHARACTER*47 HEAD/'C A N C E L L A T I O N   T I M E   R E P O R T'/
	CHARACTER*55 HEAD/'R E L A T O R I O   P R A Z O   C A N C E L A M E N T O'/
	CHARACTER*71 HEAD2/'R E L A T O R I O   D E   E R R O   P R A Z O   C A N C E L A M E N T O'/
!
	CHARACTER*27 REPNAME ! REPORT NAME
	CHARACTER*27 ERREPNAME ! FILE NAME FOR REPORTING ERRORS
!	CHARACTER*90 CLIN1(1)
!
!	CHARACTER	FF
	CHARACTER	SYSID(6)/'?','A','B','C','D','E'/
	CHARACTER	CASFBYT*760
	EQUIVALENCE (CASFBYT,ASFBYT)    !ASFBYT is equivalent to ASFINF
	DATA SYSID/'?','A','B','C','D','E'/
!	DATA FF/Z31/

!V02
	INTEGER*4  BUF(CDLEN)
!V02

C
	COUNT=0
	N=0

	CALL FASTSET(0,LIST,NUMAGT)
	CALL FASTSET(0,ERRLIST,NUMAGT)
!V02
	CALL FASTSET(0,BUF,CDLEN)
!V02

	CALL CLRSCR(5) ! CLEARS THE SCREEN
	CALL FIND_AVAILABLE_LUN(LUN,ST) ! FINDS AN AVAILABLE LOGICAL UNIT
	IF (ST.NE.0) THEN
		TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT TO OPEN FILE'
		CALL GSTOP(GEXIT_FATAL)
	ENDIF

!	TYPE*,IAM(),'*** Beginning Cancellation Time Processing'
	TYPE*,IAM(),'*** A iniciar o processo de cancelamento'
	WRITE (REPNAME,908) DAYCDC ! BUILD REPORT NAME

	OPEN(LUN,FILE=REPNAME,STATUS='NEW')

!	*********** HEADER BUILDING - BEGIN *************
	VDAT(VCDC)=DAYCDC
	CALL LCDATE(VDAT)
	CALL FIGWEK(DAYCDC,WEEK,YEAR)

	SYS=P(SYSNAM)+1
	IF(SYS.LT.1.OR.SYS.GT.6) SYS=1

	CALL ICLOCK(1,CTIM)
	CALL GDATE(CDAT(2),CDAT(3),CDAT(1))
        IF(CDAT(1).LT.77) THEN
	  CDAT(1) = CDAT(1) + 2000
	ELSE
	  CDAT(1) = CDAT(1) + 1900
	ENDIF

!	WRITE(LUN,900) FF,PROGNAME,1,CTIM
	WRITE(LUN,900) CTIM
	WRITE(LUN,901) CDAT(3),CDAT(2),CDAT(1),HEAD,(VDAT(K),K=9,13),VDAT(VCDC),SYSID(SYS)
!	********* HEADER BUILDING - END ********

	WRITE(LUN,899)
	CALL OPENASF(ASF)
!	TYPE*,IAM(),'*** Searching ASF file ...'
	TYPE*,IAM(),'*** A ler o ficheiro ASF ...'
	DO 101 TERNBR=1,NUMAGT
		CALL READASF(TERNBR,ASFREC,ST)
		IF(ST.NE.0) THEN
			CLOSE(LUN)
			TYPE*,'ASF.FIL READ ERROR ',ST,' TERMINAL ',LIST(I)
			CALL CLOSASF
			CALL GSTOP(GEXIT_FATAL)
		ENDIF

		BITMAP=AGTTAB(AGTTYP,TERNBR)
		IF(TSBIT(BITMAP,AGTCTM)) THEN
			COUNT=COUNT+1
			LIST(COUNT)=TERNBR
			WRITE(LUN,FMT=906) (ASFBYT(I),I=SAGNO,EAGNO),TERNBR,(ASFBYT(J),J=SNAME,ENAME)
!			WRITE(CLIN1,906) (ASFBYT(I),I=SAGNO,EAGNO),TERNBR,(ASFBYT(J),J=SNAME,ENAME)
!			TYPE*,CLIN1
		ENDIF

101	    CONTINUE
	CALL CLOSASF

	J=0
	K=0
	IF (COUNT.GT.0) THEN ! AGENTS WITH CANCELLATION TIME ENABLED FOUND
!		TYPE*,IAM(),'*** Agents with cancellation time enabled found!'
		TYPE*,IAM(),'*** Foram encontrados prazos de cancelamento em aberto!'
		CALL INPYESNO('*** Fechar prazos de cancelamento? [Y/N]',FLAG,ST)

		IF (ST.EQ.-1) THEN
			CLOSE(LUN)
			CALL CLOSASF
			CALL GSTOP(GEXIT_OPABORT)
		ENDIF

		IF (FLAG.EQ.1) THEN
!		*********  BEGIN - AGENT CANCELLATION TIME  ************
			CALL OPENASF(ASF)
			DO 102 I=1,COUNT ! LIST(I) => TERMINAL NUMBER WITH CANCELLATION TIME ENABLED
				CALL READASF(LIST(I),ASFREC,ST)
				
				IF(ST.NE.0) THEN
					ERRLIST(J+1)=LIST(I)
					J=J+1
					TYPE*,'ASF.FIL READ ERROR ',ST,' TERMINAL ',LIST(I)
					GOTO 102
				ENDIF

				BITMAP=AGTTAB(AGTTYP,LIST(I))
				CALL BCLR(BITMAP,AGTCTM) ! DISABLE AGENT CANCELLATION TIME
!V01
!				AGTTAB(AGTTYP,LIST(I))=BITMAP
!
!				CALL WRITASF(LIST(I),ASFREC,ST) ! WRITE CHANGES TO ASF
!
!				IF(ST.NE.0) THEN
!					ERRLIST(J+1)=LIST(I)
!					J=J+1
!					TYPE*,'ASF.FIL WRITE ERROR ',ST,' TERMINAL ',LIST(I)
!					GOTO 102
!				ENDIF
!
!				K=K+1 ! COUNTS THE OK CANCELLATIONS
!V01

!V02 (SENDS A COMMAND TO DISABLE THE AGENT CANCELLATION TIME FLAG)
! COMMAND BUFFER FORMAT
!
! WORD                CONTENTS
!   1                 COMMAND NUMBER
!   2                 COMMAND NEW VALUE
!   3                 COMMAND TYPE
!   4                 COMMAND LINE
!   5                 COMMAND TERMINAL
!   6                 COMMAND SOURCE
!   7                 COMMAND AGENT
!   8                 COMMAND DATA 1
!   9                 COMMAND DATA 2
!  10                 COMMAND DATA 3
!  11                 COMMAND DATA 4
!  12                 COMMAND DATA 5
				BUF(1)=3
				BUF(2)=BITMAP
				BUF(3)=TCAGT
				BUF(5)=LIST(I) ! TERMINAL NUMBER TO DISABLE ITS CANCELLATION TIME FLAG
				CALL QUECMD(BUF,ST)
				IF(ST.NE.0) THEN
					ERRLIST(J+1)=LIST(I)
					J=J+1
					TYPE*,' QUEUE COMMAND BUFFER ERROR ',ST,' TERMINAL ',LIST(I)
					GOTO 102
				ENDIF				
				CALL XWAIT(2,1,ST) ! WAITS 2 MILLISECONDS
				K=K+1 ! COUNTS THE OK CANCELLATIONS
!V02

102			CONTINUE
			CALL CLOSASF
!			TYPE*,IAM(),'*** Cancellation Time Processing complete'
			TYPE*,IAM(),'*** Processo de cancelamento terminado'
!		*********  END - AGENT CANCELLATION TIME  ************
		ENDIF      
               
		IF (FLAG.EQ.2) THEN
!			TYPE*,IAM(),'*** Cancellation Time Processing Cancelled'
			TYPE*,IAM(),'*** Cancelamento de prazos nao efectuado'
		ENDIF

		IF (FLAG.EQ.3) THEN
!			TYPE*,IAM(),'*** Cancellation Time Processing Cancelled'
			TYPE*,IAM(),'*** Processo de cancelamento cancelado'
			CALL CLOSASF
			CLOSE(LUN,STATUS='DELETE')
			GOTO 105
		ENDIF

	ELSE
!		TYPE*,IAM(),'*** Agents with cancellation time enabled not found'
		TYPE*,IAM(),'*** Nao foram encontrados prazos de cancelamento em aberto'
	ENDIF

	WRITE(LUN, FMT=903) COUNT ! NUMBER OF AGENTS FOUND WITH THE CANCELLATION TIME ENABLED
	WRITE(LUN, FMT=904) K,COUNT ! NUMBER OF CANCELLATIONS OK
	WRITE(LUN, FMT=909)
	CLOSE(LUN)
	
!	CREATE A ERROR FILE IF THERE ARE CANCELLATIONS NOT OK 
	IF(J.GT.0) THEN
		CALL FIND_AVAILABLE_LUN(LUN,ST) ! FINDS AN AVAILABLE LOGICAL UNIT
		IF (ST.NE.0) THEN
			TYPE*,IAM(),'ERROR GETTING LOGICAL UNIT TO OPEN FILE'
			CALL GSTOP(GEXIT_FATAL)
		ENDIF
		WRITE (ERREPNAME,910) DAYCDC ! BUILDS REPORT NAME
		OPEN(LUN,FILE=ERREPNAME,STATUS='NEW')
		
		WRITE(LUN,900) CTIM
		WRITE(LUN,911) CDAT(3),CDAT(2),CDAT(1),HEAD2,(VDAT(K),K=9,13),
     *           VDAT(VCDC),SYSID(SYS)
		
		WRITE(LUN, FMT=905)	J ! NUMBER OF CANCELLATIONS NOT OK
		WRITE(LUN,899)
    	
		CALL OPENASF(ASF)
		DO 104 I=1,J
			CALL READASF(ERRLIST(I),ASFREC,ST)
			IF(ST.NE.0) THEN
				CLOSE(LUN)
				TYPE*,'ASF.FIL READ ERROR ',ST,' TERMINAL ',ERRLIST(I)
				TYPE*,IAM(),'*** Relatorio de Erro incompleto: ',ERREPNAME
				CALL CLOSASF
				CALL GSTOP(GEXIT_FATAL)
			ENDIF
			WRITE(LUN,FMT=907) (ASFBYT(K),K=SAGNO,EAGNO),ERRLIST(I),(ASFBYT(K),K=SNAME,ENAME)
104 		CONTINUE
		WRITE(LUN,FMT=912)
		CLOSE(LUN)
		CALL CLOSASF
		TYPE*,IAM(),'*** Relatorio de Erro: ',ERREPNAME
	ENDIF

!	TYPE*,IAM(),'*** See Report File ',REPNAME
!	TYPE*,IAM(),'*** Exiting Cancellation Time Processing'
	TYPE*,IAM(),'*** Relatorio: ',REPNAME
105	TYPE*,IAM(),'*** A sair do processo de cancelamento'
	CALL GSTOP(GEXIT_SUCCESS)

C
C FORMAT STATEMENTS
C
!897	FORMAT('Name: ',<LNAME>A1,'Agtnum: ',<LAGNO>A1)
!898	FORMAT('Address1: ',<LSTRT>A1,'Terminal: ',I4.4)

899	FORMAT(132('-'),/,1X,'AGTNUM',4X,'TERM',3X,'PRAZO',4X,'NOME',/,132('-'))
!900	FORMAT(1X,A,T11,'.',I2.2,18X,
!900	FORMAT(A1,1X,A11,T14,'.',I2.2,15X,
900	FORMAT(31X,
     *  'S A N T A  C A S A  D A  M I S E R I C O R D I A  D E  L I S B O A',
     *	       16X,2A4,3X)
901	FORMAT(2X,I2.2,'.',I2.2,'.',I4.4,24X,A,22X,5A2,1X,'CDC ',I4,
     *	/,2X,'     ',117X,'SYS',A5)
902	FORMAT(/,132('-'))
903	FORMAT(/,1X,'FORAM ENCONTRADOS < ',I5.1,' > AGENTES COM O PRAZO DE CANCELAMENTO EM ABERTO')
904	FORMAT(/,132('-'),/,1X,'PRAZOS DE CANCELAMENTO FECHADOS APOS PROCESSAMENTO: < ',I4.1,' > EM < ',I4.1,' >',/,132('-'),/)
905	FORMAT(/,1X,'PRAZOS DE CANCELAMENTO NAO FECHADOS APOS PROCESSAMENTO: < ',I4.1,' >')
906	FORMAT(1X,<LAGNO>A1,2X,I5.5,3X,'ABERTO',3X,<LNAME>A1)
907	FORMAT(1X,<LAGNO>A1,2X,I5.5,3X,'ABERTO',3X,<LNAME>A1)
908	FORMAT('RELAT_PRAZO_CANCEL_',I4,'.REP')
909	FORMAT(//,24X,'F I M   D O   R E L A T O R I O')
910	FORMAT('RELAT_PRAZO_CANCEL_',I4,'.ERR')
911	FORMAT(2X,I2.2,'.',I2.2,'.',I4.4,16X,A,13X,5A2,1X,'CDC ',I4,
     *	/,2X,'     ',117X,'SYS',A5)
912	FORMAT(//,24X,'F I M   D O   R E L A T O R I O   D E    E R R O')

 	END
