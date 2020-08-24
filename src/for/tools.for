C
C This program is useful in early stage in developing.
C Currently it will print out text and control revisions
C for all the games and # of sectors per record (for FIXFIL)
C
C V03 13-OCT-1999 RXK World Tour added.
C V02 10-MAY-1999 UXN Tripla changed to Päivän Trio. Super Tripla added.
C V01 13-MAR-1998 UXN Initial release.
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
	PROGRAM TOOLS
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*13 PASPAS
	CHARACTER*20 PASENT
	EQUIVALENCE (PASPAS,PASENT)
	INTEGER*4   EXT,OPT
C
	CALL PASSWORD(5,PASENT)                                                 
	IF(PASPAS.NE.'GTECH_FINLAND') THEN
          TYPE*,' ********** ACCESS DENIED SORRY **********'                   
          TYPE*,' YOU MUST HAVE CORRECT PASSWORD FOR ACCESS'                    
          CALL GSTOP(GEXIT_SUCCESS)        
	ENDIF                                                                   
C	
10	CONTINUE
	TYPE*,IAM()
	TYPE*,IAM(),'1 - Display # of sectors per record (for FIXFIL)'
	TYPE*,IAM(),'2 - Display revisions'
	TYPE*,IAM(),'E - Exit'
	TYPE*,IAM()
	CALL INPNUM('Enter selection ',OPT,1,2,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_SUCCESS)
C
	GOTO (100,200) OPT
	GOTO 10
C
C Display record length
C
100	CONTINUE
	CALL DISP_RECLEN
	GOTO 10
C
C Display revisions.
C
200	CALL DISP_REVISION
	GOTO 10
	END
C
C ******************************************************************
C ******************************************************************
C
C Subroutine to display number of sectors per record (for FIXFIL)
C
C ******************************************************************
C ******************************************************************
C
C
 	SUBROUTINE DISP_RECLEN
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DLTREC.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:DSCREC.DEF'
	INCLUDE 'INCLIB:DTSREC.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DBNREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DPAREC.DEF'
C
	CHARACTER*20	FMT
C
	FMT = '(1X,A,'' - '',I4)'
	CALL DISPLAY_INIT
C
	CALL DISPLAY('Lotto           ',DLTSEC/2,FMT)
	CALL DISPLAY('Sports          ',DSPSEC/2,FMT)
	CALL DISPLAY('Results         ',DTGSEC/2,FMT)
	CALL DISPLAY('Wintip          ',DWISEC/2,FMT)
	CALL DISPLAY('Super Double    ',DDBSEC/2,FMT)
	CALL DISPLAY('Todays Couple   ',DCPSEC/2,FMT)
	CALL DISPLAY('Super Score     ',DSSSEC/2,FMT)
	CALL DISPLAY('Todays Trio     ',DTRSEC/2,FMT)
	CALL DISPLAY('Langen          ',DTSSEC/2,FMT)
	CALL DISPLAY('Bingo           ',DBNSEC/2,FMT)
	CALL DISPLAY('Score           ',DSCSEC/2,FMT)
	CALL DISPLAY('Joker           ',DKKSEC/2,FMT)
	CALL DISPLAY('Super Triple    ',DSTSEC/2,FMT)
	CALL DISPLAY('Passive         ',DPASEC/2,FMT)

	CALL DISPLAY('DAF file        ',DAFSEC/2,FMT)
C
	CALL DISPLAY_UPDATE
	END
C
C ******************************************************************
C ******************************************************************
C
C Subroutine to display revision numbers for the games.
C
C ******************************************************************
C ******************************************************************
C
 	SUBROUTINE DISP_REVISION
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:SCRCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:DBLCOM.DEF'
	INCLUDE 'INCLIB:CPLCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:TRPCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
C
	CHARACTER*20	FMT
	INTEGER*4	GIND,GTYP,GNUM
	CHARACTER*16	C_NAME(MAXGAM)
	EQUIVALENCE	(GLNAMES,C_NAME)
C
	FMT = '(1X,A,'' - '',Z8.8)'
C
	CALL DISPLAY_INIT
C
	DO GNUM=1,MAXGAM    
	   GTYP = GNTTAB(GAMTYP,GNUM)
	   GIND = GNTTAB(GAMIDX,GNUM)
	   IF(GTYP.EQ.TLTO) THEN
	      CALL DISPLAY(C_NAME(GNUM),LTOREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TSPT) THEN
	      CALL DISPLAY(C_NAME(GNUM),SPTREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TTGL) THEN
	      CALL DISPLAY(C_NAME(GNUM),TGLREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TTSL) THEN
	      CALL DISPLAY(C_NAME(GNUM),TSLREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TBNG) THEN
	      CALL DISPLAY(C_NAME(GNUM),BNGREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TSCR) THEN
	      CALL DISPLAY(C_NAME(GNUM),SCRREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TSSC) THEN
	      CALL DISPLAY(C_NAME(GNUM),SSCREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TDBL) THEN
	      CALL DISPLAY(C_NAME(GNUM),DBLREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TCPL) THEN
	      CALL DISPLAY(C_NAME(GNUM),CPLREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TTRP) THEN
	      CALL DISPLAY(C_NAME(GNUM),TRPREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TSTR) THEN
	      CALL DISPLAY(C_NAME(GNUM),STRREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TKIK) THEN
	      CALL DISPLAY(C_NAME(GNUM),KIKREV(GIND),FMT)
	   ELSEIF(GTYP.EQ.TWIT) THEN
	      CALL DISPLAY(C_NAME(GNUM),WITREV(GIND),FMT)
	   ENDIF
	ENDDO
	CALL DISPLAY_UPDATE
	END
C
C ******************************************************************
C ******************************************************************
C
C
C Subroutine to display data on the screen...
C
C ******************************************************************
C ******************************************************************
C
C
	SUBROUTINE DISPLAY(STRING,VALUE,FMT)
	IMPLICIT NONE
C
	CHARACTER*(*) STRING
	INTEGER*4     VALUE
	CHARACTER*(*) FMT
C
	CHARACTER*80 LINE(1000)
	BYTE	     I1LINE(80*1000)
	EQUIVALENCE  (LINE,I1LINE)
	CHARACTER*80 STR
	INTEGER*4    LEN,I
	INTEGER*4    NR_ITEMS
C
	NR_ITEMS = NR_ITEMS + 1	
	WRITE(LINE(NR_ITEMS),FMT) STRING,VALUE
	RETURN
C
C ENTRY TO INITIALIZE VARIABLES.
C
	ENTRY DISPLAY_INIT
	CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(I1LINE),I1LINE)
	NR_ITEMS = 0
	RETURN
C
C ENTRY TO PRINT THE DATE TO THE SCREEN
C
	ENTRY DISPLAY_UPDATE
C
	DO I = 1, NR_ITEMS
	    WRITE(6,'(A)') LINE(I)
	    IF(MOD(I,21).EQ.0) THEN 
               CALL INPTEXT('Press ENTER to continue',STR,LEN)
	    ENDIF
	ENDDO
	RETURN
	END

