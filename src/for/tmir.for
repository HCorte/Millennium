C
C PROGRAM TMIR
C
C V20 18-APR-2016 SCML M16 PROJECT
C V19 04-MAR-2015 SCML Bug fix 
C V18 05-MAR-2014 SCML Placard Project.
C V17 06-DEC-2013 SCML Added support for new validation types
C V16 06-JUN-2011 FJG  Exclude EM processing
C V15 08-JUN-2000 UXN  FTNAMES.DEF added.
C V14 16-Mar-2000 RXK  Totals for promoted free week tickets added.
C V13 14-May-1997 UXN  CRSVALUNIT replaced by 10.
C V12 17-May-1996 HXK  Update from Wojtek, Siew Mun
C V11 02-Sep-1994 HXK  Merge of May,June RFSS batch 
C V10 02-May-1994 HXK  SET SHARIND TO ZERO FOR OPNEWY
C V09 09-Mar-1994 JXP  Call openwy instead of open w for TMF's as only require
C                      read access. Initially required for acces to remote
C                      logging file in 'thruway'
C V08 18-Oct-1993 HXK  fix for REFunds.
C V07 02-Aug-1993 SXH  Added error exception handler NOCONSIG
C V06 12-Jul-1993 SXH  Released for Finland
C V05 21-JAN-1993 DAB  Initial Release Based on Netherlands Bible, 12/92,
C                      and Comm 1/93 update. DEC Baseline
C V04 10-APR-1992 GCAN ADDED REPORT FILE NAME OPTION AND # COPIES.
C V03 29-FEB-1992 GCAN ADDED OPTICAL DISK OPTION.
C V02 15-FEB-1991 WOL  USES NEW CHARACTER CMONY ROUTINES
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C V01 05-MAR-1990 TDM INITIAL RELEASE FOR DENMARK
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
	PROGRAM TMIR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:FTNAMES.DEF'
	INCLUDE 'INCLIB:ITNAMES.DEF'
	INCLUDE 'INCLIB:IGSTNAMES.DEF' ! V18
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:PRINTEURO.DEF'                                                !V20
C
	INTEGER*4   TPHDRLEN                !
	PARAMETER   (TPHDRLEN=80)	

	INTEGER*4   FDB(7)                  !
        INTEGER*4   FILNAME(7)              !
	INTEGER*4   OFF(5)                  !
	INTEGER*4   VALUE(5)                !
	INTEGER*4   LOGREC(LREC*3)          !
	INTEGER*4   TOTAL(TCRS+NUMCRS+1,2)  !
        INTEGER*4   PAYOPAMT(MAXTYP)        !TOTAL PAY AMOUNT FOR PAYMENT ORDER
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
        INTEGER*4   NETAMTPAY(MAXTYP)       !NET AMOUNT TO PAY
        INTEGER*4   PAYBNKAMT(MAXTYP)       !TOTAL PAY AMOUNT FOR BANK TRANSFER
        INTEGER*4   PAYCSHAMT(MAXTYP)       !TOTAL PAY AMOUNT FOR CASH
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
	INTEGER*4   STATUS                  !
	INTEGER*4   COPY                    !
	INTEGER*4   J                       !
	INTEGER*4   K                       !
	INTEGER*4   YY                      !
	INTEGER*4   XX                      !
	INTEGER*4   Y                       !
	INTEGER*4   X                       !
	INTEGER*4   SERIDX                  !
	INTEGER*4   REPIDX                  !
	INTEGER*4   XETIM                   !
	INTEGER*4   XSTIM                   !
	INTEGER*4   XESER                   !
	INTEGER*4   XSSER                   !
	INTEGER*4   FILIDX                  !
	INTEGER*4   DEVIDX                  !
	INTEGER*4   PAGE                    !
	INTEGER*4   NUM2                    !
	INTEGER*4   NUM1                    !
	INTEGER*4   I                       !
	INTEGER*4   FLAG                    !
	INTEGER*4   ETIM                    !
	INTEGER*4   STIM                    !
	INTEGER*4   SER                     !
	INTEGER*4   ESER                    !
	INTEGER*4   SSER                    !
	INTEGER*4   TER                     !
	INTEGER*4   EXT                     !
	INTEGER*4   NUM                     !
	INTEGER*4   ST                      !
	INTEGER*4   NOCHECK0                !
	INTEGER*4   PUNIT                   !
	INTEGER*4   TAPENUM                 !
	INTEGER*4   TAPBUF(LREC,LBLK+3)	    !Tape Buffer when Optical Read.
	INTEGER*4   HBUF(TPHDRLEN)	    !Tape Header Length after INIT.
	INTEGER*4   YNFLG		    !Yes No Flag.
	INTEGER*4   CDC			    !Cdc Date to read from Optical Disk.
	INTEGER*4   REPFILE(5)		    !Report File Name.
C

	CHARACTER    ANS                    !
	CHARACTER*38 STRING1(5)             !
        CHARACTER*38 STRING2(5)             !
	CHARACTER*4  DEVTYP(2)              !
	CHARACTER    TAPE*20                !
	CHARACTER    TAPNMS*20              !
	CHARACTER    CREPFILE*20            !
	CHARACTER*4  AGTOPT                 !
	CHARACTER*10 FILTYP(2)              !
	CHARACTER*8  REPTYP(2)              !
        CHARACTER*8  SERTYP(2)              !
                                            
	LOGICAL  EOT/.FALSE./               !
	LOGICAL  DISK/.TRUE./               !
	LOGICAL  CARY/.FALSE./              !
	LOGICAL  DRWF/.FALSE./              !
	LOGICAL  DETAIL/.FALSE./            !
	LOGICAL  SCRAM/.FALSE./             !
	LOGICAL  BEGIN/.TRUE./              !
	LOGICAL  OPTICAL/.FALSE./           !
	LOGICAL  EXEM/.FALSE./              ! V16

        INTEGER*4  NOCONSIG
        EXTERNAL   NOCONSIG
C
C EURO MIL PROJECT - INCLUDE SUM OF VALIDATION, WAGER AND CANCEL
C 
CV20        INTEGER*4 SUMEUROVALID, SUMEUROWAGER, SUMEUROCANCEL
        INTEGER*4 EM_SERIAL,EM_CHK
        LOGICAL EM_JUSTONE
        CHARACTER*12 C12EMXSER/'            '/                                  !V20
        CHARACTER*3  EXEMOPT/'NO '/                                             !V20
C
C V18 - Start
        INTEGER*4    IGSTOTAL(0:NUMIGSTTYP,2)  !
        LOGICAL      ODS_JUSTONE
        
        STRUCTURE    /STODSXREF/
          INTEGER*4    YEAR
          INTEGER*4    MONTH
          INTEGER*4    DAY
          INTEGER*4    GAME
          INTEGER*8    SERIAL
          INTEGER*4    LSERIAL
          INTEGER*4    HSERIAL
          INTEGER*4    CHKDIG
        END STRUCTURE
        
        RECORD /STODSXREF/ ODSXREF
        
        INTEGER*8 I8TMP
        INTEGER*4 I4TMP(2)
        EQUIVALENCE(I8TMP,I4TMP)
        
        CHARACTER*25 CBUF_XREF
        INTEGER*4    BUFLEN_XREF
        PARAMETER (BUFLEN_XREF = 25)
        
        CHARACTER*24 C24XREF/'                        '/
        INTEGER*4    LEN_XREF
        PARAMETER (LEN_XREF = 24)
C
        LOGICAL     EXIGS/.FALSE./                                              !V20
        CHARACTER*3 EXIGSOPT/'NO '/                                             !V20
C
        INTEGER*4   LINCNT
        
C V18 - End
C
	DATA DEVTYP/'DISK','TAPE'/
	DATA TAPENUM/0/
	DATA FILTYP/'CARRYOVER ','DAILY TMF '/
	DATA REPTYP/'SUMMARY ','DETAIL  '/
	DATA SERTYP/'INTERNAL','EXTERNAL'/

	DATA TAPE/'MAGX:'/
	DATA PUNIT/7/
	DATA STRING1/'Enter first  offset (A=all)           ',
     *	             'Enter second offset (A=all)           ',
     *	             'Enter third  offset (A=all)           ',
     *	             'Enter fourth offset (A=all)           ',
     *	             'Enter fifth  offset (A=all)           '/
	DATA STRING2/'Enter first  value  (A=all)           ',
     *	             'Enter second value  (A=all)           ',
     *	             'Enter third  value  (A=all)           ',
     *	             'Enter fourth value  (A=all)           ',
     *	             'Enter fifth  value  (A=all)           '/
	DATA VALUE/5*-1/
	DATA OFF/5*-1/

        INTEGER*4  PROMOTKT(4,2*MAXDRW,MAXGAM)   !numbers of promoted tickets
        INTEGER*4  PROMOBRD(4,2*MAXDRW,MAXGAM)   !numbers of promoted boards
        INTEGER*4  GOODPT, EXCHPT, FRACPT, VOIDPT
        PARAMETER  (GOODPT=1) 
        PARAMETER  (EXCHPT=2) 
        PARAMETER  (FRACPT=3) 
        PARAMETER  (VOIDPT=4) 
        INTEGER*4  NB, NKIK, L, GAM, FIRSTDRW(MAXGAM)/MAXGAM*0/

	INTEGER*4  TOTPAS(2,2),IND,TCKS
	INTEGER*4   PVAL,PRET
	PARAMETER   (PVAL=1)
	PARAMETER   (PRET=2)
C
	EQUIVALENCE (REPFILE,CREPFILE)
C
	COMMON /NOCHECK0/ NOCHECK0
C
C
C
C
	CALL COPYRITE

        CALL LIB$ESTABLISH(NOCONSIG)
C
	PAGE     =  0
	NOCHECK0 = -1
	LINCNT   =  LINSPP + 1 ! IT MUST BE GREATER THAN LINSPP
        CALL FASTSET(0,PROMOTKT,8*MAXDRW,MAXGAM)
        CALL FASTSET(0,PROMOBRD,8*MAXDRW,MAXGAM)
        CALL FASTSET(0, TOTAL, (TCRS + NUMCRS + 1) *2)
        CALL FASTSET(0, PAYOPAMT, MAXTYP)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
        CALL FASTSET(0, NETAMTPAY, MAXTYP)
        CALL FASTSET(0, PAYBNKAMT, MAXTYP)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
C
C V18 - Start
        CALL FASTSET(0, IGSTOTAL, (NUMIGSTTYP+1) *2)
C V18 - End
C
        CALL FASTSET(0, EURGWAG,    SIZEOF(EURGWAG)/4)                          !V20
        CALL FASTSET(0, EURGCAN,    SIZEOF(EURGCAN)/4)                          !V20
        CALL FASTSET(0, EURGVAL,    SIZEOF(EURGVAL)/4)                          !V20
        CALL FASTSET(0, EURTOT,     SIZEOF(EURTOT)/4)                           !V20
        CALL FASTSET(0, EURGBET,    SIZEOF(EURGBET)/4)                          !V20
        CALL FASTSET(0, EURTBET,    SIZEOF(EURTBET)/4)                          !V20
        CALL FASTSET(0, EUM1MULTOT, SIZEOF(EUM1MULTOT)/4)                       !V20
        CALL FASTSET(0, EUM1SIMTOT, SIZEOF(EUM1SIMTOT)/4)                       !V20
C
C GET FILE/TAPE
C
	TYPE *
	TYPE *,'<<<<< TMIR Transaction Master File Inquiry   V01 >>>>>'
	TYPE *
	CALL WIMG(5,'Enter file device (Disk, Tape)        ')
	READ(5,900) ANS
	IF(ANS.EQ.'E'.OR.ANS.EQ.'e') CALL GSTOP(GEXIT_OPABORT)
	IF(ANS.EQ.'T'.OR.ANS.EQ.'t') DISK=.FALSE.
C
C
5	CONTINUE
	IF(DISK) THEN
	    CALL WIMG(5,'Enter file type (Tmf, Carryover, Draw)')
	    READ(5,900) ANS
	    IF(ANS.EQ.'E'.OR.ANS.EQ.'e') CALL GSTOP(GEXIT_OPABORT)
	    IF(ANS.EQ.'C'.OR.ANS.EQ.'c') CARY=.TRUE.
	    IF(ANS.EQ.'D'.OR.ANS.EQ.'d') DRWF=.TRUE.
	    CALL WIMG(5,'Enter file name (VOLN:FILNAME)        ')
	    READ(5,901) FILNAME
	ELSE
	    OPTICAL = .FALSE.
	    CALL INPNUM('Enter tape drive number               ',
     *	                 NUM,1,9,EXT)
	    IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	    TAPENUM=NUM
	    TAPE(4:4)=CHAR(NUM+48)
	    CALL PRMYESNO('Is this a OPTICAL Disk ? ',YNFLG)
	    IF(YNFLG.EQ.1) THEN
	        OPTICAL = .TRUE.
	        CALL PRMNUM('Enter CDC date to report from: ',CDC,1,9999,EXT)
	        IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	    ENDIF
	ENDIF
C
C OPEN FILE/TAPE
C
	IF(DISK) THEN
	    IF(CARY) THEN					!CARRY FILE
	        CALL IOPEN(FILNAME,1,LREC*2,LCDC,LSER*2-1,ST)
	    ELSE IF(DRWF) THEN				!DRAW FILE
	        CALL OPENW(1,FILNAME,4,0,0,ST)
	        CALL IOINIT(FDB,1,128*256)
	    ELSE						!TM FILE
	        CALL OPENWY(1,FILNAME,0,4,0,ST)
	        CALL TOPEN(1)
	    ENDIF

	    IF(ST.NE.0) THEN
	        CALL FILERR(FILNAME,1,ST,0)
	        GOTO 5
	    ENDIF
	ELSE
	    CALL TAPOPEN(FDB,TAPE,ST)
	    CALL TAPINT(FDB,1,8192)
C
C IF OPTICAL DISK REWIND AND READ RECORD TO FIND DESIRED CDC DATE.
C
	    IF(OPTICAL) THEN
	        CALL XREWIND(FDB,ST)
	        CALL TAPINT(FDB,1,TPHDRLEN)    !Set to Tape Header Length.
	        CALL FEOT(FDB,ST)
	        CALL RTAPEW(FDB,HBUF,ST)	    !Read Standard Header.
	        IF(ST.NE.'88'X.AND.ST.NE.0) THEN
		    TYPE*,IAM(),'Optical Disk Header Read Error  St> ',ST
		    CALL GPAUSE
	        ENDIF
C
	        CALL FEOT(FDB,ST)
	        CALL RTAPEW(FDB,HBUF,ST)	    !Read Standard Header.
	        IF(ST.NE.'88'X.AND.ST.NE.0) THEN
		    TYPE*,IAM(),'Optical Disk Header Read Error  St> ',ST
		    CALL GPAUSE
	        ENDIF
C
	        CALL TAPINT(FDB,1,8192)	    !Re-Init to TM Size.
C
15	        CONTINUE
	        CALL RTAPEW(FDB,TAPBUF,ST)
	        IF(ST.EQ.500) THEN		    !End of Optical Disk.
		    TYPE*,IAM(),'End of Optical Disk Reached, ',
     *		        	'no record for CDC: ',CDC,' was found.'
		    CALL TAPCLOS(FDB,ST)
		    CALL GSTOP(GEXIT_SUCCESS)
	        ELSE IF(ST.NE.0) THEN
		    TYPE*,IAM(),'Optical Disk Read Error  St> ',ST
		    CALL GPAUSE
	        ENDIF
C
C CHECK IF RECORD IS FOR CORRECT CDC DATE.
C IF NOT CORRECT AND MORE RECORDS EXIST READ HEADER AGAIN.
C
	        IF(TAPBUF(1,2).NE.CDC) THEN    !This record is not the one.
		    CALL FEOT(FDB,ST)
		    IF(ST.EQ.500) THEN	    !End of Optical Disk.
		        TYPE*,IAM(),'End of Disk Reached, ',
     *			            'no record for CDC: ',CDC,' was found.'
		        CALL TAPCLOS(FDB,ST)
		        CALL GSTOP(GEXIT_SUCCESS)
		    ENDIF
		    GOTO 15
	        ENDIF
	    ENDIF
	ENDIF
C
C
12	CONTINUE

	CALL WIMG(5,'Enter report file name:               ')
	READ(5,904) REPFILE
	IF(CREPFILE(1:2).EQ.'E '.OR.CREPFILE(1:2).EQ.'e ') GOTO 2000
	CALL ROPEN(CREPFILE,PUNIT,ST)
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),CREPFILE,' Open error  st - ',ST
	    CALL USRCLOS1(PUNIT)
	    GOTO 12
	ENDIF
C
C GET OPTIONS
C
C
C	CALL PRMNUM('Enter number of report copies:        ',
C    *               COPY,0,20,EXT)
C	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
        COPY = 0
C        
	CALL INPNUM('Enter terminal number (A=all)         ',
     *	            TER,1,NUMAGT,EXT)
	IF(EXT.EQ.-4) TER=-1
	IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
C
	CALL WIMG(5,'Exclude EM transactions (Y/N)         ')
	CALL YESNO(FLAG)
	IF(FLAG.EQ.3) CALL GSTOP(GEXIT_OPABORT)
CV20	IF(FLAG.EQ.1) EXEM=.TRUE.	
        IF(FLAG.EQ.1) THEN                                                      !V20
          EXEMOPT='YES'                                                         !V20
          EXEM=.TRUE.                                                           !V20
        ENDIF                                                                   !V20
C
        CALL WIMG(5,'Exclude IGS transactions (Y/N)        ')                   !V20
        CALL YESNO(FLAG)                                                        !V20
        IF(FLAG.EQ.3) CALL GSTOP(GEXIT_OPABORT)                                 !V20
        IF(FLAG.EQ.1) THEN                                                      !V20
          EXIGSOPT='YES'                                                        !V20
          EXIGS=.TRUE.                                                          !V20
        ENDIF                                                                   !V20
C
C EURO MIL PROJECT - GET EXTERNAL SERIAL 
C
        EM_JUSTONE = .FALSE.
        ODS_JUSTONE = .FALSE.
	CALL INPNUM('Enter starting serial number (A=all/E=EuroMil/P=Placard)  ',
     *	            SSER,1,999999999,EXT)
	IF( EXT.LT.0.AND.EXT.NE.-4.AND.EXT.NE.-1.AND.EXT.NE.-2) CALL GSTOP(GEXIT_OPABORT)
	IF(EXT.EQ.-4) THEN
	    SSER = 1
	    ESER = 999999999
        ELSEIF (EXT .EQ. -1) THEN
           EM_JUSTONE = .TRUE.
	   CALL INPNUM('Enter external serial number                              ',
     *	                 EM_SERIAL,1,99999999,EXT)
           IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT) 
           CALL INPNUM('Enter external check digits                               ',
CV20     *	                 EM_CHK,1,256,EXT)
     *	                 EM_CHK,0,255,EXT)                                      !V20 (CHECK DIGITS ARE REPRESENTED IN 1 BYTE, SO IT RANGES BETWEEN 0 AND 255)
           IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT) 
           WRITE(C12EMXSER,'(I8.8,A1,I3.3)') EM_SERIAL, '-', EM_CHK             !V20
      	   SSER = 1
	   ESER = 999999999
	ELSEIF(EXT .EQ. -2) THEN
18	CONTINUE
        ODS_JUSTONE = .TRUE.
		CALL WIMG(5,'Enter external serial number                              ')
		
		CBUF_XREF(1:BUFLEN_XREF) = ' '
		C24XREF(1:LEN_XREF) = ' '
		READ(*,905,ERR=19) CBUF_XREF 
        IF(CBUF_XREF(1:1) .EQ. 'E' .OR. CBUF_XREF(1:1) .EQ. 'e') CALL GSTOP(GEXIT_OPABORT)
        
     	IF (LEN(TRIM(CBUF_XREF)) .NE. LEN_XREF) GOTO 19
     	
     	C24XREF(1:LEN_XREF) = CBUF_XREF(1:LEN_XREF)
     	DO I = 1,LEN_XREF
     	  IF (I .EQ. 7 .OR. I .EQ. 10 .OR. I .EQ. 21) THEN ! Positions of '-' character
     	    IF (C24XREF(I:I) .NE. '-') GOTO 19
     	  ELSE
     	    IF (C24XREF(I:I) .LT. '0' .OR. C24XREF(I:I) .GT. '9') GOTO 19
     	  ENDIF
     	ENDDO

C		READ(C24XREF(1:3),9051,ERR=19) ODSXREF.YEAR
C		READ(C24XREF(4:6),9051,ERR=19) ODSXREF.MONTH
C		READ(C24XREF(7:9),9051,ERR=19) ODSXREF.DAY
C		READ(C24XREF(11:13),9051,ERR=19) ODSXREF.GAME
C		READ(C24XREF(15:27),9052,ERR=19) ODSXREF.SERIAL
C		READ(C24XREF(29:33),9053,ERR=19) ODSXREF.CHKDIG

		READ(C24XREF(1:2),9051,ERR=19) ODSXREF.YEAR
		READ(C24XREF(3:4),9051,ERR=19) ODSXREF.MONTH
		READ(C24XREF(5:6),9051,ERR=19) ODSXREF.DAY
		READ(C24XREF(8:9),9051,ERR=19) ODSXREF.GAME
		READ(C24XREF(11:20),9052,ERR=19) ODSXREF.SERIAL
		READ(C24XREF(22:24),9053,ERR=19) ODSXREF.CHKDIG

	    SSER = 1
	    ESER = 999999999
	    
	    I8TMP=ODSXREF.SERIAL
	    ODSXREF.HSERIAL = I4TMP(2)
	    ODSXREF.LSERIAL = I4TMP(1)
	    
C	    TYPE*, ODSXREF.YEAR,ODSXREF.MONTH,ODSXREF.DAY,ODSXREF.GAME,ODSXREF.SERIAL,
C     *         ODSXREF.LSERIAL, ODSXREF.HSERIAL, ODSXREF.CHKDIG
	    
	    GOTO 20
19		TYPE*, IAM(), ' Ext Serial # must have the format YYMMDD-GG-SSSSSSSSSS-XXX'
	    
        GOTO 18   
	ELSE
	    CALL INPNUM('Enter Ending serial number (A=all)    ',
     *	                 ESER,SSER,999999999,EXT)
	    IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
	    IF(EXT.EQ.-4) ESER=999999999
	ENDIF
	SER = SSER
C
C
20	CONTINUE

	CALL INPTIM('Enter starting time (A=all)           ',
     *	             STIM,EXT)
	IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
	IF(EXT.EQ.-4) THEN
	    STIM = 0
	    ETIM = 99999999
	ELSE
	    CALL INPTIM('Enter ending time (A=all)             ',
     *	                 ETIM,EXT)
	    IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
	    IF(EXT.EQ.-4) ETIM=99999999
	    IF(STIM.GT.ETIM) THEN
	        TYPE*,'Sorry, starting time greater than ending time'
	        GOTO 20
	    ENDIF
	ENDIF
C
C
	CALL WIMG(5,'Enter report option (Detail,Summary)  ')
	READ(5,900) ANS
	IF(ANS.EQ.'E'.OR.ANS.EQ.'e') CALL GSTOP(GEXIT_OPABORT)
	IF(ANS.EQ.'D'.OR.ANS.EQ.'d') DETAIL=.TRUE.
C
C
	CALL WIMG(5,'Scramble serial numbers (Y/N)         ')
	CALL YESNO(FLAG)
	IF(FLAG.EQ.3) CALL GSTOP(GEXIT_OPABORT)
	IF(FLAG.EQ.1) SCRAM=.TRUE.
C
C
C
C EURO MIL PROJECT - include Euro Mil Options
C
CV20        TYPE*,'***   OFFSETS   *********  VALUES  **********************************' 
C	TYPE*,'Transaction(9)-> Wag(1)/Can(2)/ICan(3)/Val(4)/Claim(5)/SPE(7)' 
CV20        TYPE*,'Transaction(9)-> Wag(1)/Can(2)/ICan(3)/Val(4)/Claim(5)/SPE(7)/Eur(11)/IGS(12)' 
CV20        TYPE*,'Transaction(9) -> Wag(1)/Can(2)/ICan(3)/Val(4)/Return(5)/SPE(7)/Eur(11)/IGS(12)' 
CV20        TYPE*,'Game num(10)   -> (Game_Number)'
C	TYPE*,'Game Type(11) -> Loto(1)/Sports(2)/Joker(4)/Results(15)/Passive(16)'
CV20        TYPE*,'Game Type(11)  -> Loto(1)/Sports(2)/Joker(4)/Results(15)/Pas(16)/Eur(17)/Odds(18)'
CV20	TYPE*,'Tra Status(1)  -> Good(1)/Void(2)/CashExchg(4)/Cash(5)/Reject(6)'
CV20        TYPE*,'*******************************************************************'
        TYPE*,'***   OFFSETS   *****   VALUES   **************************************' !V20
        TYPE*,'* Trx Status(1)...: Good(1)/Void(2)/CashExchg(4)/Cash(5)/Reject(6)    *' !V20
        TYPE*,'* Trx Error(2)....: Noer(0)/Invl(1)/Synt(2)/Supr(3)/NotOn(4)/Sdor(5)  *' !V20
        TYPE*,'*                   Sdrw(6)/Rety(15)/Vinq(16)/Grev(18)/Bcrs(33)       *' !V20
        TYPE*,'* Trx Type(9).....: Wag(1)/Can(2)/Ican(3)/Val(4)/Ret(5)/Spe(7)/Crs(9) *' !V20
        TYPE*,'*                   Eur(11)/Igs(12)                                   *' !V20
        TYPE*,'* Game Num(10)....: TotobolaNormal(1)/Totoloto(2)/TotobolaExtra2(3)   *' !V20
        TYPE*,'*                   Loto2(4)/Joker(5)/TotolotoSab(6)/TotolotoQua(7)   *' !V20
        TYPE*,'*                   Classica(8)/Popular(9)/TotobolaExtra1(10)         *' !V20
        TYPE*,'* Game Type(11)...: Loto(1)/Sports(2)/Joker(4)/Results(15)/Pas(16)    *' !V20
        TYPE*,'*                   Eur(17)/Odds(18)/Raffle(19)                       *' !V20
        TYPE*,'* Spe Func(25)....: Son(1)/Soff(2)/SalRep(5)/GamRep(6)/Reprint(7)     *' !V20
        TYPE*,'*                   CtrlReq(19)/BilRep(51)                            *' !V20
        TYPE*,'* Eur TrxType(25).: Wag(1)/Can(2)/Val(4)                              *' !V20
        TYPE*,'* Eur WagChan(45).: Retlr(1)/Web(2)/SMS(3)/PortalMed(4)/Mob(5)        *' !V20
        TYPE*,'* Igs TrxType(25).: Wag(0)/Can(1)/Val(2)/Pay(3)/Rep(4)                *' !V20
        TYPE*,'***********************************************************************' !V20

	DO I = 1, 5
	    CALL INPNUM(STRING1(I),NUM1,1,TRALEN,EXT)
	    IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
	    IF(EXT.EQ.-4) GOTO 40

CV20	    CALL INPNUM(STRING2(I),NUM2,0,99999999,EXT)
	    CALL INPNUM(STRING2(I),NUM2,0,999999999,EXT)                              !V20
	    IF(EXT.LT.0.AND.EXT.NE.-4) CALL GSTOP(GEXIT_OPABORT)
	    IF(EXT.EQ.-4) GOTO 40

	    OFF(I)=NUM1
	    VALUE(I)=NUM2
        END DO
C
C READ TRANSACTION
C
40	CONTINUE
	IF(BEGIN) THEN
	    CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,
     *	                PUNIT,PAGE,DAYCDC)
	    DEVIDX=1
	    IF(.NOT.DISK) DEVIDX=2     !DEVICE TYPE
C
	    FILIDX=1
	    IF(.NOT.CARY) FILIDX=2     !FILE TYPE
C
	    TAPNMS='    '
	    IF(TAPENUM.NE.0) TAPNMS=TAPE
	    WRITE (AGTOPT,902) TER
	    IF(TER.EQ.-1) WRITE (AGTOPT,903)
	    XSSER = SSER
	    XESER = ESER
	    IF(SSER.EQ.1.AND.ESER.EQ.1) THEN
	        XSSER = -999999999
	        XESER = -999999999
	    ENDIF

	    XSTIM = STIM
	    XETIM = ETIM
	    IF(STIM.EQ.0) XSTIM=0
	    IF(ETIM.EQ.99999999) XETIM=0
	    REPIDX = 1
	    IF(DETAIL) REPIDX=2        !REPORT TYPE
C
	    SERIDX=1
	    IF(SCRAM) SERIDX=2         !SERIAL NUMBER TYPE
C
	    WRITE(PUNIT,10000) DEVTYP(DEVIDX),FILTYP(FILIDX),
CV20     *	      (FILNAME(X),X=1,5),TAPNMS,(AGTOPT(Y:Y),Y=1,4),XSSER,XESER,
     *	      (FILNAME(X),X=1,5),TAPNMS,(AGTOPT(Y:Y),Y=1,4),                    !V20
     *         EXEMOPT, EXIGSOPT,                                               !V20
     *         C12EMXSER, C24XREF,                                              !V20
     *         XSSER,XESER,                                                     !V20
     *	      DISTIM(XSTIM),DISTIM(XETIM),REPTYP(REPIDX),SERTYP(SERIDX),
     *	      (OFF(XX),XX=1,5),(VALUE(YY),YY=1,5)
	    BEGIN=.FALSE.
	ENDIF
C
	IF(.NOT.DISK) THEN
	    CALL READTAPE(LOGREC,FDB,EOT)
	    IF(EOT) GOTO 1000
	    GOTO 100
	ENDIF
C
C
	IF(.NOT.CARY .AND. .NOT.DRWF) THEN
	    CALL READTMF(LOGREC,SER,EOT)
	    IF(EOT) GOTO 1000
	    GOTO 100
	ENDIF
C
	IF(DRWF) THEN
	    CALL READDRW(LOGREC,FDB,EOT)
	    IF(EOT) GOTO 1000
	    GOTO 100
	ENDIF
C
	CALL READTCF(LOGREC,1,EOT)
	IF(EOT) GOTO 1000
C
C CHECK IF TRANSACTION SHOULD BE PRINTED
C
100	CONTINUE
	CALL LOGTRA(TRABUF,LOGREC)
	IF(TRABUF(TTER).NE.TER.AND.TER.GT.0) GOTO 40
	IF(TRABUF(TTIM).LT.STIM)             GOTO 40
	IF(TRABUF(TTIM).GT.ETIM)             GOTO 1000
	IF(TRABUF(TSER).LT.SSER)             GOTO 40
	IF(TRABUF(TSER).GT.ESER.AND.CARY)    GOTO 40
	IF(TRABUF(TSER).GT.ESER)             GOTO 1000

	DO I = 1, 5
	    IF(OFF(I).LT.0)                      GOTO 200
	    IF(TRABUF(OFF(I)).NE.VALUE(I))       GOTO 40
        END DO
C
C PRINT TRANSACTION
C
200	CONTINUE
C EURO MIL PROJECT - PRINT EURO MIL TRANSACTION
C
C       CALL PRINTRA(TRABUF,PUNIT,DETAIL,SCRAM,TOTAL,CARY)
C        INTEGER*4 EM_SERIAL,EM_CHK
C        IF ((TRABUF(TTYP) .NE. TEUR) .AND. (.NOT. EM_JUSTONE)) THEN ! V18 - COMMENT
        IF ((TRABUF(TTYP) .NE. TEUR) .AND. (.NOT. EM_JUSTONE) .AND. 
     *     (TRABUF(TTYP) .NE. TIGS) .AND. (.NOT. ODS_JUSTONE)) THEN ! V18 - ADD
           CALL IGS_PRINTRA(TRABUF,PUNIT,DETAIL,LINCNT,SCRAM,TOTAL,CARY)
CV20        ELSE IF(TRABUF(TTYP) .EQ. TEUR) THEN
        ELSE IF(TRABUF(TTYP) .EQ. TEUR .AND. (.NOT. ODS_JUSTONE)) THEN
C20          IF(EXEM) THEN
          IF(EXEM) GOTO 40                                                      !V20 READ NEXT TRANSACTION
          IF(.NOT. EM_JUSTONE) THEN                                             !V20
C----+------------------------------------------------------------------
C V19| Bug fix
C----+------------------------------------------------------------------
C           CALL PRINTRA(TRABUF,PUNIT,DETAIL,SCRAM,TOTAL,CARY)
C20            CALL IGS_PRINTRA(TRABUF,PUNIT,DETAIL,LINCNT,SCRAM,TOTAL,CARY)
C----+------------------------------------------------------------------
C V19| Bug fix
C----+------------------------------------------------------------------
            CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)         !V20
            GOTO 40                                                             !V20
          ELSE
C----+---+-------------+------------------------------------------------
C V20|BEG| M16 PROJECT | EM_JUSTONE LOGIC IMPROVEMENT
C----+---+-------------+------------------------------------------------
CV20            IF (EM_JUSTONE) THEN
CV20              IF ((EM_SERIAL .NE. TRABUF(TEUSER)) .AND. (EM_CHK .NE. TRABUF(TEUCHK))) GOTO 40
            IF(TRABUF(TEUTYP).EQ.TWAG) THEN
              IF(EM_SERIAL.EQ.TRABUF(TEUSER) .AND. 
     *           EM_CHK   .EQ.TRABUF(TEUCHK)) THEN                            !EM WAGER
                CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                GOTO 40
              ENDIF
              IF(TRABUF(TEUW_SMWFL).NE.0) THEN
                IF((EM_SERIAL.EQ.TRABUF(TEUW_SMWSN) .AND. 
     *              EM_CHK   .EQ.TRABUF(TEUW_SMWCD))) THEN                    !SM WAGER
                  CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                ENDIF
                GOTO 40
              ENDIF
C
            ELSEIF(TRABUF(TEUTYP).EQ.TVAL) THEN                               !EM/SM VALIDATION
              IF(EM_SERIAL.EQ.TRABUF(TEUVWSER) .AND. 
     *           EM_CHK   .EQ.TRABUF(TEUVWCKD)) THEN                          !EM/SM WAGER VALIDATED
                CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                GOTO 40
              ENDIF
C
              IF(EM_SERIAL.EQ.TRABUF(TEUSER) .AND. 
     *           EM_CHK   .EQ.TRABUF(TEUCHK)) THEN
                CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                GOTO 40
              ENDIF
C
            ELSEIF(TRABUF(TEUTYP).EQ.TCAN) THEN                               !EM CANCELLATION
              IF(EM_SERIAL.EQ.TRABUF(TEUCWSER) .AND. 
     *           EM_CHK   .EQ.TRABUF(TEUCWCKD)) THEN                          !EM WAGER CANCELLED
                CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                GOTO 40
              ENDIF
C
              IF(EM_SERIAL.EQ.TRABUF(TEUSER) .AND. 
     *           EM_CHK   .EQ.TRABUF(TEUCHK)) THEN
                CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                GOTO 40
              ENDIF
C
              IF(TRABUF(TEUC_SMCFL).NE.0) THEN
                IF(EM_SERIAL.EQ.TRABUF(TEUC_SMWSN) .AND.
     *             EM_CHK   .EQ.TRABUF(TEUC_SMWCD)) THEN                      !SM WAGER CANCELLED
                  CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                  GOTO 40
                ENDIF
                IF(EM_SERIAL.EQ.TRABUF(TEUC_SMCSN) .AND. 
     *             EM_CHK   .EQ.TRABUF(TEUC_SMCCD)) THEN                      !SM CANCEL
                  CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
                  GOTO 40
                ENDIF
              ENDIF
            ENDIF
            GOTO 40
CV20           CALL PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM,
CV20     *                     SUMEUROVALID,SUMEUROWAGER,SUMEUROCANCEL)
C----+---+-------------+------------------------------------------------
C V20|END| M16 PROJECT | EM_JUSTONE LOGIC IMPROVEMENT
C----+---+-------------+------------------------------------------------
          ENDIF
C V18 - START
CV20	      ELSE IF(TRABUF(TTYP) .EQ. TIGS) THEN
	      ELSE IF(TRABUF(TTYP) .EQ. TIGS .AND. .NOT.EM_JUSTONE) THEN
C----+---+-------------+------------------------------------------------
C V20|BEG| M16 PROJECT | ODS_JUSTONE LOGIC IMPROVEMENT
C----+---+-------------+------------------------------------------------
C20	        IF (ODS_JUSTONE) THEN
C20	          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) THEN ! PLACARD
C20	            IF (TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
C20	              IF (ODSXREF.YEAR    .NE. TRABUF(TIGSW_WRDY) .OR.
C20     *                ODSXREF.MONTH   .NE. TRABUF(TIGSW_WRDM) .OR.                              
C20     *                ODSXREF.DAY     .NE. TRABUF(TIGSW_WRDD) .OR. 
C20     *                ODSXREF.GAME    .NE. TRABUF(TIGSW_WRGM) .OR. 
C20     *                ODSXREF.LSERIAL .NE. TRABUF(TIGSW_WRSL) .OR.
C20     *                ODSXREF.HSERIAL .NE. TRABUF(TIGSW_WRSH) .OR.     
C20     *                ODSXREF.CHKDIG  .NE. TRABUF(TIGSW_WRCD)) GOTO 40
C20                
C20                ELSEIF (TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
C20                  IF (ODSXREF.YEAR    .NE. TRABUF(TIGSC_CRDY) .OR.
C20     *                ODSXREF.MONTH   .NE. TRABUF(TIGSC_CRDM) .OR.                              
C20     *                ODSXREF.DAY     .NE. TRABUF(TIGSC_CRDD) .OR. 
C20     *                ODSXREF.GAME    .NE. TRABUF(TIGSC_CRGM) .OR. 
C20     *                ODSXREF.LSERIAL .NE. TRABUF(TIGSC_CRSL) .OR.
C20     *                ODSXREF.HSERIAL .NE. TRABUF(TIGSC_CRSH) .OR.     
C20     *                ODSXREF.CHKDIG  .NE. TRABUF(TIGSC_CRCD)) GOTO 40
C20        
C20                ELSEIF (TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
C20                  IF (ODSXREF.YEAR    .NE. TRABUF(TIGSP_PRDY) .OR.
C20     *                ODSXREF.MONTH   .NE. TRABUF(TIGSP_PRDM) .OR.                              
C20     *                ODSXREF.DAY     .NE. TRABUF(TIGSP_PRDD) .OR. 
C20     *                ODSXREF.GAME    .NE. TRABUF(TIGSP_PRGM) .OR. 
C20     *                ODSXREF.LSERIAL .NE. TRABUF(TIGSP_PRSL) .OR.
C20     *                ODSXREF.HSERIAL .NE. TRABUF(TIGSP_PRSH) .OR.     
C20     *                ODSXREF.CHKDIG  .NE. TRABUF(TIGSP_PRCD)) GOTO 40
C20                ELSE 
C20                   GOTO 40
C20                ENDIF
C20	          ENDIF
C20	        ENDIF
          IF(EXIGS) GOTO 40                                                     !V20 READ NEXT TRANSACTION
          IF(ODS_JUSTONE) THEN
            IF(TRABUF(TGAMTYP).EQ.TODS.AND.TRABUF(TGAMIND).EQ.1) THEN           !PLACARD GAME
              IF(TRABUF(TIGS_TTYP).EQ.IGSWAG) THEN                              !PLACARD WAGER
                IF(ODSXREF.YEAR   .EQ.TRABUF(TIGSW_WRDY) .AND.
     *             ODSXREF.MONTH  .EQ.TRABUF(TIGSW_WRDM) .AND.
     *             ODSXREF.DAY    .EQ.TRABUF(TIGSW_WRDD) .AND.
     *             ODSXREF.GAME   .EQ.TRABUF(TIGSW_WRGM) .AND.
     *             ODSXREF.LSERIAL.EQ.TRABUF(TIGSW_WRSL) .AND.
     *             ODSXREF.HSERIAL.EQ.TRABUF(TIGSW_WRSH) .AND.
     *             ODSXREF.CHKDIG .EQ.TRABUF(TIGSW_WRCD)
     *            ) THEN
                  CALL PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
                ENDIF
              ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSVAL) THEN                          !PLACARD VALIDATION (INQUIRY)
                IF(ODSXREF.YEAR   .EQ.TRABUF(TIGSV_WRDY) .AND.
     *             ODSXREF.MONTH  .EQ.TRABUF(TIGSV_WRDM) .AND.
     *             ODSXREF.DAY    .EQ.TRABUF(TIGSV_WRDD) .AND.
     *             ODSXREF.GAME   .EQ.TRABUF(TIGSV_WRGM) .AND.
     *             ODSXREF.LSERIAL.EQ.TRABUF(TIGSV_WRSL) .AND.
     *             ODSXREF.HSERIAL.EQ.TRABUF(TIGSV_WRSH) .AND.
     *             ODSXREF.CHKDIG .EQ.TRABUF(TIGSV_WRCD)
     *            ) THEN
                  CALL PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
                ENDIF
              ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSPAY) THEN                          !PLACARD PAYMENT
                IF(   (ODSXREF.YEAR   .EQ.TRABUF(TIGSP_WRDY) .AND.
     *                 ODSXREF.MONTH  .EQ.TRABUF(TIGSP_WRDM) .AND.
     *                 ODSXREF.DAY    .EQ.TRABUF(TIGSP_WRDD) .AND.
     *                 ODSXREF.GAME   .EQ.TRABUF(TIGSP_WRGM) .AND.
     *                 ODSXREF.LSERIAL.EQ.TRABUF(TIGSP_WRSL) .AND.
     *                 ODSXREF.HSERIAL.EQ.TRABUF(TIGSP_WRSH) .AND.
     *                 ODSXREF.CHKDIG .EQ.TRABUF(TIGSP_WRCD))
     *            .OR.(ODSXREF.YEAR   .EQ.TRABUF(TIGSP_PRDY) .AND.
     *                 ODSXREF.MONTH  .EQ.TRABUF(TIGSP_PRDM) .AND.
     *                 ODSXREF.DAY    .EQ.TRABUF(TIGSP_PRDD) .AND.
     *                 ODSXREF.GAME   .EQ.TRABUF(TIGSP_PRGM) .AND.
     *                 ODSXREF.LSERIAL.EQ.TRABUF(TIGSP_PRSL) .AND.
     *                 ODSXREF.HSERIAL.EQ.TRABUF(TIGSP_PRSH) .AND.
     *                 ODSXREF.CHKDIG .EQ.TRABUF(TIGSP_PRCD))
     *            ) THEN
                  CALL PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
                ENDIF
              ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSCAN) THEN                          !PLACARD CANCEL
                IF(   (ODSXREF.YEAR   .EQ.TRABUF(TIGSC_WRDY) .AND.
     *                 ODSXREF.MONTH  .EQ.TRABUF(TIGSC_WRDM) .AND.
     *                 ODSXREF.DAY    .EQ.TRABUF(TIGSC_WRDD) .AND.
     *                 ODSXREF.GAME   .EQ.TRABUF(TIGSC_WRGM) .AND.
     *                 ODSXREF.LSERIAL.EQ.TRABUF(TIGSC_WRSL) .AND.
     *                 ODSXREF.HSERIAL.EQ.TRABUF(TIGSC_WRSH) .AND.
     *                 ODSXREF.CHKDIG .EQ.TRABUF(TIGSC_WRCD))
     *            .OR.(ODSXREF.YEAR   .EQ.TRABUF(TIGSC_CRDY) .AND.
     *                 ODSXREF.MONTH  .EQ.TRABUF(TIGSC_CRDM) .AND.
     *                 ODSXREF.DAY    .EQ.TRABUF(TIGSC_CRDD) .AND.
     *                 ODSXREF.GAME   .EQ.TRABUF(TIGSC_CRGM) .AND.
     *                 ODSXREF.LSERIAL.EQ.TRABUF(TIGSC_CRSL) .AND.
     *                 ODSXREF.HSERIAL.EQ.TRABUF(TIGSC_CRSH) .AND.
     *                 ODSXREF.CHKDIG .EQ.TRABUF(TIGSC_CRCD))
     *            ) THEN
                  CALL PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
                ENDIF
              ENDIF
            ENDIF
            GOTO 40
          ENDIF
C----+---+-------------+------------------------------------------------
C V20|END| M16 PROJECT | ODS_JUSTONE LOGIC IMPROVEMENT
C----+---+-------------+------------------------------------------------          
          CALL PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
          GOTO 40                                                               !V20 READ NEXT TRANSACTION
C V18 - END	       WRITE(PUNIT,9031) IGSTNAMES(J),IGSTOTAL(J+1,1), '           ')
        ENDIF
C
        IF(EM_JUSTONE .OR. ODS_JUSTONE) GOTO 40                                 !V20 READ NEXT TRANSACTION
C
C SET TOTAL PAY AMOUNT FOR PAYMENT ORDES
C
        IF(TRABUF(TTYP) .EQ. TVAL .AND. TRABUF(TSTAT) .NE. REJT) THEN
          IF(TRABUF(TGAMTYP) .NE. TPAS) THEN
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
            ! Transaction is not a bank transfer
            IF(TRABUF(TVTYPE).NE.VNBNK) THEN
            
              ! There are no paying orders for low prizes
              IF( (TRABUF(TVOPPAY) + TRABUF(TVKOPPAY)) .LT. P(VALORDER)) THEN
                
                ! New validations have net amount here
                
                ! Main game
                IF(TRABUF(TVOPPAY) .NE. 0) THEN
                  NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVOPPAY)
                  PAYCSHAMT(TVAL) = PAYCSHAMT(TVAL) + TRABUF(TVOPPAY) 
                ELSE
                  NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVPAY)
                  PAYCSHAMT(TVAL) = PAYCSHAMT(TVAL) + TRABUF(TVPAY) 
                ENDIF
                
                ! Kicker game
                IF(TRABUF(TVKOPPAY) .NE. 0) THEN
                  NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVKOPPAY) 
                  PAYCSHAMT(TVAL) = PAYCSHAMT(TVAL) + TRABUF(TVKOPPAY) 
                ELSE
                  NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVKPAY)
                  PAYCSHAMT(TVAL) = PAYCSHAMT(TVAL) + TRABUF(TVKPAY) 
                ENDIF
              
              ! Now we handle existing paying orders for mid-tier prizes
              ELSE
                PAYOPAMT(TVAL) = PAYOPAMT(TVAL) + TRABUF(TVOPPAY) + TRABUF(TVKOPPAY)
                NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVOPPAY) + TRABUF(TVKOPPAY)
              ENDIF
              
            ! Transaction is a bank transfer
            ELSE
              ! Main game
              IF(TRABUF(TVOPPAY) .NE. 0) THEN
                PAYBNKAMT(TVAL) = PAYBNKAMT(TVAL) + TRABUF(TVOPPAY)
                NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVOPPAY)  
              ELSE
                PAYBNKAMT(TVAL) = PAYBNKAMT(TVAL) + TRABUF(TVPAY)
                NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVPAY)  
              ENDIF
              
              ! Kicker game
              IF(TRABUF(TVKOPPAY) .NE. 0) THEN
                PAYBNKAMT(TVAL) = PAYBNKAMT(TVAL) + TRABUF(TVKOPPAY) 
                NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVKOPPAY)  
              ELSE
                PAYBNKAMT(TVAL) = PAYBNKAMT(TVAL) + TRABUF(TVKPAY)
                NETAMTPAY(TVAL) = NETAMTPAY(TVAL) + TRABUF(TVKPAY)  
              ENDIF
            ENDIF
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
          ENDIF
        ENDIF
C
C PASSIVE TOTALS
C
        IF((TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TRET) .AND.
     *      TRABUF(TGAMTYP).EQ.TPAS .AND. TRABUF(TERR).EQ.NOER) THEN

            DO TCKS = 1, TRABUF(TPTCK)
	       IND = 0
               IF (TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.RETURND .OR. 
     *             TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.RETAFDR ) IND = PRET
         
               IF (TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER ) IND = PVAL
 
	       IF (IND.GT.0) THEN
                   TOTPAS(IND,1) = TOTPAS(IND,1) + 1
                   TOTPAS(IND,2) = TOTPAS(IND,2) + TRABUF(TPPAY1+OFFTRA*(TCKS-1))  
	       ENDIF
            ENDDO

	ENDIF

C
C SUM PROMOTED TICKETS AND BOARDS
C
        IF(TRABUF(TWADDFW).GT.0) THEN
C
           GAM = TRABUF(TGAM)
           IF(GAM.LT.1.OR.GAM.GT.MAXGAM) GOTO 40
           IF(FIRSTDRW(GAM).EQ.0) FIRSTDRW(GAM) = TRABUF(TWEND) 
           L = MAXDRW + FIRSTDRW(GAM) - TRABUF(TWEND)
           IF(L.GT.0.AND.L.LE.2*MAXDRW) THEN
C
              IF(TRABUF(TGAMTYP).EQ.TLTO) NB = TRABUF(TWSIMP)
              IF(TRABUF(TGAMTYP).EQ.TKIK) 
     *           NB = IDNINT(DFLOAT(TRABUF(TWAMT)) / (KIKPRC(TRABUF(TGAMIND))*P(PRFACTOR)))
              IF(TRABUF(TGAMTYP).NE.TKIK .AND. TRABUF(TWKGME).GT.0)
     *           NKIK = IDNINT(DFLOAT(TRABUF(TWKAMT)) / (KIKPRC(TRABUF(TGAMIND))*P(PRFACTOR)))
C
              IF(TRABUF(TSTAT).EQ.GOOD.AND.TRABUF(TWFFLG).EQ.0) THEN      
                 PROMOTKT(GOODPT,L,GAM) = PROMOTKT(GOODPT,L,GAM) + 1
                 PROMOBRD(GOODPT,L,GAM) = PROMOBRD(GOODPT,L,GAM) + NB
                 IF(TRABUF(TGAMTYP).NE.TKIK.AND.TRABUF(TWKGME).GT.0) THEN
                    PROMOBRD(GOODPT,L,TRABUF(TWKGME)) =
     *              PROMOBRD(GOODPT,L,TRABUF(TWKGME)) + NKIK
                 ENDIF
              ENDIF
C
              IF(TRABUF(TSTAT).EQ.EXCH.AND.TRABUF(TWFFLG).EQ.0) THEN      
                 PROMOTKT(EXCHPT,L,GAM) = PROMOTKT(EXCHPT,L,GAM) + 1
                 PROMOBRD(EXCHPT,L,GAM) = PROMOBRD(EXCHPT,L,GAM) + NB
                 IF(TRABUF(TGAMTYP).NE.TKIK.AND.TRABUF(TWKGME).GT.0) THEN
                    PROMOBRD(EXCHPT,L,TRABUF(TWKGME)) =
     *              PROMOBRD(EXCHPT,L,TRABUF(TWKGME)) + NKIK
                 ENDIF
              ENDIF
C 
              IF(TRABUF(TSTAT).EQ.FRAC) THEN      
                 PROMOTKT(FRACPT,L,GAM) = PROMOTKT(FRACPT,L,GAM) + 1
                 PROMOBRD(FRACPT,L,GAM) = PROMOBRD(FRACPT,L,GAM) + NB
                 IF(TRABUF(TGAMTYP).NE.TKIK.AND.TRABUF(TWKGME).GT.0) THEN
                    PROMOBRD(FRACPT,L,TRABUF(TWKGME)) =
     *              PROMOBRD(FRACPT,L,TRABUF(TWKGME)) + NKIK
                 ENDIF
              ENDIF
C
              IF(TRABUF(TSTAT).EQ.VOID) THEN     
                 PROMOTKT(VOIDPT,L,GAM) = PROMOTKT(VOIDPT,L,GAM) + 1
                 PROMOBRD(VOIDPT,L,GAM) = PROMOBRD(VOIDPT,L,GAM) + NB
                 IF(TRABUF(TGAMTYP).NE.TKIK.AND.TRABUF(TWKGME).GT.0) THEN
                    PROMOBRD(VOIDPT,L,TRABUF(TWKGME)) =
     *              PROMOBRD(VOIDPT,L,TRABUF(TWKGME)) + NKIK
                 ENDIF
              ENDIF
C
           ENDIF
        ENDIF
	GOTO 40
C
1000	CONTINUE
C
	WRITE(PUNIT,9000)
	WRITE(PUNIT,9011)
	WRITE(PUNIT,9012)
	DO K = 1, TVAL
C            NETAMTPAY = TOTAL(K,2) - PAYOPAMT(K)
	    WRITE(PUNIT,9001) FTNAMES(K), TOTAL(K,1),
     *                        CMONY(TOTAL(K,2),11,BETUNIT),
     *                        CMONY(PAYOPAMT(K),11,BETUNIT),
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
     *                        CMONY(NETAMTPAY(K),11,BETUNIT),
     *                        CMONY(PAYBNKAMT(K),11,BETUNIT),
     *                        CMONY(PAYCSHAMT(K),11,BETUNIT)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
        END DO
C
	WRITE(PUNIT, 9003) 
     *    TOTAL(TCRS, 1), 
     *    CMONY(TOTAL(TCRS, 2), 11, BETUNIT),
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
     *    CMONY(0, 11, BETUNIT),                ! WE DON'T HAVE OPS WITH INSTANT
     *    CMONY(TOTAL(TCRS, 2), 11, BETUNIT),
     *    CMONY(0, 11, BETUNIT),                
     *    CMONY(0, 11, BETUNIT)                 
C     *    CMONY(PAYOPAMT(TCRS),11,BETUNIT),
C     *    CMONY(NETAMTPAY(TCRS),11,BETUNIT),
C     *    CMONY(PAYBNKAMT(TCRS),11,BETUNIT),
C     *    CMONY(PAYCSHAMT(TCRS),11,BETUNIT)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
C
        WRITE(PUNIT,9005)
        DO 5100 J=1,NUMCRS
            IF(ITNAMES(J-1).EQ.'????????') GOTO 5100
            WRITE(PUNIT,9002) GTNAMES(TINS),ITNAMES(J-1),
     *                        TOTAL(TCRS+J,1)
5100    CONTINUE
        WRITE(PUNIT,9004) TOTAL(TCRS+NUMCRS+1,1)
C
C PASSIVE TOTALS
C
	WRITE(PUNIT,9008)
	WRITE(PUNIT,9009) TOTPAS(PRET,1),CMONY(TOTPAS(PRET,2),13,BETUNIT)
	WRITE(PUNIT,9010) TOTPAS(PVAL,1),CMONY(TOTPAS(PVAL,2),13,BETUNIT)

        DO J=1,MAXDRW
           DO K=1,MAXGAM
              IF(PROMOTKT(GOODPT,J,K).GT.0 .OR.
     *           PROMOTKT(EXCHPT,J,K).GT.0 .OR.
     *           PROMOTKT(FRACPT,J,K).GT.0 .OR.
     *           PROMOTKT(VOIDPT,J,K).GT.0) THEN
                 WRITE(PUNIT,9006) K, MAXDRW + FIRSTDRW(K) - J + 1,
     *                             PROMOTKT(GOODPT,J,K),
     *                             PROMOTKT(EXCHPT,J,K),
     *                             PROMOTKT(FRACPT,J,K),
     *                             PROMOTKT(VOIDPT,J,K)
                 WRITE(PUNIT,9007) PROMOBRD(GOODPT,J,K),
     *                             PROMOBRD(EXCHPT,J,K),
     *                             PROMOBRD(FRACPT,J,K),
     *                             PROMOBRD(VOIDPT,J,K)
      
              ENDIF
           ENDDO
        ENDDO
C
C EURO MIL PROJECT - INCLUDE SUM OF TRANSACTIONS
C
CV20        WRITE (PUNIT,9020)
CV20        WRITE (PUNIT,9021) SUMEUROWAGER
CV20        WRITE (PUNIT,9022) SUMEUROCANCEL
CV20        WRITE (PUNIT,9023) SUMEUROVALID
        CALL PRTEURSTAT(PUNIT)                                                  !V20
C
C V18 - INCLUDE IGS TRANSACTIONS STATISTICS
C
        WRITE (PUNIT,9030)
        DO 6100 J=0,NUMIGSTTYP
            IF(IGSTNAMES(J).EQ.'????????') GOTO 6100
            IF(J .EQ. IGSWAG .OR. J .EQ. IGSCAN) THEN 
              WRITE(PUNIT,9031) IGSTNAMES(J),IGSTOTAL(J,1), CMONY(IGSTOTAL(J,2),11,BETUNIT)
            ELSEIF(J .EQ. IGSVAL .OR. J .EQ. IGSPAY) THEN
              WRITE(PUNIT,9031) IGSTNAMES(J),IGSTOTAL(J,1), CMONY(IGSTOTAL(J,2),11,VALUNIT)
            ELSEIF(J .EQ. IGSREP) THEN
              WRITE(PUNIT,9031) IGSTNAMES(J),IGSTOTAL(J,1), '           '
            ENDIF
6100    CONTINUE
        WRITE(PUNIT,9032) IGSTOTAL(NUMIGSTTYP,1)
C
C V18 - End
C
	IF(.NOT.DISK) THEN
	    CALL XREWIND(FDB,ST)
	    CALL TAPCLOS(FDB,ST)
	    GOTO 2000
	ENDIF
C
	IF(.NOT.CARY) THEN
	    CALL CLOSEFIL(FDB)
	    GOTO 2000
	ENDIF
C
2000	CONTINUE

	CALL USRCLOS1(PUNIT)
	CALL SPOOL(CREPFILE,COPY,STATUS)
C
900	FORMAT(A1)
901	FORMAT(7A4)
902	FORMAT(I4)
903	FORMAT('ALL ')
904	FORMAT(5A4)
!!!!!!!!!!!!!!!!! RETIRAR COMENTARIO - APENAS PARA TESTES
905	FORMAT(A) !V18 ODS BET REFERENCE SERIAL NUMBER
C9051	FORMAT(I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3) !V18 ODS BET REFERENCE SERIAL NUMBER
C9051	FORMAT(I3.3,I3.3,I3.3,1X,I3.3,1X,I13.13,1X,I5.5) !V18 ODS BET REFERENCE SERIAL NUMBER
9051	FORMAT(I3.3) !V18 ODS BET REFERENCE SERIAL NUMBER
9052	FORMAT(I13.13) !V18 ODS BET REFERENCE SERIAL NUMBER
9053	FORMAT(I5.5) !V18 ODS BET REFERENCE SERIAL NUMBER
9054	FORMAT(I) !V18 ODS BET REFERENCE SERIAL NUMBER
!!!!!!!!!!!!!!!!!
9000	FORMAT(//)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9001    FORMAT(
     *            ' Total '
     *          , A8
     *          , '    '
     *          , I10
     *          , 9X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *  )
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9002    FORMAT(1X,2A8,1X,I8)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9003    FORMAT(
     *            ' Total Instant Cashes '
     *          , I7
     *          , 9X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *          , 5X
     *          , A11
     *  )
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9004    FORMAT(' Total Instant Transactions ',I10)
9005    FORMAT(/,' Total Instant Message Transactions ',/)
9006    FORMAT(/1X,'For game number ',I2,', draw ',I5,' found ',I8,' GOOD, ',
     *         I8,' EXCH, ',I8,' FRAC and ',I8,' VOID promoted tickets')
9007    FORMAT(1X,'altogether',T39,I8,' GOOD, ',
     *         I8,' EXCH, ',I8,' FRAC and ',I8,' VOID boards')
9008	FORMAT(/,' Total Passive Returns and Validations ',/)
9009	FORMAT(1X,'Returns     ',I8,3X,A13) 
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9010	FORMAT(1X,'Validations ',I8,3X,A13) 
9011    FORMAT(
     *            '                     '
     *          , '   COUNT'
     *          , '    '
     *          , '       TOTAL AMT'
     *          , '     '
     *          , 'TOT OPS AMT'
     *          , '     '
     *          , 'TOT NET AMT'
     *          , '     '
     *          , 'TOT BNK AMT'
     *          , '     '
     *          , 'TOT CSH AMT'
     *  )
9012    FORMAT(19X, 94('-'))
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
C
C EURO MIL PROJECT 
C
9020    FORMAT(/,' Euro Milhoes transactions',/)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
9021    FORMAT('  Wagers       ',I10)
9022    FORMAT('  Cancel       ',I10)
9023    FORMAT('  Validations  ',I10)
C----+------------------------------------------------------------------
C V17| Added support for new validation types
C----+------------------------------------------------------------------
C
C V18 - IGS MIL PROJECT 
C
9030    FORMAT(/,' IGS Message Transactions',/)
9031    FORMAT(1X,A8,3X,I8,5X,A11)
9032    FORMAT(' Total IGS Transactions ',I10)
C
10000	FORMAT(///,' R E P O R T    B A S E D   O N   F O L L O W I',
     *	      ' N G   P A R A M E T E R S',////,
     *	      5X,'DEVICE --------------> ',A4,/,
     *	      5X,'FILE TYPE -----------> ',A10,/,
     *	      5X,'FILE NAME -----------> ',5A4,/,
     *	      5X,'TAPE DRIVE ----------> ',A,/,
     *	      5X,'TERMINAL NUMBER -----> ',4A1,/,
     *        5X,'EXCLUDE EM TRX ------> ',A,/,                                 !V20
     *        5X,'EXCLUDE IGS TRX -----> ',A,/,                                 !V20
     *        5X,'EUROMIL EXT SER # ---> ',A,/,                                 !V20
     *        5X,'PLACARD EXT SER # ---> ',A,/,                                 !V20
     *	      5X,'STARTING SERIAL -----> ',I9,/,
     *	      5X,'ENDING SERIAL -------> ',I9,/,
     *	      5X,'STARTING TIME -------> ',A8,/,
     *	      5X,'ENDING TIME ---------> ',A8,/,
     *	      5X,'REPORT OPTION -------> ',A8,/,
     *	      5X,'SERIAL NUMBERS ------> ',A8,/,
CV20     *	      5X,'OFFSETS -------------> ',5(I4,' :'),/,
     *	      5X,'OFFSETS -------------> ',5(I9,' : '),/,
CV20     *	      5X,'VALUES --------------> ',5(I4,' :'))
     *	      5X,'VALUES --------------> ',5(I9,' : '),/)                        !V20

C
	END
