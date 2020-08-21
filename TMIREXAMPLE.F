!this is a example of the full tmir program to generate the reports but with alot of prints for debug and understanding purposes
      PROGRAM TMIREXAMPLE
      IMPLICIT NONE

      CALL COPYRITE

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

        INTEGER*4   NETAMTPAY(MAXTYP)       !NET AMOUNT TO PAY
        INTEGER*4   PAYBNKAMT(MAXTYP)       !TOTAL PAY AMOUNT FOR BANK TRANSFER
        INTEGER*4   PAYCSHAMT(MAXTYP)       !TOTAL PAY AMOUNT FOR CASH

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
CV20 
         INTEGER*4 SUMEUROVALID, SUMEUROWAGER, SUMEUROCANCEL
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


      END

