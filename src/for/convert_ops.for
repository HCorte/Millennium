C CONVERT_OPS.FOR
C
C V01 13-DEC-2010 FJG LOTTO2 BATCH: INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO CONVERT OLD OPS.FIL TO NEW ONE
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
        options /check=nooverflow
	program convert_ops
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
	include 'inclib:ops_rec.def'
!===============================================================================
CDEC$ OPTIONS /WARNING=NOALIGNMENT
	STRUCTURE /OLD_OPS_STRU/
           
	   CHARACTER*2  GAME             ! Game number (Joker does not appear here) (FILLED WITH ZERO LEFT)
	   CHARACTER*6  YEARWEEK         ! Year plus Week (YYYYWW)
	   CHARACTER*6  ORDER            ! Sequential 6 digit order number (FILLED WITH ZEROS LEFT)        

	   CHARACTER*14 BILHETE          ! Serial number for Offline (Joker number) or Online (external serial)

	   LOGICAL      SPLITTED         ! Contains TRUE if the Order is ONE OF THE TWO orders generated from a
				         ! SINGLE OFFLINE Order (case of LOTO+LOTO2 Order)

	   CHARACTER*7  AGENT            ! Agent to receive order document (who sold the winning ticket) (ZEROS LEFT)

	   LOGICAL      PRINTED_BY_OFF   ! Indicates if it is an order already printed before our startup (by Offline)
	   LOGICAL      CLAIM            ! Indicates if the order is a CLAIM
	                                 
	   INTEGER*4    PROC_PAID_CDC    ! Day when this OP was registered as paid (from ODJ file)
           LOGICAL      GENERATED        ! True if it was already selected to print

	   INTEGER*4    PAYABLE_CDC      ! Limit for paying the order (prescription will be done some days later)
	   INTEGER*4    PAID_CDC         ! CDC when order was paid by the bank
	   LOGICAL      PAID_SENT_SAP    ! If paid info was sent to SAP 

           LOGICAL      ONLINE_ORDER     ! Indicates a if ONLINE order 
	   LOGICAL      HI_PRIZE         ! Indicates if a high Order prize (Payable only in specific Branches) 

	   INTEGER*4    WINS(5)          ! Wins on div (Up to 6 divisions)
	   INTEGER*4    JOKER_DIV        ! Win Division for Joker
	   INTEGER*4    TOTAL_GAME       ! Value in prize just for the main game 
           INTEGER*4    TOTAL_JOKER      ! Value in prize just for joker

	   CHARACTER*4  BANK             ! Bank for dsicounting the order and SCML prize account (ZEROS LEFT)
	   CHARACTER*4  BRANCH           ! Branch for discounting the order (high Orders) and for SCML prize account

C	   LOGICAL      PURGED           ! Is set to .true. when record is purged

	   LOGICAL      PAID_MANUALLY    ! Is set to .true. when OP is paid manually by PAGAMENTO_MANUAL

	END STRUCTURE

CDEC$ END OPTIONS

	RECORD /OLD_OPS_STRU/ OLD_OPS_REC
!===============================================================================	
	integer*4 flag
        integer*4 nlun
        integer*4 recs
        integer*4 ierr
        integer*4 nerr
        integer*8 ntot
        integer*8 itot
        character*12 nfil/'FILE:OPS.NEW'/
        character*12 ofil/'FILE:OPS.OLD'/        
        character*12 ffil/'FILE:OPS.FIL'/        
!
        parameter (nlun = OPS_LUN + 1)
!===============================================================================	
        call copyrite
        type*,iam()
        type*,iam(),'<<<<< CONVERT_OPS - Conversion of OPS file V2 >>>>>'
        type*,iam()
        type*,iam(),'This program will convert the OPS.FIL to the new format'
        type*,iam()        
        type*,iam(),'>>>>>>>>>> This process CAN only be run ONCE <<<<<<<<<<'
        type*,iam()      
        call prmyesno('Do you really want to proceed (Y/N) ',flag)          
        if(flag.ne.1) call gstop(GEXIT_OPABORT)        
!+++++++Create new file+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        type*,iam()
        type*,iam(),'Creating new file OPS.NEW'
	open (UNIT           = nlun,
     *        FILE           = nfil,                  ! FILE:OPS.NEW
     *        STATUS         = 'NEW',
     *        ORGANIZATION   = 'INDEXED',
     *        ACCESS         = 'KEYED',
     *        FORM           = 'UNFORMATTED',
     *        RECL           = SIZEOF(OPS_REC),
     *        KEY            = (1:15:CHARACTER:ASCENDING, 16:29:CHARACTER:ASCENDING),
     *        RECORDTYPE     = 'FIXED',
     *        IOSTAT         = nerr,
     *        INITIALSIZE    = 10000)
!+++++++Open existing file++++++++++++++++++++++++++++++++++++++++++++++++++++++
        type*,iam(),'Opening  OLD file OPS.FIL'
	open (UNIT           = OPS_LUN,
     *        FILE           = ffil,                  ! FILE:OPS.FIL
     *        STATUS         = 'OLD',
     *        ORGANIZATION   = 'INDEXED',
     *        ACCESS         = 'SEQUENTIAL',
     *        FORM           = 'UNFORMATTED',
     *        RECL           = SIZEOF(OLD_OPS_REC),
     *        KEY            = (1:14:CHARACTER:ASCENDING, 15:28:CHARACTER:ASCENDING),
     *        RECORDTYPE     = 'FIXED',
     *        IOSTAT         = ierr)     
!+++++++Main Loop+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        recs = 0
        itot = 0
        ntot = 0
        type*,iam()
        type*,iam(),'Converting OPS records...'
        type*,iam()        
100     continue
        read (OPS_LUN, END=300, IOSTAT=ierr) OLD_OPS_REC
        if(ierr.ne.0) THEN
          type*,iam(),'Error reading OPS.FIL ',ierr,' record: ',recs+1
          call gstop (GEXIT_FATAL)
        endif     
        recs = recs + 1
        if(mod(recs,5000).eq.0) type*,iam(),recs,' OPS converted...'
!
        OPS_REC.GAME           = OLD_OPS_REC.GAME             
        OPS_REC.YEARWEEK       = OLD_OPS_REC.YEARWEEK(1:4)//'0'//OLD_OPS_REC.YEARWEEK(5:6)
        OPS_REC.CWEEK          = OLD_OPS_REC.YEARWEEK(5:6)
        OPS_REC.ORDER          = OLD_OPS_REC.ORDER            
        OPS_REC.BILHETE        = OLD_OPS_REC.BILHETE          
        OPS_REC.SPLITTED       = OLD_OPS_REC.SPLITTED         
        OPS_REC.AGENT          = OLD_OPS_REC.AGENT            
        OPS_REC.PRINTED_BY_OFF = OLD_OPS_REC.PRINTED_BY_OFF   
        OPS_REC.CLAIM          = OLD_OPS_REC.CLAIM            
        OPS_REC.PROC_PAID_CDC  = OLD_OPS_REC.PROC_PAID_CDC    
        OPS_REC.GENERATED      = OLD_OPS_REC.GENERATED        
        OPS_REC.PAYABLE_CDC    = OLD_OPS_REC.PAYABLE_CDC      
        OPS_REC.PAID_CDC       = OLD_OPS_REC.PAID_CDC         
        OPS_REC.PAID_SENT_SAP  = OLD_OPS_REC.PAID_SENT_SAP    
        OPS_REC.ONLINE_ORDER   = OLD_OPS_REC.ONLINE_ORDER     
        OPS_REC.HI_PRIZE       = OLD_OPS_REC.HI_PRIZE         
        OPS_REC.WINS(1)        = OLD_OPS_REC.WINS(1)          
        OPS_REC.WINS(2)        = OLD_OPS_REC.WINS(2)          
        OPS_REC.WINS(3)        = OLD_OPS_REC.WINS(3)          	   
        OPS_REC.WINS(4)        = OLD_OPS_REC.WINS(4)          
        OPS_REC.WINS(5)        = OLD_OPS_REC.WINS(5)          	   
        OPS_REC.WINS(6)        = 0
        OPS_REC.JOKER_DIV      = OLD_OPS_REC.JOKER_DIV        
        OPS_REC.TOTAL_GAME     = OLD_OPS_REC.TOTAL_GAME       
        OPS_REC.TOTAL_JOKER    = OLD_OPS_REC.TOTAL_JOKER      
        OPS_REC.BANK           = OLD_OPS_REC.BANK             
        OPS_REC.BRANCH         = OLD_OPS_REC.BRANCH           
        OPS_REC.PAID_MANUALLY  = OLD_OPS_REC.PAID_MANUALLY   
        ntot = ntot + OPS_REC.TOTAL_GAME + OPS_REC.TOTAL_JOKER
        itot = itot + OLD_OPS_REC.TOTAL_GAME + OLD_OPS_REC.TOTAL_JOKER        
!
        write(UNIT=nlun, IOSTAT=nerr) OPS_REC
        IF (nerr.NE.0) THEN
          type*,iam(),'Error writting OPS.NEW ',nerr,' record: ',recs
          call gstop (GEXIT_FATAL)
        ENDIF
!
        goto 100
!+++++++End of Loop+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
300     continue
        type*,iam(),recs,' OPS converted...'
        type*,iam()
        type*,iam(),'Total amount (in cents) for OPS.FIL: ',itot
        type*,iam(),'Total amount (in cents) for OPS.NEW: ',ntot
        type*,iam()
!                
        close(OPS_LUN)     
        close(nlun)        
!
        type*,iam(),'Renaming ',ffil,' to ',ofil
        call lib$rename_file(ffil,ofil)
        type*,iam(),'Renaming ',nfil,' to ',ffil
        call lib$rename_file(nfil,ffil)
!        
        type*,iam()  
        call gstop(GEXIT_SUCCESS)
	end
