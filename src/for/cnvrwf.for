C PROGRAM TO CONVERT WIN RESERVE FUND FILE                                    
C
C V03 23-MAR-2011 RXK Added DIR before and after conversion
C V02 25-FEB-2004 FRP EuroMillions: recalculate record according to MAXGAM=50.
C V01 02-MAR-2000 OXK release for Vakio changes
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
C       
        PROGRAM CNVRWF                                                    
        IMPLICIT NONE

        INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'                                           
        INCLUDE 'INCLIB:RECRDF.DEF'                                           
        INCLUDE 'INCLIB:RECRDF_OLD.DEF'

        INTEGER*4  OLD_MAXGAM
        PARAMETER (OLD_MAXGAM=10)

        INTEGER*4  FDB(7),OLD_FDB(7)
        INTEGER*4   MAXREC_CNV   ! # of records to convert
C
        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
C	
        INTEGER*4  ST,YESNO
C
        CALL COPYRITE
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR RWF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        NEW_FILE = 'FILE:RWF.NEW'
        OLD_FILE = 'FILE:RWF.FIL'
C
        MAXREC_CNV = 1
        FILE_SIZE  = MAXREC_CNV*RDFSEC/2
        TYPE*,IAM(),'Old file size = ', MAXREC_CNV*ORDFSEC/2
        TYPE*,IAM(),'New file size = ', MAXREC_CNV*RDFSEC/2
        TYPE*,IAM()
        TYPE*,IAM(),'Number of records to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL INPYESNO('Are you sure you want to convert RWF file',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL NEWFIL(1,NEW_FILE,FILE_SIZE,.FALSE.,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4OLD_FILE,4,0,0,ST)                                 
        CALL IOINIT(OLD_FDB,3,ORDFSEC*256)                        
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        CALL OPENW(4,I4NEW_FILE,4,0,0,ST)                                 
        CALL IOINIT(FDB,4,RDFSEC*256)                        
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C 
C Read INPUT file.
C                                                                
        CALL READW(OLD_FDB,1,ORDFREC,ST)  
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,1)
C
C Convert the file...
C 
	CALL FASTSET(0,RDFREC,SIZEOF(RDFREC)/4)
C
	CALL FASTMOV(ORDFPOL,RDFPOL,OLD_MAXGAM)
	CALL FASTMOV(ORDFADD,RDFADD,OLD_MAXGAM)
	CALL FASTMOV(ORDFUSE,RDFUSE,OLD_MAXGAM)
	CALL FASTMOV(ORDFUDW,RDFUDW,OLD_MAXGAM)
	CALL FASTMOV(ORDFADW,RDFADW,OLD_MAXGAM)
	CALL FASTMOV(ORDFADR,RDFADR,OLD_MAXGAM)
	CALL FASTMOV(ORDFUSD,RDFUSD,OLD_MAXGAM)
	CALL FASTMOV(ORDFDRW,RDFDRW,OLD_MAXGAM)
	CALL FASTMOV(ORDF_WRFTAB,RDF_WRFTAB,10*OLD_MAXGAM)
	CALL FASTMOV(ORDF_WRFCUD,RDF_WRFCUD,OLD_MAXGAM)

	CALL FASTMOV(ORDF_SPTPOLDIV,RDF_SPTPOLDIV,NUMSPT*SPGDIV)
C
C Write to output file...
C
	CALL WRITEW(FDB,1,RDFREC,ST)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,1)
C
        CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:RWF.FIL to FILE:RWF.OLD'
        CALL LIB$RENAME_FILE('FILE:RWF.FIL','FILE:RWF.OLD')

        TYPE*,IAM(),'Renaming FILE:RWF.NEW to FILE:RWF.FIL'
        CALL LIB$RENAME_FILE('FILE:RWF.NEW','FILE:RWF.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR RWF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'RWF file converted succesfully!'
        CALL GSTOP(GEXIT_SUCCESS)
C
        END                                                                   
