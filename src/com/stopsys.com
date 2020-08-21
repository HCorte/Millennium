$!
$! $Log:   GXAFXT:[GOLS]STOPSYS.COV  $
$! 
$!    Rev 1.0   17 Apr 1996 12:06:38   HXK
$! Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
$! 
$!    Rev 1.2   18 Aug 1993 16:07:12   SXH
$! Comment out DISABLExxxx CALLs
$! 
$!    Rev 1.1   12 Jul 1993 15:54:18   SXH
$! Released for Finland
$! 
$!    Rev 1.0   25 Feb 1993 11:03:06   EBD
$! Initial revision.
$!
$!
$! V03 12-AUG-11 RXK "Millennium" replaced with "ES Evolution"
$! V02 24-JUN-1991 JSL Added @GXCOM:DISABLEVISION.COM
$!                           @GXCOM:DISABLEHASF.COM  
$!                           @GXCOM:DISABLEDRAW.COM
$!                           This will disable the LOTTERY accounts...
$!                  
$! V01 25-APR-91 TKO  Initial release
$!
$! This command file will cause STOPSYS to be run for Maryland.  It is
$! analogous to RUNSYS in that it will increase the user's working set
$! quota before running STOPSYS.
$!
$! 
$! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$!  This item is the property of GTECH Corporation, Providence, Rhode
$!  Island, and contains confidential and trade secret information. It
$!  may not be transferred from the custody or control of GTECH except
$!  as authorized in writing by an officer of GTECH. Neither this item
$!  nor the information it contains may be used, transferred,
$!  reproduced, published, or disclosed, in whole or in part, and
$!  directly or indirectly, except as expressly authorized by an
$!  officer of GTECH, pursuant to written agreement.
$! 
$!  Copyright 1991 GTECH Corporation. All rights reserved.
$! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
$! 
$!
$!
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT "Portugal ES Evolution System Shutdown"
$ WRITE SYS$OUTPUT " "
$ WRITE SYS$OUTPUT " "
$!
$!
$ SET MESSAGE /NOFACIL/NOIDENT/NOSEVERITY/NOTEXT
$ DEFINE SYS$INPUT SYS$COMMAND
$ SET MESSAGE /FACIL/IDENT/SEVERITY/TEXT
$!
$!
$! Get current working set parameters
$!
$ OLDWSQUOTA	= F$GETJPI("","WSQUOTA")	!CURRENT QUOTA
$ WSAUTH	= F$GETJPI("","WSAUTH")		!AUTHORIZED QUOTA
$!
$ ON ERROR     THEN GOTO ALLDONE
$ ON CONTROL_Y THEN GOTO ALLDONE
$!
$ SET WORKING_SET/QUOTA='WSAUTH			!SET TO MAXIMUM
$!
$! DISABLE THE LOTTERY ACCOUNTS...
$! 
$! @GXCOM:DISABLEVISION.COM
$! @GXCOM:DISABLEHASF.COM
$! @GXCOM:DISABLEDRAW.COM
$!
$ SUBRUN STSYSTEM
$!
$!
$ALLDONE:
$ SET WORKING_SET/QUOTA='OLDWSQUOTA		!RESET BACK
$!
