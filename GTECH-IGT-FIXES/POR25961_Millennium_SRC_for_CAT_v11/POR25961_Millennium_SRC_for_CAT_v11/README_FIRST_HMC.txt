* ============================================================================================================
* This item is the property of IGT Corporation, West Greenwich,
* Rhode Island, and contains confidential and trade secret information.
* It may not be transfered from the custody or control of IGT except
* as authorized in writing by an officer of IGT.  Neither this item
* nor the information it contains may be used, transfered, reproduced,
* published, or disclosed, in whole or in part, and directly or
* indirectly, except as expressly authorized by an officer of IGT,
* persuant to written agreement.
*
* Copyright (c) 2022 IGT Corporation.  All rights reserved.
* ============================================================================================================
*
* DATE            	BY          DESCRIPTION
* ------------------	-------	---------------------------------------------------------------------------------
* 04-APR-2022 	VCF        POR25961 Central Systems Ext IPS Release for CAT & CAT TIR's
*
*
* ============================================================================================================
* FILES DELIVERED WITH THIS RELEASE
* ============================================================================================================

File                     					Change Description
-------------------------------------------------                                   ----------------------------------------------------------------------------------------------------------
TRNSNP1.FOR					ALLOW 7 DIGITS FOR PACKS NUMBERS (EXPAND PRINTING MASK).
PTHPRO.TO_LNK					EURCOM SHAREABLE IMAGE WAS ADDED TO THE LINKER PROCEDURE.
PTHOUT.TO_LNK					EURCOM SHAREABLE IMAGE WAS ADDED TO THE LINKER PROCEDURE.

*
* ============================================================================================================
* INSTRUCTIONS FOR EXECUTABLES GENERATION
* ============================================================================================================
  1. RESERVE (GET) THE LISTED FILES FROM CMS INTO WORKING DIRECTORY. THE NEW ONES DON'T RESERVE IT
  2. COPY FROM THE FOLDER THE PREVIOUSLY LISTED FILES INTO WORKING DIRECTORY
  3. RENAME ALL THE *.TO_LNK FILES TO *.LNK IN THE WORKING DIRECTORY
  4. INSERT (PUT) THE PREVIOUSLY LISTED SOURCE CODE FILES INTO CMS
  5. LOTGEN IS NOT REQUIRED FOR THIS DELIVERY
  6. COMPILE *.FOR MODULES INCLUDED ABOVE
  7. EXECUTE VISION.LNK and GUIWORK.LNK
  8. EXECUTE *.LNK MODULES INCLUDED ABOVE 
* ============================================================================================================

1. GET TRNSNP1.FOR
1. GET PTHPRO.LNK
1. GET PTHOUT.LNK
2. FTP para esta diretoria dos ficheiros atualizados
3. RENAME ALL THE *.TO_LNK FILES TO *.LNK IN THE WORKING DIRECTORY
4. PUT TRNSNP1.FOR "ALLOW 7 DIGITS FOR PACKS NUMBERS (EXPAND PRINTING MASK)"
4. PUT PTHPRO.LNK "EURCOM SHAREABLE IMAGE WAS ADDED TO THE LINKER PROCEDURE"
4. PUT PTHOUT.LNK "EURCOM SHAREABLE IMAGE WAS ADDED TO THE LINKER PROCEDURE"
6. DFOR TRNSNP1
7. DLNK VISION
7. DLNK GUIWORK
8. DLNK PTHPRO
8. DLNK PTHOUT