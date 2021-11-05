$!
$! v01 13-feb-95 wxm initial release
$!
$ if "''p1'" .eqs. "" then p1 = "ACTIVE"
$ if "''p1'" .eqs. "*"      then goto START
$ if "''p1'" .eqs. "ACTIVE" then goto START
$ if "''p1'" .eqs. "LIVE"   then goto START
$ if "''p1'" .eqs. "TOOL"   then goto START
$ if "''p1'" .eqs. "JUNK"   then goto START
$ if "''p1'" .eqs. "UPDATE" then goto START
$ write sys$output "Invalid group parameter."
$ goto END
$ START:
$ if f$search("gxsrc:alllnk.lis") .nes. "" then del/nolog gxsrc:alllnk.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:alllnk.lis *.LNK
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:alllnk.lis 'p1'_code
$ endif
$ if f$search("gxsrc:alllnk.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:alllnk.lis
$ if f$search("alllnk.com") .nes. "" then del/nolog gxsrc:alllnk.com;*
$ open/write/error=erropout outfil gxsrc:alllnk.com
$ write outfil "$set nover"
$ write outfil "$set noon"
$ write outfil "$link = ""link/nomap"""
$ cnt = 0
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if f$parse("''file'",,,"TYPE") .nes. ".LNK" then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  write/err=errwrout outfil "$@dss$gtech:dss_dolnk ''name'"
$  cnt = cnt+1
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:ALLLNK.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:ALLLNK.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:ALLLNK.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:ALLLNK.COM"
$ close infil
$ goto END
$ ENDIN:
$ write/err=errwrout outfil "$del/symbol link"
$ close infil
$ close outfil
$ del/nolog gxsrc:alllnk.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No .LNK files found in ''p1' group"
$  del/nolog gxsrc:alllnk.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No .LNK files found in ''p1' group"
$ END:
$ exit
