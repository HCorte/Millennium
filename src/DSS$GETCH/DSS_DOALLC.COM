$!
$! v01 16-MAY-95 SLK initial release
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
$ if f$search("gxsrc:allc.lis") .nes. "" then del/nolog gxsrc:allc.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:allc.lis *.C
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:allc.lis 'p1'_code
$ endif
$ if f$search("gxsrc:allc.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:allc.lis
$ if f$search("gxsrc:allc.com") .nes. "" then del/nolog gxsrc:allc.com;*
$ open/write/error=erropout outfil gxsrc:allc.com
$ write/err=errwrout outfil "$set noon"
$ write/err=errwrout outfil "$set ver"
$ cnt = 0
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if f$parse("''file'",,,"TYPE") .nes. ".C" then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  write/err=errwrout outfil "$LOTCC ''name'"
$  cnt = cnt+1
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:ALLC.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:ALLC.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:ALLC.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:ALLC.COM"
$ close infil
$ goto END
$ ENDIN:
$ write/err=errwrout outfil "$set nover"
$ close infil
$ close outfil
$ del/nolog gxsrc:allc.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No .C files found in ''p1' group"
$  del/nolog gxsrc:allc.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No .C files found in ''p1' group"
$ END:
$ exit
