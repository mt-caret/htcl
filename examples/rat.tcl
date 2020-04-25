proc rat {n d} {
  if {!$d} {error "denominator can't be 0"}
  if {$d<0} {set n [- $n]; set d [- $d]}
  set g [gcd $n $d]
  set n [/ $n $g]
  set d [/ $d $g]
  expr {$d==1? $n: "$n/$d" }
}

proc ratsplit args {
   foreach {r _n _d} $args {
      upvar 1 $_n n  $_d d
      foreach {n d} [split $r /] break
      if {$d eq ""} {set d 1}
   }
}

#-- Four-species math on "rats":
proc rat+ {r s} {
   ratsplit $r a b $s c d
   rat [+ [* $a $d] [* $c $b]] [* $b $d]
}
proc rat- {r s} {
   ratsplit $r a b $s c d
   rat [- [* $a $d] [* $c $b]] [* $b $d]
}
proc rat* {r s} {
   ratsplit $r a b $s c d
   rat [* $a $c] [* $b $d]
}
proc rat/ {r s} {
   ratsplit $r a b $s c d
   rat [* $a $d] [* $b $c]
}

proc func {name argl body} {proc $name $argl [list expr $body]}

#-- Greatest common denominator:
func gcd {u v} {$u? [gcd [% $v $u] $u]: abs($v)}

#-- Binary expr operators exported:
foreach op {+ * / %} {func $op {a b} \$a$op\$b}

#-- "-" can have 1 or 2 operands:
func - {a {b ""}} {$b eq ""? -$a: $a-$b}

#-- a little tester reports the unexpected:
proc ? {cmd expected} {
   catch {uplevel 1 $cmd} res
   if {$res ne $expected} {puts "$cmd -> $res, expected $expected"}
}

#-- The test suite should silently pass when this file is sourced:
? {rat 42 6} 7
? {rat 1 -2} -1/2
? {rat -1 -2} 1/2
? {rat 1 0} "denominator can't be 0"
? {rat+ 1/3 1/3} 2/3
? {rat+ 1/2 1/2} 1
? {rat+ 1/2 1/3} 5/6
? {rat+ 1 1/2}    3/2
? {rat- 1/2 1/8} 3/8
? {rat- 1/2 1/-8} 5/8
? {rat- 1/7 1/7} 0
? {rat* 1/2 1/2} 1/4
? {rat/ 1/4 1/4} 1
? {rat/ 4 -6} -2/3
