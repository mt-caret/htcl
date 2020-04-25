# DustMotePlus - with a subset of CGI support
set root      c:/html
set default   index.htm
set port      80
set encoding  iso8859-1
proc bgerror msg {puts stdout "bgerror: $msg\n$::errorInfo"}
proc answer {sock host2 port2} {
    fileevent $sock readable [list serve $sock]
}
proc serve sock {
    fconfigure $sock -blocking 0
    gets $sock line
    if {[fblocked $sock]} {
        return
    }
    fileevent $sock readable ""
    set tail /
    regexp {(/[^ ?]*)(\?[^ ]*)?} $line -> tail args
    if {[string match */ $tail]} {
        append tail $::default
    }
    set name [string map {%20 " " .. NOTALLOWED} $::root$tail]
    if {[file readable $name]} {
        puts $sock "HTTP/1.0 200 OK"
        if {[file extension $name] eq ".tcl"} {
            set ::env(QUERY_STRING) [string range $args 1 end]
            set name [list |tclsh $name]
        } else {
            puts $sock "Content-Type: text/html;charset=$::encoding\n"
        }
        set inchan [open $name]
        fconfigure $inchan -translation binary
        fconfigure $sock   -translation binary
        fcopy $inchan $sock -command [list done $inchan $sock]
    } else {
        puts $sock "HTTP/1.0 404 Not found\n"
        close $sock
    }
}
proc done {file sock bytes {msg {}}} {
    close $file
    close $sock
}
socket -server answer $port
puts "Server ready..."
vwait forever

