#
proc decrypt {string key} {
    if [string match {} $key] { return {<ENCRYPTED TEXT>} }
    global zircon
    return [exec $zircon(lib)/crypt D "$string" "$key"]
}
#
proc encrypt {string key} {
    if [string match {} $key] { return $string }
    global zircon
    return "\001SED [exec $zircon(lib)/crypt E "$string" "$key"]\001"
}
