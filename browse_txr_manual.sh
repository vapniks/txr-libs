#!/bin/sh

# Browse txr manpage using fzf
# Keybindings are shown in the header line.
# Options are inherited from $FZF_DEFAULT_OPTS, but --header is overwritten unless you have saved it in $FZF_HEADER
# (in which case it's prepended with info about new keybindings).


TXRMANPAGEFILE=~/tmp/txrmanpage
TXRHEADERSFILE=~/tmp/txrheaders
TXRFZFHISTORY=~/tmp/txrfzfhistory
if ! [ -r $TXRMANPAGEFILE ]; then
    man txr > $TXRMANPAGEFILE
fi
if ! [ -r $TXRHEADERSFILE ]; then
    grep '^\s\+[0-9]\+\(\.[0-9]\+\)\+' $TXRMANPAGEFILE > $TXRHEADERSFILE
fi

PREVIEWCMD="grep -F -A 10000 {} ${TXRMANPAGEFILE}|sed -n '1,/^\s\+[0-9]\+\.[0-9]\+\s/p'"
grep '^\s\+[0-9]\+\.[0-9]\+\s' ${TXRHEADERSFILE}|\
    fzf --height=100% --no-sort --preview-window=right:wrap --preview="${PREVIEWCMD}" \
	--history=$TXRFZFHISTORY \
	--bind "alt-v:execute(${PREVIEWCMD}|less)" \
	--bind "enter:execute(${PREVIEWCMD}|less)" \
	--bind "alt-h:select-all+reload(grep '^\s\+[0-9]\+\.[0-9]\+\.[0-9]\+' {+f} >/dev/null && grep '^\s\+[0-9]\+\.[0-9]\+\s' ${TXRHEADERSFILE} || cat ${TXRHEADERSFILE})" \
	--header "RET=view,M-h=toggle subheaders,${FZF_HEADER}"

