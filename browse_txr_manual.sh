#!/bin/sh

# Browse txr manpage using fzf
# Keybindings are shown in the header line.
# Options are inherited from $FZF_DEFAULT_OPTS, but --header is overwritten unless you have saved it in $FZF_HEADER

# Locations of cache files
TXRMANPAGEFILE=~/tmp/txrmanpage
TXRHEADERSFILE=~/tmp/txrheaders
TXRFZFHISTORY=~/tmp/txrfzfhistory
if ! [ -r $TXRMANPAGEFILE ]; then
    man txr > $TXRMANPAGEFILE
fi
if ! [ -r $TXRHEADERSFILE ]; then
    grep '^\s\+[0-9]\{1,2\}\(\.[0-9]\{1,3\}\)\+\s[A-Z]' $TXRMANPAGEFILE > $TXRHEADERSFILE
fi
# Change following variable if you make any changes to keybindings (i.e. the --bind options below)
FZF_HEADER2="RET/M-v=view section,M-V=view summary,M-h=toggle subheaders"

if ! [ -z $FZF_HEADER ]; then
    FZF_HEADER3="${FZF_HEADER}
${FZF_HEADER2}"
else
    FZF_HEADER3="${FZF_HEADER2}"
fi

PREVIEWCMD1="grep -F -A 10000 {} ${TXRMANPAGEFILE}|sed -n '1,/^\s\+[0-9]\{1,2\}\.[0-9]\{1,3\}\s[A-Z]/p'"
PREVIEWCMD2="grep -F -A 10000 {} ${TXRMANPAGEFILE}|sed -n '1n;/^\s\+[0-9]\{1,2\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}/h;/Description:/{n;n;N;H;x;p};/^\s\+[0-9]\{1,2\}\.[0-9]\{1,3\}\s[A-Z]/q'"

grep '^\s\+[0-9]\+\.[0-9]\+\s' ${TXRHEADERSFILE}|\
    fzf --height=100% --no-sort --preview-window=right:wrap --preview="${PREVIEWCMD1}" \
	--history=$TXRFZFHISTORY \
	--bind "alt-v:execute(${PREVIEWCMD1}|less)" \
	--bind "alt-V:execute(${PREVIEWCMD2}|less)" \
	--bind "enter:execute(${PREVIEWCMD1}|less)" \
	--bind "alt-h:select-all+reload(grep '^\s\+[0-9]\+\.[0-9]\+\.[0-9]\+' {+f} >/dev/null && grep '^\s\+[0-9]\+\.[0-9]\+\s' ${TXRHEADERSFILE} || cat ${TXRHEADERSFILE})" \
	--header "${FZF_HEADER3}"
