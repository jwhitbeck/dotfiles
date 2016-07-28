# bash extras

# completion for leiningen

_lein(){
    if [ $COMP_CWORD == 1 ]; then
        curw=${COMP_WORDS[COMP_CWORD]}
        COMPREPLY=($(compgen -W "check checkouts classpath clean compile deploy deps do help install jar javac midje new plugin pom repl retest run search show test trampoline uberjar update upgrade version with-profile lobos" -- $curw))
    fi
    return 0
}

complete -F _lein -o default lein