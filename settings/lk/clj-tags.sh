find $1 \! -name '.*' -name '*.clj' | xargs etags --regex='/[ \t\(]*def[a-z]* \([a-z-!]+\)/\1/' --regex='/[ \t\(]*ns \([a-z.]+\)/\1/'
