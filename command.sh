alias stack="stack-2.9.1"
alias build="stack build"
alias run="stack exec"

function hs(){
    build
    echo "--------------------------$1------------------------------------"
    run $1
}

# browser
function bs(){
    hs $1
    run $1 > ./bin/output.html
}