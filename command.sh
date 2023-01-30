alias stack="stack-2.9.1"
alias build="stack build"
alias run="stack exec"

function hs(){
    build
    echo "--------------------------$1------------------------------------"
    run $1
}
bin=./bin
# browser
function bs(){
    hs $1
    if [ ! -d "$bin" ]; then
        echo "$bin does not exist."
        mkdir $bin
        echo "create $bin"
    fi
    run $1 > $bin/output.html
    echo "--------------------------$1------------------------------------"
    echo "the output is in $bin/output.html"
}