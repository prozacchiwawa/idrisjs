export IDRISJS_OPTIM=$2
export IDRISJS_DEBUG=$3
idris -p js -p effects -p contrib -p containers --codegen javascript  $1 -o ${1%%.idr}.html
