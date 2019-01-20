for f in tests/*.idr
do
  echo compiling $f
  rm ${f%%.idr}.ibc 2>/dev/null
  idris -p js -p contrib  --codegen javascript $f  -o ${f%%.idr}.js
done
