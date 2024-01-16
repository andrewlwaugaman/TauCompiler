repos=(
  "vm"
  "tau"
  "rdgen"
  "testerator"
)

for repo in "${repos[@]}"
do
  echo "Pulling " "$repo"
  (cd "$repo"; git pull)
done
