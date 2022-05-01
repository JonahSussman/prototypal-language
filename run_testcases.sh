TESTS="3_1_compare.lot 3_1_convert.lot 3_3_things.lot"

for test in $TESTS; do
  echo "---" $test "---"
  ./lot "examples/$test"
done